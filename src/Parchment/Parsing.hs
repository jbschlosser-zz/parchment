module Parchment.Parsing
    ( parseTelnet
    , parseEscSeq
    , ParseState(..)
    ) where

import Data.Word (Word8)
import qualified Data.ByteString as BS

data ParseState a = NotInProgress | InProgress a | Success a | Error a
    deriving (Show)

-- TELNET PARSING --
telnet_SE = 240 -- End of sub-negotiation parameters.
telnet_NOP = 241 -- No operation.                                        
telnet_DATA_MARK = 242 -- The data stream portion of a Synch. This should always be accompanied
                 -- by a TCP Urgent notification.
telnet_BREAK = 243 -- NVT character BRK.                                 
telnet_IP = 244 -- The function IP (interrupt process).                  
telnet_AO = 245 -- The function AO (abort output).                       
telnet_AYT = 246 -- The function AYT (are you there).                    
telnet_EC = 247 -- The function EC (erase character).                    
telnet_EL = 248 -- The function EL (erase line).                         
telnet_GA = 249 -- The GA (go ahead) signal.                             
telnet_SB = 250 -- Indicates that what follows is subnegotiation of the indicated option.
telnet_WILL = 251 -- Indicates the desire to begin performing, or confirmation that you are now
            -- performing, the indicated option.
telnet_WONT = 252 -- Indicates the refusal to perform, or continue performing, the indicated option.
telnet_DO = 253 -- Indicates the request that the other party perform, or
          -- confirmation that you are expecting the other party to perform, the
          -- indicated option.                                       
telnet_DONT = 254 -- Indicates the demand that the other party stop performing,
            -- or confirmation that you are no longer expecting the other party
            -- to perform, the indicated option.                     
telnet_IAC = 255 -- Interpret As Command. Indicates the start of a telnet option negotiation.
telnet_GMCP = 0xC9

twoByteCommands :: [Word8]
twoByteCommands = [telnet_IAC, telnet_NOP, telnet_DATA_MARK, telnet_BREAK, telnet_IP,
                   telnet_AO, telnet_AYT, telnet_EC, telnet_EL, telnet_GA]

threeByteCommands :: [Word8]
threeByteCommands = [telnet_WILL, telnet_WONT, telnet_DO, telnet_DONT]

parseTelnet :: ParseState BS.ByteString -> Word8 -> ParseState BS.ByteString
parseTelnet st b =
    case st of
         NotInProgress ->
             if b == telnet_IAC then
                InProgress $ BS.singleton b
             else
                NotInProgress
         InProgress bs ->
             case BS.length bs of
                  1 -> case b of _
                                  | elem b twoByteCommands -> Success new_bs
                                  | elem b threeByteCommands -> InProgress new_bs
                                  | b == telnet_SB -> InProgress new_bs
                                  | otherwise -> Error new_bs
                  2 -> case prev of _
                                     | elem prev threeByteCommands -> Success new_bs
                                     | prev == telnet_SB -> InProgress new_bs
                                     | otherwise -> Error new_bs
                  _ -> case (prev, b) of _
                                          | prev == telnet_IAC &&
                                                b == telnet_SE -> Success new_bs
                                          | otherwise -> InProgress new_bs
                where
                    new_bs = BS.snoc bs b
                    prev = BS.last bs
         _ -> parseTelnet NotInProgress b 

-- ESC SEQ PARSING --
escSeq_BEGIN = 0x1B
escSeq_END = 0x6D
escSeq_MAX_SIZE = 15

parseEscSeq :: ParseState BS.ByteString -> Word8 -> ParseState BS.ByteString
parseEscSeq st b =
    case st of
         NotInProgress ->
             if b == escSeq_BEGIN then
                InProgress $ BS.singleton b
             else
                NotInProgress
         InProgress bs ->
             let new_bs = BS.snoc bs b in
             if BS.length new_bs > escSeq_MAX_SIZE then
                Error new_bs
             else if b == escSeq_END then
                Success new_bs
             else
                InProgress new_bs
         _ -> parseEscSeq NotInProgress b 

