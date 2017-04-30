module Parchment.Telnet
    ( parseTelnet
    , tSE
    , tNOP
    , tDATA_MARK
    , tBREAK
    , tIP
    , tAO
    , tAYT
    , tEC
    , tEL
    , tGA
    , tSB
    , tWILL
    , tWONT
    , tDO
    , tDONT
    , tIAC
    , tGMCP
    , tTELETYPE
    , tSEND
    , tIS
    , tNAWS
    , tMXP
    , tMCCP2
    , tMSSP
    , tMSDP
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Parchment.ParseState

-- TELNET PARSING --
tSE = 240 -- End of sub-negotiation parameters.
tNOP = 241 -- No operation.                                        
tDATA_MARK = 242 -- The data stream portion of a Synch. This should always be accompanied
                 -- by a TCP Urgent notification.
tBREAK = 243 -- NVT character BRK.                                 
tIP = 244 -- The function IP (interrupt process).                  
tAO = 245 -- The function AO (abort output).                       
tAYT = 246 -- The function AYT (are you there).                    
tEC = 247 -- The function EC (erase character).                    
tEL = 248 -- The function EL (erase line).                         
tGA = 249 -- The GA (go ahead) signal.                             
tSB = 250 -- Indicates that what follows is subnegotiation of the indicated option.
tWILL = 251 -- Indicates the desire to begin performing, or confirmation that you are now
            -- performing, the indicated option.
tWONT = 252 -- Indicates the refusal to perform, or continue performing, the indicated option.
tDO = 253 -- Indicates the request that the other party perform, or
          -- confirmation that you are expecting the other party to perform, the
          -- indicated option.                                       
tDONT = 254 -- Indicates the demand that the other party stop performing,
            -- or confirmation that you are no longer expecting the other party
            -- to perform, the indicated option.                     
tIAC = 255 -- Interpret As Command. Indicates the start of a telnet option negotiation.
tGMCP = 201 :: Word8
tTELETYPE = 24 :: Word8
tSEND = 1 :: Word8
tIS = 0 :: Word8
tNAWS = 31 :: Word8
tMXP = 91 :: Word8
tMCCP2 = 86 :: Word8
tMSSP = 70 :: Word8
tMSDP = 69 :: Word8

twoByteCommands :: [Word8]
twoByteCommands = [tIAC, tNOP, tDATA_MARK, tBREAK, tIP,
                   tAO, tAYT, tEC, tEL, tGA]

threeByteCommands :: [Word8]
threeByteCommands = [tWILL, tWONT, tDO, tDONT]

parseTelnet :: ParseState BS.ByteString -> Word8 -> ParseState BS.ByteString
parseTelnet st b =
    case st of
         NotInProgress ->
             if b == tIAC then
                InProgress $ BS.singleton b
             else
                NotInProgress
         InProgress bs ->
             case BS.length bs of
                  1 -> case b of _
                                  | elem b twoByteCommands -> Success new_bs
                                  | elem b threeByteCommands -> InProgress new_bs
                                  | b == tSB -> InProgress new_bs
                                  | otherwise -> Error new_bs
                  2 -> case prev of _
                                     | elem prev threeByteCommands -> Success new_bs
                                     | prev == tSB -> InProgress new_bs
                                     | otherwise -> Error new_bs
                  _ -> case (prev, b) of _
                                          | prev == tIAC &&
                                                b == tSE -> Success new_bs
                                          | otherwise -> InProgress new_bs
                where
                    new_bs = BS.snoc bs b
                    prev = BS.last bs
         _ -> parseTelnet NotInProgress b 
