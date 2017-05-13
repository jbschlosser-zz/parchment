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
twoByteCommands = [tIAC, tNOP, tDATA_MARK, tBREAK, tIP, tAO, tAYT, tEC, tEL, tGA]

threeByteCommands :: [Word8]
threeByteCommands = [tWILL, tWONT, tDO, tDONT]

parseTelnet :: ParseState BS.ByteString -> Word8 -> ParseState BS.ByteString
parseTelnet NotInProgress b
    | b == tIAC = InProgress $ BS.singleton b
    | otherwise = NotInProgress
parseTelnet (InProgress bs) b
    | len == 1 && b `elem` twoByteCommands = Success . BS.reverse $ new_bs
    | len == 1 && b `elem` threeByteCommands = InProgress new_bs
    | len == 1 && b == tSB = InProgress new_bs
    | len == 1 = Error new_bs
    | len == 2 && prev `elem` threeByteCommands = Success . BS.reverse $ new_bs
    | len == 2 && prev == tSB = InProgress new_bs
    | len == 2 = Error new_bs
    | prev == tIAC && b == tSE = Success . BS.reverse $ new_bs
    | otherwise = InProgress new_bs
    where len = BS.length bs
          new_bs = BS.cons b bs
          prev = BS.head bs
parseTelnet _ b = parseTelnet NotInProgress b
