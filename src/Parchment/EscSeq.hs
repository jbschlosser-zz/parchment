module Parchment.EscSeq
    ( parseEscSeq
    , escSeqPartParser
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Text.Parsec hiding (Error)
import Parchment.ParseState

escSeq_BEGIN = 0x1B
escSeq_END = 0x6D
escSeq_MAX_SIZE = 15

parseEscSeq :: ParseState BS.ByteString -> Word8 -> ParseState BS.ByteString
parseEscSeq NotInProgress b
    | b == escSeq_BEGIN = InProgress $ BS.singleton b
    | otherwise = NotInProgress
parseEscSeq (InProgress bs) b
    | BS.length new_bs > escSeq_MAX_SIZE = Error new_bs
    | b == escSeq_END = Success . BS.reverse $ new_bs
    | otherwise = InProgress new_bs
    where new_bs = BS.cons b bs
parseEscSeq _ b = parseEscSeq NotInProgress b

-- Returns the internal parts of the esc sequence.
escSeqPartParser :: Parsec String () [String]
escSeqPartParser = string "\ESC[" *> sepBy (many1 digit) (char ';') <* char 'm'
