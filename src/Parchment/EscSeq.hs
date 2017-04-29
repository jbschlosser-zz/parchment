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

-- Returns the internal parts of the esc sequence.
escSeqPartParser :: Parsec String () [String]
escSeqPartParser = string "\ESC[" *> sepBy (many1 digit) (char ';') <* char 'm'
