module Parchment.EscSeq
    ( parseEscSeq
    , escSeqPartParser
    , updateCharAttr
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import qualified Graphics.Vty as V
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

-- Updates a char attribute based on an esc sequence.
updateCharAttr :: V.Attr -> BS.ByteString -> V.Attr
updateCharAttr attr seq =
    case parse escSeqPartParser "error" (BSC.unpack seq) of
         Right [] -> V.defAttr -- handle \ESC[m case
         Right parts -> foldr (flip (.)) id (map escSeqPartTransform parts) attr
         Left _ -> attr

escSeqPartTransform :: String -> V.Attr -> V.Attr
escSeqPartTransform s =
    case s of
        "0" -> const V.defAttr
        "1" -> flip V.withStyle V.bold
        "30" -> flip V.withForeColor V.black
        "31" -> flip V.withForeColor V.red
        "32" -> flip V.withForeColor V.green
        "33" -> flip V.withForeColor V.yellow
        "34" -> flip V.withForeColor V.blue
        "35" -> flip V.withForeColor V.magenta
        "36" -> flip V.withForeColor V.cyan
        "37" -> flip V.withForeColor V.white
        "39" -> \a -> V.Attr
            { V.attrStyle = (V.attrStyle a)
            , V.attrForeColor = V.Default
            , V.attrBackColor = (V.attrBackColor a)
            }
        "40" -> flip V.withBackColor V.black
        "41" -> flip V.withBackColor V.red
        "42" -> flip V.withBackColor V.green
        "43" -> flip V.withBackColor V.yellow
        "44" -> flip V.withBackColor V.blue
        "45" -> flip V.withBackColor V.magenta
        "46" -> flip V.withBackColor V.cyan
        "47" -> flip V.withBackColor V.white
        "49" -> \a -> V.Attr
            { V.attrStyle = (V.attrStyle a)
            , V.attrForeColor = (V.attrForeColor a)
            , V.attrBackColor = V.Default
            }
        _ -> id
