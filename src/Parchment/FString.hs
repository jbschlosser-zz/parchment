{-# LANGUAGE TemplateHaskell #-}

module Parchment.FString
    ( FChar(..)
    , FString
    , applyAttr
    , colorize
    , withStyle
    , defaultStyle
    , formatStr
    , removeFormatting
    ) where

import qualified Graphics.Vty as V
import Text.Parsec hiding (Error, getInput)

data FChar = FChar {_ch :: Char, _attr :: V.Attr}
instance Show FChar where
    show FChar {_ch = ch, _attr = _} = [ch]

type FString = [FChar]

applyAttr :: V.Attr -> String -> FString
applyAttr a str = map (\c -> FChar {_ch = c, _attr = a}) str 

colorize :: V.Color -> String -> FString
colorize c = applyAttr (V.withForeColor V.defAttr c)

withStyle :: V.Style -> FChar -> FChar
withStyle style FChar {_ch = ch, _attr = a} = FChar {_ch = ch, _attr =
    V.Attr (V.SetTo style) (V.attrForeColor a) (V.attrBackColor a) }

defaultStyle :: String -> FString
defaultStyle = applyAttr V.defAttr

removeFormatting :: FString -> String
removeFormatting = map _ch

formatStr :: String -> FString
formatStr s = case runParser formatStrParser V.defAttr "source" s of
                   -- TODO: Maybe better error handling?
                   Left _ -> defaultStyle $ s
                   Right fc -> foldr (++) [] fc

-- Matches a format part.
formatPartParser :: Parsec String V.Attr String
formatPartParser = do
    char '{'
    code <- oneOf "xbrgyumcwH{"
    attr <- case code of
                   'x' -> return V.defAttr
                   'b' -> return $ V.withForeColor V.defAttr V.black
                   'r' -> return $ V.withForeColor V.defAttr V.red
                   'g' -> return $ V.withForeColor V.defAttr V.green
                   'y' -> return $ V.withForeColor V.defAttr V.yellow
                   'u' -> return $ V.withForeColor V.defAttr V.blue
                   'm' -> return $ V.withForeColor V.defAttr V.magenta
                   'c' -> return $ V.withForeColor V.defAttr V.cyan
                   'w' -> return $ V.withForeColor V.defAttr V.white
                   _ -> getState
    putState attr
    let chars = case code of
                     '{' -> "{"
                     _ -> ""
    return chars

-- Matches a whole string with optional format parts.
formatStrParser :: Parsec String V.Attr [FString]
formatStrParser = many $ do
    part <- formatPartParser <|> many1 (noneOf "{")
    state <- getState
    return $ applyAttr state part
