{-# LANGUAGE TemplateHaskell #-}

module Parchment.Fchar
    ( Fchar(..)
    , applyAttr
    , colorize
    , defaultStyle
    , formatStr
    ) where

import qualified Graphics.Vty as V
import Text.Parsec hiding (Error, getInput)

data Fchar = Fchar {_ch :: Char, _attr :: V.Attr}
instance Show Fchar where
    show Fchar {_ch = ch, _attr = _} = show ch

applyAttr :: V.Attr -> String -> [Fchar]
applyAttr a str = map (\c -> Fchar {_ch = c, _attr = a}) str 

colorize :: V.Color -> String -> [Fchar]
colorize c = applyAttr (V.withForeColor V.defAttr c)

defaultStyle :: String -> [Fchar]
defaultStyle = applyAttr V.defAttr

formatStr :: String -> [Fchar]
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
formatStrParser :: Parsec String V.Attr [[Fchar]]
formatStrParser = many $ do
    part <- formatPartParser <|> many1 (noneOf "{")
    state <- getState
    return $ applyAttr state part
