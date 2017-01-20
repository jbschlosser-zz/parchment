module ScriptInterface
    ( scriptInterface
    ) where

import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Variables
import Parchment.Fchar
import Parchment.Session

scriptInterface :: IO Env
scriptInterface = r5rsEnv >>= flip extendEnv
    [ ((varNamespace, "send"), PrimitiveFunc sendToServerWrapper)
    , ((varNamespace, "del-key"), toOpaque delKey)
    , ((varNamespace, "clear-input-line"), toOpaque clearInputLine)
    , ((varNamespace, "page-up"), toOpaque pageUp)
    , ((varNamespace, "page-down"), toOpaque pageDown)
    , ((varNamespace, "next-history"), toOpaque nextHistory)
    , ((varNamespace, "history-older"), toOpaque historyOlder)
    , ((varNamespace, "history-newer"), toOpaque historyNewer)
    , ((varNamespace, "scroll-history"), PrimitiveFunc scrollHistoryWrapper)
    , ((varNamespace, "scroll-lines"), PrimitiveFunc scrollLinesWrapper)
    , ((varNamespace, "print"), PrimitiveFunc writeScrollbackWrapper)
    , ((varNamespace, "println"), PrimitiveFunc writeScrollbackLnWrapper)
    , ((varNamespace, "add-key"), PrimitiveFunc addKeyWrapper)]

-- === BINDING WRAPPERS. ===
-- TODO: Find a less repetitive way to do this.
addKeyWrapper :: [LispVal] -> ThrowsError LispVal
addKeyWrapper xs | length xs == 1 =
    case head xs of
         Char c -> Right $ toOpaque $ addKey c
         _ -> Left $ Default "Expected a character"
addKeyWrapper _ = Left $ Default "Expected a character"

scrollHistoryWrapper :: [LispVal] -> ThrowsError LispVal
scrollHistoryWrapper xs | length xs == 1 =
    case head xs of
         Number i -> Right $ toOpaque $ scrollHistory $ fromIntegral i
         _ -> Left $ Default "Expected a number"
scrollHistoryWrapper _ = Left $ Default "Expected a number"

scrollLinesWrapper :: [LispVal] -> ThrowsError LispVal
scrollLinesWrapper xs | length xs == 1 =
    case head xs of
         Number i -> Right $ toOpaque $ scrollLines $ fromIntegral i
         _ -> Left $ Default "Expected a number"
scrollLinesWrapper _ = Left $ Default "Expected a number"

writeScrollbackWrapper :: [LispVal] -> ThrowsError LispVal
writeScrollbackWrapper xs | length xs == 1 =
    case (head xs) of
         String s -> Right $ toOpaque $ writeScrollback $ formatStr s
         _ -> Left $ Default "Expected a string"
writeScrollbackWrapper _ = Left $ Default "Expected a string"

writeScrollbackLnWrapper :: [LispVal] -> ThrowsError LispVal
writeScrollbackLnWrapper xs | length xs == 1 =
    case (head xs) of
         String s -> Right $ toOpaque $ writeScrollbackLn $ formatStr s
         _ -> Left $ Default "Expected a string"
writeScrollbackLnWrapper _ = Left $ Default "Expected a string"

sendToServerWrapper :: [LispVal] -> ThrowsError LispVal
sendToServerWrapper xs | length xs == 1 =
    case (head xs) of
         String s -> Right $ toOpaque $ sendToServer s
         _ -> Left $ Default "Expected a string"
sendToServerWrapper _ = Left $ Default "Expected a string"
