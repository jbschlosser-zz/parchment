module ScriptInterface
    ( scriptInterface
    , opaqueToSessFunc
    , chainM
    ) where

import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Variables
import Parchment.Fchar
import Parchment.Session

scriptInterface :: IO Env
scriptInterface = r5rsEnv >>= flip extendEnv
    [ ((varNamespace, "send"), CustFunc sendToServerWrapper)
    , ((varNamespace, "del-key"), toOpaque delKey)
    , ((varNamespace, "clear-input-line"), toOpaque clearInputLine)
    , ((varNamespace, "page-up"), toOpaque pageUp)
    , ((varNamespace, "page-down"), toOpaque pageDown)
    , ((varNamespace, "next-history"), toOpaque nextHistory)
    , ((varNamespace, "history-older"), toOpaque historyOlder)
    , ((varNamespace, "history-newer"), toOpaque historyNewer)
    , ((varNamespace, "scroll-history"), CustFunc scrollHistoryWrapper)
    , ((varNamespace, "scroll-lines"), CustFunc scrollLinesWrapper)
    , ((varNamespace, "print"), CustFunc writeScrollbackWrapper)
    , ((varNamespace, "println"), CustFunc writeScrollbackLnWrapper)
    , ((varNamespace, "add-key"), CustFunc addKeyWrapper)
    , ((varNamespace, "action"), CustFunc compositeAction)]

-- Convert an opaque lisp value to a session-transforming action.
opaqueToSessFunc :: LispVal -> Sess -> IO Sess
opaqueToSessFunc lv = case fromOpaque lv :: ThrowsError (Sess -> IO Sess) of
                           Right f -> f
                           Left _ -> case fromOpaque lv :: ThrowsError (Sess -> Sess) of
                                          Right f -> return . f
                                          Left err -> return . (writeScrollbackLn
                                              (colorize V.red $ "Error: " ++ (show err)))

-- Chain a list of monad functions, with each result feeding into the next.
chainM :: (Monad m) => [a -> m a] -> a -> m a
chainM [] a = return a
chainM (x:xs) a = x a >>= chainM xs

-- === BINDING WRAPPERS. ===
-- TODO: Find a less repetitive way to do this.
addKeyWrapper :: [LispVal] -> IOThrowsError LispVal
addKeyWrapper xs | length xs == 1 =
    case head xs of
         Char c -> liftThrows $ Right $ toOpaque $ addKey c
         _ -> liftThrows $ Left $ Default "Expected a character"
addKeyWrapper _ = liftThrows $ Left $ Default "Expected a character"

scrollHistoryWrapper :: [LispVal] -> IOThrowsError LispVal
scrollHistoryWrapper xs | length xs == 1 =
    case head xs of
         Number i -> liftThrows $ Right $ toOpaque $ scrollHistory $ fromIntegral i
         _ -> liftThrows $ Left $ Default "Expected a number"
scrollHistoryWrapper _ = liftThrows $ Left $ Default "Expected a number"

scrollLinesWrapper :: [LispVal] -> IOThrowsError LispVal
scrollLinesWrapper xs | length xs == 1 =
    case head xs of
         Number i -> liftThrows $ Right $ toOpaque $ scrollLines $ fromIntegral i
         _ -> liftThrows $ Left $ Default "Expected a number"
scrollLinesWrapper _ = liftThrows $ Left $ Default "Expected a number"

writeScrollbackWrapper :: [LispVal] -> IOThrowsError LispVal
writeScrollbackWrapper xs | length xs == 1 =
    case (head xs) of
         String s -> liftThrows $ Right $ toOpaque $ writeScrollback $ formatStr s
         _ -> liftThrows $ Left $ Default "Expected a string"
writeScrollbackWrapper _ = liftThrows $ Left $ Default "Expected a string"

writeScrollbackLnWrapper :: [LispVal] -> IOThrowsError LispVal
writeScrollbackLnWrapper xs | length xs == 1 =
    case (head xs) of
         String s -> liftThrows $ Right $ toOpaque $ writeScrollbackLn $ formatStr s
         _ -> liftThrows $ Left $ Default "Expected a string"
writeScrollbackLnWrapper _ = liftThrows $ Left $ Default "Expected a string"

sendToServerWrapper :: [LispVal] -> IOThrowsError LispVal
sendToServerWrapper xs | length xs == 1 =
    case (head xs) of
         String s -> liftThrows $ Right $ toOpaque $ sendToServer s
         x -> liftThrows $ Left $ Default ("Expected a string, got: " ++ show x)
sendToServerWrapper _ = liftThrows $ Left $ Default "Usage: send <string>"

compositeAction :: [LispVal] -> IOThrowsError LispVal
compositeAction xs | length xs == 1 =
    case head xs of
         List l -> liftThrows $ Right $ toOpaque $ chainM $ map opaqueToSessFunc l
         x -> liftThrows $ Left $ Default ("Expected a list, got: " ++ show x)
compositeAction _ = liftThrows $ Left $ Default "Usage: action <list>"
