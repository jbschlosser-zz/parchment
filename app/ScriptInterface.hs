module ScriptInterface
    ( scriptInterface
    , opaqueToSessFunc
    , chainM
    ) where

import qualified Data.Map as M
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
    , ((varNamespace, "composite"), CustFunc compositeAction)
    , ((varNamespace, "do-nothing"), toOpaque (id :: Sess -> Sess))
    , ((varNamespace, "string-repr"), CustFunc stringRepr)
    , ((varNamespace, "make-hash"), CustFunc makeHash)
    , ((varNamespace, "hash-contains?"), CustFunc hashContains)
    , ((varNamespace, "hash-get"), CustFunc hashGet)
    , ((varNamespace, "hash-set"), CustFunc hashSet)]

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
addKeyWrapper :: [LispVal] -> IOThrowsError LispVal
addKeyWrapper [(Char c)] = liftThrows . Right . toOpaque $ addKey c
addKeyWrapper _ = liftThrows . Left . Default $ "Usage: (add-key <char>)"

scrollHistoryWrapper :: [LispVal] -> IOThrowsError LispVal
scrollHistoryWrapper [(Number n)] = liftThrows . Right . toOpaque . scrollHistory $
    fromIntegral n
scrollHistoryWrapper _ = liftThrows . Left . Default $ "Usage: (scroll-history <num>)"

scrollLinesWrapper :: [LispVal] -> IOThrowsError LispVal
scrollLinesWrapper [(Number n)] = liftThrows . Right . toOpaque . scrollLines $
    fromIntegral n
scrollLinesWrapper _ = liftThrows . Left . Default $ "Usage: (scroll-lines <num>)"

writeScrollbackWrapper :: [LispVal] -> IOThrowsError LispVal
writeScrollbackWrapper [(String s)] = liftThrows . Right . toOpaque . writeScrollback $
    formatStr s
writeScrollbackWrapper _ = liftThrows . Left . Default $ "Usage: (print str)"

writeScrollbackLnWrapper :: [LispVal] -> IOThrowsError LispVal
writeScrollbackLnWrapper [(String s)] = liftThrows . Right . toOpaque . writeScrollbackLn $
    formatStr s
writeScrollbackLnWrapper _ = liftThrows . Left . Default $ "Usage: (println str)"

sendToServerWrapper :: [LispVal] -> IOThrowsError LispVal
sendToServerWrapper [(String s)] = liftThrows . Right . toOpaque $ sendToServer s
sendToServerWrapper _ = liftThrows . Left . Default $ "Usage: (send <string>)"

compositeAction :: [LispVal] -> IOThrowsError LispVal
compositeAction [(List l)] = liftThrows . Right . toOpaque . chainM $ map opaqueToSessFunc l
compositeAction _ = liftThrows . Left . Default $ "Usage: (composite <list>)"

stringRepr :: [LispVal] -> IOThrowsError LispVal
stringRepr [v] = liftThrows . Right . String $ show v
stringRepr _ = liftThrows . Left . Default $ "Usage: (string-repr <val>)"

makeHash :: [LispVal] -> IOThrowsError LispVal
makeHash [] = liftThrows . Right $ HashTable M.empty
makeHash _ = liftThrows . Left . Default $ "Usage: (make-hash)"

hashContains :: [LispVal] -> IOThrowsError LispVal
hashContains [(HashTable ht), key] = liftThrows . Right . Bool $ M.member key ht
hashContains _ = liftThrows . Left . Default $ "Usage: (hash-contains? ht key)"

hashGet :: [LispVal] -> IOThrowsError LispVal
hashGet [(HashTable ht), key] =
    case M.lookup key ht of
         Just val -> liftThrows . Right $ val
         Nothing -> liftThrows . Right . Bool $ False
hashGet _ = liftThrows . Left . Default $ "Usage: (hash-get ht key)"

hashSet :: [LispVal] -> IOThrowsError LispVal
hashSet [(HashTable ht), key, val] = liftThrows . Right . HashTable $
    M.insert key val ht
hashSet _ = liftThrows . Left . Default $ "Usage: (hash-set ht key val)"
