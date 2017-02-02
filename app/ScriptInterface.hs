module ScriptInterface
    ( scriptInterface
    , opaqueToAction
    , loadConfig
    ) where
import qualified Data.Map as M
import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Variables
import Lens.Micro ((&), (.~))
import Parchment.FString
import Parchment.Session
import System.Environment.XDG.BaseDir

scriptInterface :: IO Env
scriptInterface = r5rsEnv >>= flip extendEnv
    [ ((varNamespace, "del-key"), sessFuncToOpaque delKey)
    , ((varNamespace, "quit"), actionToOpaque $ return . (const Nothing))
    , ((varNamespace, "clear-input-line"), sessFuncToOpaque clearInputLine)
    , ((varNamespace, "page-up"), sessFuncToOpaque pageUp)
    , ((varNamespace, "page-down"), sessFuncToOpaque pageDown)
    , ((varNamespace, "next-history"), sessFuncToOpaque nextHistory)
    , ((varNamespace, "history-older"), sessFuncToOpaque historyOlder)
    , ((varNamespace, "history-newer"), sessFuncToOpaque historyNewer)
    , ((varNamespace, "do-nothing"), sessFuncToOpaque id)
    , ((varNamespace, "reload-config"), ioSessFuncToOpaque loadConfigAction)
    , ((varNamespace, "send"), CustFunc sendToServerWrapper)
    , ((varNamespace, "scroll-history"), CustFunc scrollHistoryWrapper)
    , ((varNamespace, "scroll-lines"), CustFunc scrollLinesWrapper)
    , ((varNamespace, "search-backwards"), CustFunc searchBackwardsWrapper)
    , ((varNamespace, "print"), CustFunc writeBufferWrapper)
    , ((varNamespace, "println"), CustFunc writeBufferLnWrapper)
    , ((varNamespace, "add-key"), CustFunc addKeyWrapper)
    , ((varNamespace, "composite"), CustFunc compositeAction)
    , ((varNamespace, "string-repr"), CustFunc stringRepr)
    , ((varNamespace, "make-hash"), CustFunc makeHash)
    , ((varNamespace, "hash-contains?"), CustFunc hashContains)
    , ((varNamespace, "hash-get"), CustFunc hashGet)
    , ((varNamespace, "hash-set"), CustFunc hashSet)]

-- Loads the config file. Returns the environment and optionally any errors.
loadConfig :: IO (Env, Maybe String)
loadConfig = do
    scmEnv <- scriptInterface
    configPath <- getUserConfigFile "parchment" "config.scm"
    let conf = List [Atom "include", String configPath]
    res <- evalLisp' scmEnv conf
    case res of
         Left err -> return (scmEnv, Just $ show err)
         Right _ -> return (scmEnv, Nothing)

loadConfigAction :: Sess -> IO Sess
loadConfigAction sess = do
    (scmEnv, configErr) <- loadConfig
    return $ case configErr of
        Just err -> writeBufferLn (colorize V.red ("Config error: " ++ err)) $ sess
        Nothing -> sess & scm_env .~ scmEnv

-- Helper functions for converting between list types and Sess actions.
opaqueToAction :: LispVal -> Sess -> IO (Maybe Sess)
opaqueToAction lv =
    case fromOpaque lv :: ThrowsError (Sess -> IO (Maybe Sess)) of
         Right f -> f
         Left err -> return . Just . writeBufferLn (colorize V.red $ "Error: " ++ (show err))

actionToOpaque :: (Sess -> IO (Maybe Sess)) -> LispVal
actionToOpaque = toOpaque

sessFuncToOpaque :: (Sess -> Sess) -> LispVal
sessFuncToOpaque sf = actionToOpaque $ return . Just . sf

ioSessFuncToOpaque :: (Sess -> IO Sess) -> LispVal
ioSessFuncToOpaque sf = actionToOpaque $ \sess -> do
    res <- sf sess
    return . Just $ res

-- Chain a list of monad functions, with each result feeding into the next.
chainMaybeIO :: [a -> IO (Maybe a)] -> a -> IO (Maybe a)
chainMaybeIO [] a = return . Just $ a
chainMaybeIO (x:xs) a = do
    res <- x a
    case res of
         Nothing -> return Nothing
         Just r -> chainMaybeIO xs r

-- === BINDING WRAPPERS. ===
addKeyWrapper :: [LispVal] -> IOThrowsError LispVal
addKeyWrapper [(Char c)] = liftThrows . Right . sessFuncToOpaque $ addKey c
addKeyWrapper _ = liftThrows . Left . Default $ "Usage: (add-key <char>)"

scrollHistoryWrapper :: [LispVal] -> IOThrowsError LispVal
scrollHistoryWrapper [(Number n)] = liftThrows . Right . sessFuncToOpaque . scrollHistory $
    fromIntegral n
scrollHistoryWrapper _ = liftThrows . Left . Default $ "Usage: (scroll-history <num>)"

scrollLinesWrapper :: [LispVal] -> IOThrowsError LispVal
scrollLinesWrapper [(Number n)] = liftThrows . Right . sessFuncToOpaque . scrollLines $
    fromIntegral n
scrollLinesWrapper _ = liftThrows . Left . Default $ "Usage: (scroll-lines <num>)"

searchBackwardsWrapper :: [LispVal] -> IOThrowsError LispVal
searchBackwardsWrapper [(String s)] =
    liftThrows . Right . sessFuncToOpaque $ searchBackwards s
searchBackwardsWrapper _ = liftThrows . Left . Default $ "Usage: (search-backwards <string>)"

writeBufferWrapper :: [LispVal] -> IOThrowsError LispVal
writeBufferWrapper [(String s)] = liftThrows . Right . sessFuncToOpaque . writeBuffer $
    formatStr s
writeBufferWrapper _ = liftThrows . Left . Default $ "Usage: (print str)"

writeBufferLnWrapper :: [LispVal] -> IOThrowsError LispVal
writeBufferLnWrapper [(String s)] = liftThrows . Right . sessFuncToOpaque . writeBufferLn $
    formatStr s
writeBufferLnWrapper _ = liftThrows . Left . Default $ "Usage: (println str)"

sendToServerWrapper :: [LispVal] -> IOThrowsError LispVal
sendToServerWrapper [(String s)] = liftThrows . Right . ioSessFuncToOpaque $ sendToServer s
sendToServerWrapper _ = liftThrows . Left . Default $ "Usage: (send <string>)"

compositeAction :: [LispVal] -> IOThrowsError LispVal
compositeAction [(List l)] = liftThrows . Right . actionToOpaque . chainMaybeIO $
    map opaqueToAction l
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
