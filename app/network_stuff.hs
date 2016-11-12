module Main where

import qualified Data.ByteString as B
import Brick
import Control.Exception
import Control.Monad (unless)
import Lib
import Network.Socket hiding (send, recv, sendTo, recvFrom)
import Network.Socket.ByteString
import System.Environment

-- HostName, Port number
openConnection :: HostName -> String -> IO Socket
openConnection hostname port = do
    let hints = defaultHints { addrFamily = AF_INET }
    addrinfos <- getAddrInfo (Just hints) (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    return sock 

ui :: Widget ()
ui = str "Hello world!"

main :: IO ()
main = withSocketsDo $ bracket init close talk
    where init = do
            args <- getArgs
            sock <- openConnection (args !! 0) (args !! 1)
            return sock
          talk s = do
            msg <- recv s 4096
            putStrLn $ "< " ++ (show (B.length msg))
            msg <- recv s 4096
            putStrLn $ "< " ++ (show (B.length msg))
            simpleMain ui
