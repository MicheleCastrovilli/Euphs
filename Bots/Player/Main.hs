module Main where

import Euphs.Bot
import Euphs.Events
import Euphs.Commands
import Euphs.Types
import Euphs.Options

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import Data.Maybe
import Data.Function

import System.Process
import Safe

data ClientState = ClientState {
    playHandle :: TMVar ProcessHandle
}

main :: IO ()
main = do
    let myOptions = defaults {
          roomList = "music"
        , botNick = ""
        }
    opts <- getOpts myOptions options
    cbot <- clientBot
    botWithOpts cbot opts

clientBot :: IO BotFunctions
clientBot = do
    player <- atomically newEmptyTMVar
    let stat = ClientState player
    return emptyBot {
        eventsHook = myFun stat
    ,   dcHook = Just (myClean stat)
    }

myFun stat (SendEvent m) = do
    case findPlay m of
            Nothing -> return ()
            Just l -> liftIO $ playStuff stat l

myFun stat sn@SnapshotEvent{} = do
    let mes = reverse $ sortBy (compare `on` timeRecieved) $ messages sn
    case headMay (mapMaybe findPlay mes) of
        Nothing -> return ()
        Just m -> liftIO $ playStuff stat m

myFun _ _ = return ()

findPlay :: MessageData -> Maybe String
findPlay m = do
    let play = filter (isInfixOf "watch?v=")
               $ drop 1 $ dropWhile (/= "!play") $ words
               $ contentMsg m
    req <- headMay play
    return req

playStuff :: ClientState -> String -> IO ()
playStuff stat l = do
    let p = playHandle stat
    hl <- spawnProcess "youtube-viewer" ["-n","--no-interactive", l]
    tryKillVideo p
    atomically $ putTMVar p hl

tryKillVideo :: TMVar ProcessHandle -> IO ()
tryKillVideo p = do
     m <- atomically $ tryTakeTMVar p
     case m of
        Nothing -> return ()
        Just proc -> terminateProcess proc

myClean :: ClientState -> IO ()
myClean s = do
    let p = playHandle s
    tryKillVideo p
