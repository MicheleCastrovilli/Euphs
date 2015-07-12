{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import           Euphoria.Bot
import           Euphoria.Events
import           Euphoria.Commands
import           Euphoria.Types
import           System.Environment
import           Control.Concurrent
import           Control.Monad               (when)
import           System.Process
import           YTBot

main :: IO ()
main = do
       args <- getArgs
       if length args < 2 then
        putStrLn $ "Usage: ./EuPhBot <function> <function param>\n"                 ++
                   "Current functions include : \n"                                 ++
                   "E - <room argument> Starts HeliumDJBot in the room specified\n" ++
                   "C - <room argument> Starts  CounterBot in the room specified\n" ++
                   "F - <room argument> Starts  FortuneBot in the room specified\n"
       else if head args == "E"  then 
            do
            ytFun <- getYtFun "AIzaSyA0x4DFVPaFr8glEQvd5nylwThPrDUD4Yc"
            euphoriaBot "♪|HeliumDJBot" (args !! 1) ytFun
        else if head args == "C" then
            do
            a <- newMVar True
            b <- newMVar 0
            euphoriaBot "CounterBot" (args !! 1) $ countFunction $ CountState a b
        else when (head args == "F") $ euphoriaBot "FortuneBot" (args !! 1) fortuneFunction

fortuneFunction :: BotFunction
fortuneFunction botState (SendEvent message)
  = when (contentMsg message == "!fortune") $ 
      do
      a <- readProcess "fortune" ["-s"] []
      putStrLn a
      sendPacket botState $ Send a $ msgID message
      return ()

fortuneFunction _ _ = return ()

data CountState = CountState (MVar Bool) (MVar Int)

countFunction :: CountState -> BotFunction
countFunction cs@(CountState up num) botState (SendEvent message)
   =  case words (contentMsg message) of 
      "!upCount" : _ ->
        do
        prevUp <- takeMVar up
        putMVar up True
        sendPacket botState (Send (if prevUp then "It was already up!" else "Set to up") $ msgID message)
      "!downCount" : _ ->
        do 
        prevUp <- takeMVar up
        putMVar up False
        sendPacket botState (Send (if prevUp then "Set to down" else "It was already down!") $ msgID message)
      "!count" : _ ->
        do
        prevNum <- takeMVar num
        prevUp  <- takeMVar up
        threadDelay 500000
        putMVar up prevUp
        let nextNum = if prevUp then prevNum + 1 else prevNum - 1
        putMVar num nextNum
        sendPacket botState $ Send (show nextNum) $ msgID message
      "!gotoRoom" : x ->
        do
        closeConnection botState
        euphoriaBot "CounterBot"  (head x) $ countFunction cs
      "!replicateTo" : x ->
        euphoriaBot "CounterBot"  (head x) $ countFunction cs

      _ -> return ()

countFunction _ _ _ =
      return ()
