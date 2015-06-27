{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (
    main
) where

import           Euphoria.Bot
import           Euphoria.Events
import           Euphoria.Commands
import           Euphoria.Types
import           Data.Maybe                  as M
import           System.Environment
import           Control.Concurrent
import           Control.Monad               (void, when)
import           Text.Show                   as S
import           System.Process

functions :: [ (String , BotFunction ) ]
functions =  [ ( "Q" , myFunction ),
                ("P",  fortuneFunction)]
data CountState = CountState (MVar Bool) (MVar Int)
main = do
       args <- getArgs
       if length args < 2 then
        putStrLn "Usage: ./EuPhBot <bot name> <room name> [<functions>]"
       else 
        do
        print $ drop 2 args
        --euphoriaBot "ViviBot" (args !! 1) myFunction
        a <- newMVar True
        b <- newMVar 0
        forkIO $ euphoriaBot "CounterBot" (args !! 1) $ countFunction $ CountState a b
        euphoriaBot "FortuneBot" (args !! 1) fortuneFunction
        --mapM_ (void . forkIO . euphoriaBot (head args) ( args !! 1))
          --      (M.mapMaybe (`lookup` functions)  $ drop 2 args)


myFunction :: BotFunction
myFunction botState (SendEvent (MessageData time msgID parentMsg sender content _ _))
        = when (content == "!testViviBot") $
             do
             !a <- readFile "MyIDs"
             -- (map ("!q youtube.com/watch?v=" ++) (lines a))
             queueSongs [1..5] botState msgID
             -- sendPacket botState (Send ("Hello! @" ++ name sender) msgID)

myFunction botState (SendReply (MessageData time msgID parentMsg sender content _ _))
        = return ()
myFunction _ _ = return ()

queueSongs :: (Show a) => [a] -> BotState -> MessageID -> IO ()
queueSongs (x:xs) botState parent= 
  do 
  sendPacket botState (Send (show x) parent)
  threadDelay 2000000
  queueSongs xs botState parent
queueSongs _ _ _ = return ()

fortuneFunction :: BotFunction
fortuneFunction botState (SendEvent (MessageData time msgID parentMsg sender content _ _))
  = when (content == "!fortune") $ 
      do
      a <- readProcess "fortune" ["-s"] []
      putStrLn a
      sendPacket botState (Send a msgID)
      return ()

fortuneFunction _ _ = return ()

countFunction :: CountState -> BotFunction
countFunction (CountState up num) botState (SendEvent (MessageData time msgID parentMsg sender content _ _ ))
   = if content == "!upCount" then
        do
        prevUp <- takeMVar up
        putMVar up True
        sendPacket botState (Send (if prevUp then "It was already up!" else "Set to up") msgID)
     else if content == "!downCount" then
        do 
        prevUp <- takeMVar up
        putMVar up False
        sendPacket botState (Send (if prevUp then "Set to down" else "It was already down!") msgID)
    else if content == "!count" then
        do
        prevNum <- takeMVar num
        prevUp  <- takeMVar up
        putMVar up prevUp
        let nextNum = if prevUp then prevNum + 1 else prevNum - 1
        putMVar num nextNum
        sendPacket botState (Send (show nextNum) msgID)
    else if content == "!replicate" then
        euphoriaBot "ReplicatedCounterBot" "test" $ countFunction $ CountState up num
    else 
        return ()
    
countFunction _ _ _ 
   = return ()

