{-# LANGUAGE OverloadedStrings #-}
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

functions :: [ (String , BotFunction ) ]
functions =  [ ( "Q" , myFunction ) ]

main = do
       args <- getArgs
       if length args < 2 then
        putStrLn "Usage: ./EuPhBot <bot name> <room name> [<functions>]"
       else 
        do
        print $ drop 2 args
        euphoriaBot "ViviBot" (args !! 1) myFunction
        --mapM_ (void . forkIO . euphoriaBot (head args) ( args !! 1))
          --      (M.mapMaybe (`lookup` functions)  $ drop 2 args)


myFunction :: BotFunction
myFunction botState (SendEvent (MessageData time msgID parentMsg sender content _ _))
        = when (content == "!testViviBot") $
             do
             a <- readFile "MyIDs"
             queueSongs (map ("!q youtube.com/watch?v=" ++) (lines a)) botState msgID
             -- sendPacket botState (Send ("Hello! @" ++ name sender) msgID)

myFunction botState (SendReply (MessageData time msgID parentMsg sender content _ _))
        = return ()
myFunction _ _ = return ()

queueSongs :: [String] -> BotState -> MessageID -> IO ()
queueSongs (x:xs) botState parent= 
  do 
  sendPacket botState (Send (x) parent)
  threadDelay 2000000
  queueSongs xs botState parent
queueSongs _ _ _ = return ()


