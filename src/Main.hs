{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where

import            Euphoria.Bot            as B
import            Euphoria.Events         as E
import            Euphoria.Commands       as C
import            Data.Maybe              as M
import qualified  Data.Aeson              as J
import            Control.Concurrent
import            Control.Monad           (forever, unless, mzero)
import            Control.Monad.Trans     (liftIO)
import qualified  Network.WebSockets      as WS
import qualified  Data.Text               as T
import qualified  Data.Text.Encoding      as T
import qualified  Data.Text.IO            as T
import qualified  Data.ByteString         as B
import qualified  Data.ByteString.Lazy    as BL

functions :: [ BotState -> IO () ]

main = do
       euphBot (service "ViviBot" myFunction) "haskell"
       args <- getArgs
       if(length args < 4) then
        putStrLn ("Usage: ./" ++ args !! 1  ++ "<bot name> <room name> [<functions>]"
       else 
        sequence_ map (euphoriaBot (args !! 2)  (args !! 3)) (drop 3 args)


myFunction :: WS.Connection -> MVar Int -> MVar UserData -> EuphEvent -> IO()
myFunction conn count myAgent (SendEvent (MessageData time msgID parentMsg sender content _ _)) 
        = if content == "!testViviBot" then
            do 
            myFile <- readFile "songs"
            queueSongs (lines myFile) count conn msgID
           -- packetId <- getNext count
           -- WS.sendTextData conn $ J.encode (Send packetId ("Hello @" ++  name sender) msgID)
          else
            return ()
myFunction conn count myAgent (SendReply (MessageData time msgID parentMsg sender content _ _))
        = return ()
myFunction conn count myAgent _ = return ()

queueSongs (x:xs)  count conn msgID = do 
                                packetId <- getNext count
                                WS.sendTextData conn $ J.encode (Send packetId x msgID)
                                threadDelay 2000000
                                queueSongs xs count conn msgID 
queueSongs _ _ _ _ = return ()


