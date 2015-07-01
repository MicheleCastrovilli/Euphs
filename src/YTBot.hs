{-# LANGUAGE OverloadedStrings #-}
module YTBot where

import qualified Data.Aeson as J
import           Euphoria.Bot
import           Euphoria.Events
import           Euphoria.Types
import           Euphoria.Commands
import           Control.Concurrent
import           Control.Monad      (forever,liftM,mzero)
import           Data.List


data YTState = YTState (Chan YTMetadata)

data YTMetadata = YTMetadata {
  ytID :: String,
  title :: String,
  thumbnailUrl :: String,
  duration :: Int,
  embeddable :: Bool,
  restricted :: String
}

getYtFun :: IO BotFunction
getYtFun = 
  do
  a <- newChan
  return $ ytFunction $ YTState a

ytFunction :: YTState -> BotFunction
ytFunction ytState botState (SendEvent (MessageData time msgID parentMsg sender content _ _ ))
   =  case words content of 
      "!vq" : x ->
        do
        let ytLink = map getYtID $ filter isYtLink x
        mapM_ (\x -> queueSong x botState msgID ytState) ytLink

ytFunction ytState botState (SnapshotEvent {}) =
  ytLoop botState ytState 

instance J.FromJSON YTMetadata where
  parseJSON (J.Object v) = 
    do
    ytl <- head <$> v J..: "items"
    YTMetadata <$> ( ytl J..: "id" )
               <*> ( ytl J..: "title" )
               <*> ( ytl J..: "thumbnails" >>= (J..: "default") >>= (J..: "url"))
               <*> ( liftM parseISO8601 ( ytl J..: "contentDetails" >>= (J..: "duration")))
               <*> ( ytl J..: "status" >>= (J..: "embeddable"))
               <*> ( ytl J..: "contentDetails"  >>=  (J..:? "regionRestriction") J..!= mzero >>=  (J..:? "blocked")  J..!= "")

isYtLink :: String -> Bool
isYtLink x = isPrefixOf "youtube.com/watch?v=" x || isPrefixOf "youtu.be/watch?v=" x

getYtID :: String -> String
getYtID = takeWhile (/= '&') . drop 1 . dropWhile  ( '=' /= )

parseISO8601 = error "Not Implemented"

retrieveYtData = error "Not Implemented"

queueSong x bs idRepl (YTState queue) = 
  do
  ytData <- retrieveYtData x
  case ytData of
    Left err -> sendPacket bs (Send err idRepl)
    Right yt -> do
                writeChan queue yt
                sendPacket bs (Send "Queued!" idRepl)

ytLoop botState (YTState queue) = forever $ do
  x <- readChan queue
  sendPacket botState (Send ("!play youtube.com/watch?v=" ++ ytID x) "")
  threadDelay $ duration x

