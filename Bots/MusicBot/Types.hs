{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Types where

import qualified Data.Aeson as J
import qualified Data.Sequence as SQ
import qualified Data.Set as S

import Data.Maybe (fromMaybe)
import Data.Char (isNumber)
import Data.Function (on)

import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar,TMVar)
import Control.Monad.Reader (ReaderT)

import Utils
import Countries

import Euphs.Bot (Net)
import Euphs.Types (UserData,name)


type MusicBot = ReaderT MusicState Net

data MConfig = MConfig {
    apiKeyConf :: !String
  , stopped    :: !Bool
  , sequenceMemory :: !Int
  , restingTime :: !Int
} deriving (Show)

instance J.FromJSON MConfig where
    parseJSON (J.Object v) = do
      MConfig <$> (v J..: "youtube" >>= (J..: "api"))
              <*> v J..: "stopped"
              <*> v J..:? "prevMemory" J..!= 50
              <*> v J..:? "rest" J..!=6
    parseJSON _ = fail "Error in parsing the config file"

data MusicState = MusicState {
    queue         :: TVar Queue
  , previousQueue :: TVar Queued
  , musicConfig   :: !MConfig
  , peopleSet     :: TVar People
  , skipSong      :: TMVar ()
}

data User = User {
    userData     :: UserData
  , userCountry  :: Maybe Country
}

instance Eq User where
    (==) = (==) `on` userData

instance Ord User where
    compare = compare `on` userData

instance Show User where
    show (User d c) =  name d ++ " from " ++ maybe "???" showCountry c


type People = S.Set User

type Requester = UserData
type QueueTime = Int

data QueueItem = QueueItem {
    metadata  :: !YTMetadata
  , requester :: !Requester
  , startTime :: !QueueTime
  , stopTime  :: !QueueTime
  } deriving (Show,Read)

playFormat :: QueueItem -> String
playFormat q = "youtube.com/watch?v=" ++ (ytID $ metadata q) ++ mayTimeStart ++ mayTimeStop
    where mayTimeStart = if (startTime q /= 0) then "&t="  ++ show (startTime q) else ""
          mayTimeStop  = if (stopTime  q /= duration (metadata $ q)) then "&te=" ++ show (startTime q) else ""

data QueuedItem = QueuedItem {
    queuedItem :: !QueueItem
  , timePlayed :: !Integer
  }
  deriving Show

type Queue = SQ.Seq QueueItem
type Queued = SQ.Seq QueuedItem

pqAdd :: MConfig -> QueuedItem -> Queued -> Queued
pqAdd mc item sq = let l = sequenceMemory mc in
                   SQ.take l $ item SQ.<| sq

sqHeadMay :: SQ.Seq a -> Maybe a
sqHeadMay s = case SQ.viewl s of
                SQ.EmptyL -> Nothing
                x SQ.:< _ -> Just x

sqInsert :: [QueueItem] -> Queue -> Queue
sqInsert items q = SQ.fromList items SQ.>< q

sqAppend :: [QueueItem] -> Queue -> Queue
sqAppend items q = q SQ.>< SQ.fromList items

sqInsertPos :: [QueueItem] -> Int -> Queue -> Queue
sqInsertPos items pos q = let (p1,p2) = SQ.splitAt pos q in
                              p1 SQ.>< sqInsert items p2


roomQueue :: String -> String
roomQueue r = r ++ "-queue"

-------------------------- YOUTUBE DATA TYPES

type YoutubeID = String

data YoutubeRequest = YoutubeRequest {
    youtubeID :: YoutubeID
  , startTimeReq :: Maybe QueueTime
  , stopTimeReq   :: Maybe QueueTime
}

data YTMetadata = YTMetadata {
    ytID :: String
  , title :: String
  , thumbnailUrl :: String
  , duration :: Int
  , embeddable :: Bool
  , restricted :: S.Set Country
  , allowed :: S.Set Country
} deriving (Show, Read)

data YTResult = One YTMetadata | None | Playlist [YTMetadata]
    deriving (Show)

instance J.FromJSON YTResult where
    parseJSON (J.Object v) = do
        res <- v J..: "pageInfo" >>= (J..: "totalResults")
        if (res :: Int) == 0 then
            return None
        else if res == 1 then
            ((One . head) <$> v J..: "items")
        else
            Playlist <$> (v J..: "items")
    parseJSON _ = fail "Couldn't parse the result"

instance J.FromJSON YTMetadata where
    parseJSON (J.Object ytl) = do
        res <- YTMetadata <$> (ytl J..: "id")
            <*> (ytl J..: "snippet" >>= (J..: "title"))
            <*> (ytl J..: "snippet" >>= (J..: "thumbnails") >>= (J..: "default") >>= (J..: "url"))
            <*> (parseISO8601 <$> (ytl J..: "contentDetails" >>= (J..: "duration")))
            <*> (ytl J..: "status" >>= (J..: "embeddable"))
            <*> (fmap toFakeCountry $ (ytl J..: "contentDetails" >>= (J..: "regionRestriction") >>=  (J..: "blocked")) <|> return [])
            <*> (fmap toFakeCountry $ (ytl J..: "contentDetails" >>= (J..: "regionRestriction") >>=  (J..: "allowed")) <|> return [])
        return $ balanceAllowed res
    parseJSON _ = fail "Couldn't parse API"

parseISO8601 :: String -> Int
parseISO8601 x =
  let sec  = readFun 'S'
      min' = readFun 'M'
      hour = readFun 'H'
      in (sec + 60*min' + hour*3600)
  where readFun y = fromMaybe 0 (maybeRead
                        (reverse $ takeWhile isNumber $
                            drop 1 $ dropWhile (/=y) $ reverse x
                        ) :: Maybe Int)

balanceAllowed :: YTMetadata -> YTMetadata
balanceAllowed yt
    | not $ S.null $ restricted yt = yt {
              restricted = S.intersection countries $ restricted yt
            , allowed    = S.difference countries $ restricted yt
            }
    | not $ null $ allowed yt = yt {
              restricted = S.difference countries $ allowed yt
            , allowed    = S.intersection countries $ allowed yt
            }
    | otherwise = yt { allowed = countries }

