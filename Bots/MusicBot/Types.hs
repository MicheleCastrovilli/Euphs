{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Types where

import qualified Data.Aeson as J
import qualified Data.Sequence as SQ
import qualified Data.Set as S

import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.Char (isNumber)
import Data.Function (on)

import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (ReaderT)

import Utils
import Countries

import Euphs.Bot (Net)
import Euphs.Types (UserData)


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
  , peopleList    :: TVar People
}

data User = User {
    userData     :: UserData
  , userCountry  :: Maybe Country
}

instance Eq User where
    (==) = (==) `on` userData

instance Ord User where
    compare = compare `on` userData

type People = S.Set User

type Requester = UserData
type QueueTime = Int

data QueueItem = QueueItem {
    metadata  :: !YTMetadata
  , requester :: !Requester
  , startTime :: !QueueTime
  , stopTime  :: !QueueTime
  } deriving (Show,Read)

data QueuedItem = QueuedItem {
    item :: !QueueItem
  , timePlayed :: !Integer
  }

type Queue = SQ.Seq QueueItem
type Queued = SQ.Seq QueuedItem

roomQueue :: String -> String
roomQueue r = r ++ "-queue"

-------------------------- YOUTUBE DATA TYPES

type YoutubeID = String

data YoutubeRequest = YoutubeRequest {
    youtubeID :: YoutubeID
  , startTimeReq :: Maybe QueueTime
  , endTimeReq   :: Maybe QueueTime
}

data YTMetadata = YTMetadata {
    ytID :: String
  , title :: String
  , thumbnailUrl :: String
  , duration :: Int
  , embeddable :: Bool
  , restricted :: [String]
  , allowed :: [String]
} deriving (Show, Read)

data YTResult = One YTMetadata | None | Playlist [YTMetadata]

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
            <*> ((ytl J..: "contentDetails" >>= (J..: "regionRestriction") >>=  (J..: "blocked")) <|> return [])
            <*> ((ytl J..: "contentDetails" >>=  (J..: "regionRestriction") >>=  (J..: "allowed")) <|> return [])
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
    | not $ null $ restricted yt =
        let restOrd = sort (restricted yt) in yt {
              restricted = restOrd
            , allowed = S.toAscList (S.difference countries
                                    (S.fromAscList restOrd))
            }
    | not $ null $ allowed yt =
        let allowOrd = sort (allowed yt) in yt {
              restricted = S.toAscList (S.difference countries
                                       (S.fromAscList allowOrd))
            , allowed = allowOrd
            }
    | otherwise = yt { allowed = S.toAscList countries }

