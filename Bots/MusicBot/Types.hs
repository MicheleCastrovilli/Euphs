{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Concurrent.STM (TVar, TMVar)
import qualified Data.Aeson as J
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.Char (isNumber)
import Euphs.Types (UserData)
import Control.Applicative ((<|>))

import Utils

data MConfig = MConfig {
    apiKeyConf :: String
  , stopped    :: Bool
  , sequenceMemory :: Int
  , restingTime :: Int
} deriving (Show)

instance J.FromJSON MConfig where
    parseJSON (J.Object v) = do
      MConfig <$> (v J..: "youtube" >>= (J..: "api"))
              <*> v J..: "stopped"
              <*> v J..:? "prevMemory" J..!= 50
              <*> v J..:? "rest" J..!=6

data MusicState = MusicState {
    queue         :: TVar Queue
  , previousQueue :: TVar Queue
  , musicConfig   :: MConfig
  , peopleList    :: TVar People
}

data User = User {
    ud :: UserData
  , c  :: Country
}

instance Eq User where
    x == y = ud x == ud y

instance Ord User where
    compare x y = compare (ud x) (ud y)

type People = S.Set User
type Country = String

type Requester = UserData
type QueueTime = Int

data QueueItem = QueueItem {
    metadata  :: YTMetadata
  , requester :: Requester
  , startTime :: QueueTime
  , stopTime  :: QueueTime
  } deriving (Show,Read)

data QueuedItem = QueuedItem {
    item :: QueueItem
  , timePlayed :: Integer
  }

type Queue = SQ.Seq QueueItem

roomQueue :: String -> String
roomQueue r = r ++ "-queue"

-------------------------- YOUTUBE DATA TYPES

type YoutubeID = String
type YTRequest = (YoutubeID, Maybe QueueTime, Maybe QueueTime)

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

instance J.FromJSON YTResult where
    parseJSON (J.Object v) = do
        res <- v J..: "pageInfo" >>= (J..: "totalResults")
        if (res :: Int) == 0 then
            return None
        else if res == 1 then
            ((One . head) <$> v J..: "items")
        else
            Playlist <$> (v J..: "items")

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

countries :: S.Set String
countries = S.fromAscList
             ["AD","AE","AF","AG","AI","AL","AM","AO","AQ","AR","AS","AT","AU","AW","AX",
              "AZ","BA","BB","BD","BE","BF","BG","BH","BI","BJ","BL","BM","BN","BO","BR",
              "BS","BT","BV","BW","BY","BZ","CA","CC","CD","CF","CG","CH","CI","CK","CL",
              "CM","CN","CO","CR","CU","CV","CX","CY","CZ","DE","DJ","DK","DM","DO","DZ",
              "EC","EE","EG","EH","ER","ES","ET","FI","FJ","FK","FM","FO","FR","GA","GB",
              "GD","GE","GF","GG","GH","GI","GL","GM","GN","GP","GQ","GR","GS","GT","GU",
              "GW","GY","HK","HM","HN","HR","HT","HU","ID","IE","IL","IM","IN","IO","IQ",
              "IR","IS","IT","JE","JM","JO","JP","KE","KG","KH","KI","KM","KN","KP","KR",
              "KW","KY","KZ","LA","LB","LC","LI","LK","LR","LS","LT","LU","LV","LY","MA",
              "MC","MD","ME","MF","MG","MH","MK","ML","MM","MN","MO","MP","MQ","MR","MS",
              "MT","MU","MV","MW","MX","MY","MZ","NA","NC","NE","NF","NG","NI","NL","NO",
              "NP","NR","NU","NZ","OM","PA","PE","PF","PG","PH","PK","PL","PM","PN","PR",
              "PS","PT","PW","PY","QA","RE","RO","RS","RU","RW","SA","SB","SC","SD","SE",
              "SG","SH","SI","SJ","SK","SL","SM","SN","SO","SR","ST","SV","SY","SZ","TC",
              "TD","TF","TG","TH","TJ","TK","TL","TM","TN","TO","TR","TT","TV","TW","TZ",
              "UA","UG","UM","US","UY","UZ","VA","VC","VE","VG","VI","VN","VU","WF","WS",
              "YE","YT","ZA","ZM","ZW"]
