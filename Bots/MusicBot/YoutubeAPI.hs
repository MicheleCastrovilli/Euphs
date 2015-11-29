module YoutubeAPI where

type YoutubeID = String
type YTTime = Integer
type YTRequest = (YoutubeID, YTTime, YTTime)

data YoutubeRequest = YoutubeRequest {
    youtubeID :: YoutubeID
  , startTime :: Maybe YTTime
  , endTime   :: Maybe YTTime
}

data YTMetadata = YTMetadata {
    ytID :: String
  , title :: String
  , thumbnailUrl :: String
  , duration :: Integer
  , durationStr :: String
  , embeddable :: Bool
  , restricted :: [String]
  , allowed :: [String]
} deriving (Show, Read)

apiUrl :: String
apiUrl = "https://content.googleapis.com/youtube/v3/videos?part=snippet%2C+status%2C+contentDetails&id="

apiToken :: String -> String
apiToken apiKeyStr = "&key=" ++ apiKeyStr

getYtReq :: String -> Maybe YTRequest
getYtReq y = do
             let mYT = "youtube.com/watch"
             let mVid = param ++ "v=([A-Za-z0-9_\\-]{9,})" :: String
             let mYTS = "youtu.be/([A-Za-z0-9_\\-]{9,})" :: String
             let param = "(&|\\?)"
             let t1 c = param ++ c ++"=([0-9]+h)?([0-9]+m)?([0-9]+s?)?" :: String

             (before, _, after, groups)  <-  y =~~ m1 <|> y =~~ m2 :: Maybe (String, String, String, [String])
             let startTime = maybe 0 parseTime (after =~~  t1 "t"  :: Maybe (String, String, String, [String]))
             let endTime   = maybe (-1) parseTime (after =~~  t1 "te" :: Maybe (String, String, String, [String]))
             if endTime > 0  && startTime > endTime then
                 return $ (groups !! 0,  endTime, startTime)
             else
                 return $ (groups !! 0, startTime, endTime)
             where readFun x = fromMaybe 0 (maybeRead2 (takeWhile isNumber x) :: Maybe Integer)
                   parseTime (_, _, _, grp) = sum $ zipWith (*) [3600, 60, 1] $ map readFun $ drop 1 grp

retrieveRequest :: YTState -> YTRequest -> UserData -> Integer -> IO (Either String YTQueueItem)
retrieveRequest ytState (ytid, starttime, endtime) usr time = retrieveYtData ytid ytState
    >>= (return . fmap (\x -> YTQueueItem x usr (modify x starttime) (modifyE x endtime) time))
    where modify  x time = if duration x < time then 0 else time
          modifyE x time = if duration x > time && time > 0 then time else duration x

retrieveYtData :: YoutubeID -> YTState -> IO (Either String YTMetadata)
retrieveYtData ytId ytState = do
  --putStrLn ytId
  ytJson <- recoverAll limitedBackoff $ simpleHttp $ apiUrl ++ ytId ++ apiToken (apiKey ytState)
  --B.putStrLn ytJson
  return $ J.eitherDecode ytJson

parseISO8601 :: String -> Integer
parseISO8601 x =
  let sec  = readFun 'S'
      min' = readFun 'M'
      hour = readFun 'H'
      in (sec + 60*min' + hour*3600)
  where readFun y = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/=y) $ reverse x ) :: Maybe Integer)

balanceAllowed :: YTMetadata -> YTMetadata
balanceAllowed yt | not $ null $ restricted yt = let restOrd = sort (restricted yt) in yt {
                        restricted = restOrd,
                        allowed = S.toAscList (S.difference countries (S.fromAscList restOrd))
                        }
                  | not $ null $ allowed yt =  let allowOrd = sort (allowed yt) in yt {
                        restricted = S.toAscList (S.difference countries (S.fromAscList allowOrd)),
                        allowed = allowOrd
                    }
                  | otherwise = yt {
                      allowed = S.toAscList countries
                    }


instance J.FromJSON YTMetadata where
  parseJSON (J.Object v) = do
    tmp <-  safeHead <$> v J..: "items"
    case tmp of
      Nothing -> mzero
      Just ytl -> do
                  snippet <- ytl J..: "snippet"
                  (YTMetadata <$> ( ytl J..: "id" )
                             <*> ( snippet J..: "title" )
                             <*> ( snippet J..: "thumbnails" >>= (J..: "default") >>= (J..: "url"))
                             <*> ( parseISO8601 <$> ( ytl J..: "contentDetails" >>= (J..: "duration")))
                             <*> ( ytl J..: "contentDetails" >>= (J..: "duration"))
                             <*> ( ytl J..: "status" >>= (J..: "embeddable"))
                             <*> ((ytl J..: "contentDetails"  >>=  (J..: "regionRestriction") >>=  (J..: "blocked")) <|> return [])
                             <*> ((ytl J..: "contentDetails"  >>=  (J..: "regionRestriction") >>=  (J..: "allowed")) <|> return []) >>= (return . balanceAllowed))
  parseJSON _ = mzero
