module YoutubeAPI where

import qualified Data.Aeson as J
import Network.URI
import Data.List.Split
import Control.Monad
import Text.Parsec
import Data.Char (isAlphaNum)
import Data.List (stripPrefix, isInfixOf, isSuffixOf)
import qualified Control.Applicative as A ((<|>))
import Debug.Trace (traceShowId)

import Utils

import Euphs.Easy

type YoutubeID = String
type YTTime = Int
type YTRequest = (YoutubeID, Maybe YTTime, Maybe YTTime)

{-
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
-}

------------------------------  BOT   TIME ------------------------------

main :: IO ()
main = easyBot [("!test", (testFun, "A test function", "This should explain more, but doesn't."))]

testFun s = return $ unlines $ map (\x -> maybe ("Couldn't parse the link: " ++ x) show $ parseRequest x) $ words s

------------------------------ END BOT TIME ------------------------------

-- | Part of the URL for asking the youtube API
apiUrl :: String
apiUrl = "https://content.googleapis.com/youtube/v3/videos?part=snippet%2C+status%2C+contentDetails&id="

-- | The API key, passed from the config outside, in the yaml file
apiToken :: String -> String
apiToken apiKeyStr = "&key=" ++ apiKeyStr

-- | Parsing a request from a link
parseRequest :: String -> Maybe YTRequest
parseRequest request = do
    uri <- parseURI request A.<|> (traceShowId $ parseURIReference $ "//" ++ request)
    uriAuth <- traceShowId $ uriAuthority uri
    parseGoogle uri uriAuth A.<|> parseYoutube uri uriAuth

-- | Parsing a google referral link
parseGoogle :: URI -> URIAuth -> Maybe YTRequest
parseGoogle uri uriAuth = do
    guard $ isInfixOf "google" $ uriRegName uriAuth
    guard $ (== "/url") $ uriPath uri
    let qt = queryTable $ uriQuery uri
    url <- join $ lookup "url" qt
    uri' <- parseURI  (unEscapeString url) A.<|> parseURIReference ("//" ++ unEscapeString url)
    uriAuth' <- uriAuthority uri'
    parseYoutube uri' uriAuth'

-- | Convenience function, to parse both the youtube.com and youtu.be link
parseYoutube :: URI -> URIAuth -> Maybe YTRequest
parseYoutube uri uriAuth = parseYoutubeInternal uri uriAuth A.<|> parseYoutubeShort uri uriAuth

-- | Function to parse exclusively the youtube.com link
parseYoutubeInternal :: URI -> URIAuth -> Maybe YTRequest
parseYoutubeInternal uri uriAuth = do
    guard $ (isSuffixOf "youtube.com") $ uriRegName uriAuth
    guard $ (== "/watch") $ uriPath uri
    let qt =  queryTable $ uriQuery uri
    ytid <- join $ lookup "v" qt
    guardId ytid
    let (timeStart, timeEnd) = parseTimes qt
    return (ytid, timeStart, timeEnd)

-- | Function to parse exclusively the youtu.be link
parseYoutubeShort :: URI -> URIAuth -> Maybe YTRequest
parseYoutubeShort uri uriAuth = do
    guard $ (isSuffixOf "youtu.be") $ uriRegName uriAuth
    let ytid = uriPath uri
    guardId ytid
    let (timeStart, timeEnd) = parseTimes $ queryTable $ uriQuery uri
    return (ytid, timeStart, timeEnd)

-- | Funciton to parse the time in the NhNmNs notation, providing an order to the two times
parseTimes :: [(String , Maybe String)] -> (Maybe YTTime, Maybe YTTime)
parseTimes q = let t1 = parseAux q "t"
                   t2 = parseAux q "te"
               in  case (t1,t2) of
                     (Just m1, Just m2) -> if m1 > m2 then (Just m2,Just m1) else (Just m1,Just m2)
                     x -> x
    where parseAux q par  = do
            a <- join $ lookup par q
            parseTimeQuery a

-- | Auxiliary function for parsing the time
parseTimeQuery :: String -> Maybe YTTime
parseTimeQuery s =
    case parse auxParser "(unknown)" s of
        Left _ -> Nothing -- Ignoring the error, in future i could print it to the user, to tell what they did wrong.
        Right x -> Just x
    where auxParser = do -- We are in the Parser monad right now
            h <- try (aux' 'h') <|> return 0 -- Giving 0 in case the time is not parsed
            m <- try (aux' 'm') <|> return 0 -- Giving 0 in case the time is not parsed
            s' <- aux'' 's' <|> return 0 -- Giving 0 in case the time is not parsed
            eof
            return (h*3600+m*60+s')
          aux' c = (do
            l <- many1 digit
            char c
            case (maybeRead l :: Maybe Int) of
                Nothing -> fail "Integer out of Int boundaries"
                Just x -> return x ) :: Parsec String () Int
          aux'' c = (do
            l <- many1 digit
            optional  $ char c
            case (maybeRead l :: Maybe Int) of
                Nothing -> fail "Integer out of Int boundaries"
                Just x -> return x ) :: Parsec String () Int

-- | Auxiliary function, for converting the query URI, into a lookup list
queryTable :: String -> [(String, Maybe String)]
queryTable s = map (\x -> let y = splitOn "=" x in (head y, safeHead $ drop 1 y)) $ wordsBy (`elem` "?&") s

-- | Auxiliary guard, to check if an ID is valid.
guardId :: (MonadPlus m) => String -> m ()
guardId ytid = guard $ length ytid >=9 && all (\x -> isAlphaNum x || x == '-' || x == '_') ytid


{-
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

-}
