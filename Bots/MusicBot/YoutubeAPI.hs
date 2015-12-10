module YoutubeAPI where

import qualified Data.Aeson as J
import           Control.Monad
import           Control.Retry

import           Data.List.Split
import           Data.List (stripPrefix, isInfixOf, isSuffixOf)
import           Data.Char (isAlphaNum, isNumber)
import           Data.Maybe (fromMaybe)
import           Text.Parsec
import qualified Control.Applicative as A ((<|>))
import           Safe

import           Network.URI
import qualified Network.Http.Client as H
import qualified Data.ByteString.Char8 as B

import           Euphs.Easy
import           Euphs.Types (UserData)

import           Utils
import           Types

------------------------------  BOT   TIME ------------------------------
--
--main :: IO ()
--main = easyBot [("!test", (testFun, "A test function", "This should explain more, but doesn't."))]
--
--testFun s = return $ unlines $ map (\x -> maybe ("Couldn't parse the link: " ++ x) show $ parseRequest x) $ words s
--
------------------------------ END BOT TIME ------------------------------

limitedBackoff :: RetryPolicy
limitedBackoff = exponentialBackoff 50 <> limitRetries 5

-- | Part of the URL for asking the youtube API
apiUrl :: String
apiUrl = "https://content.googleapis.com/youtube/v3/videos?part=snippet%2C+status%2C+contentDetails&id="

-- | The API key, passed from the config outside, in the yaml file
apiToken :: String -> String
apiToken apiKeyStr = "&key=" ++ apiKeyStr

-- | Parsing a request from a link
parseRequest :: String -> Maybe YTRequest
parseRequest request = do
    uri <- parseURI request A.<|> (parseURIReference $ "//" ++ request)
    uriAuth <- uriAuthority uri
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
parseTimes :: [(String , Maybe String)] -> (Maybe QueueTime, Maybe QueueTime)
parseTimes q = let t1 = parseAux q "t"
                   t2 = parseAux q "te"
               in  case (t1,t2) of
                     (Just m1, Just m2) -> if m1 > m2 then (Just m2,Just m1) else (Just m1,Just m2)
                     x -> x
    where parseAux q par  = do
            a <- join $ lookup par q
            parseTimeQuery a

-- | Auxiliary function for parsing the time
parseTimeQuery :: String -> Maybe QueueTime
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
queryTable s = map (\x -> let y = splitOn "=" x in (head y, headMay $ drop 1 y)) $ wordsBy (`elem` "?&") s

-- | Auxiliary guard, to check if an ID is valid.
guardId :: (MonadPlus m) => String -> m ()
guardId ytid = guard $ length ytid >=9 && all (\x -> isAlphaNum x || x == '-' || x == '_') ytid



retrieveRequest :: MusicState ->
                   YTRequest ->
                   UserData ->
                   IO [QueueItem]
retrieveRequest ms (ytid, starttime, endtime) usr = do
    y <- retrieveYoutube ms ytid
    let l = case y of
                None -> []
                One x -> [x]
                Playlist p -> p
    return $  fmap (\x -> QueueItem x usr (modify x starttime) $ modifyE x endtime) l
    where modify  x time = fromMaybe 0 $ do
                           t <- time
                           guard $ duration x < t
                           time
          modifyE x time = fromMaybe (duration x) $ do
                           t <- time
                           guard $ duration x > t && t > 0
                           time

retrieveYoutube :: MusicState ->
                   YoutubeID ->
                   IO YTResult
retrieveYoutube ms ytId = do
    let ak = apiKeyConf $ musicConfig ms
    recoverAll limitedBackoff
                $ H.get (B.pack $ apiUrl ++ ytId ++ apiToken ak)
                    H.jsonHandler :: IO YTResult


