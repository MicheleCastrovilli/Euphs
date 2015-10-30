{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module YTBot where

import qualified Data.Aeson as J
import           Euphoria.Bot
import           Euphoria.Events
import           Euphoria.Types
import           Euphoria.Commands
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad      (forever, mzero, void, when, unless, liftM2 , ap)
import           Control.Retry
import           Data.List
import           Network.HTTP.Conduit
import           Data.Char
import           Data.Maybe
import           System.Timeout
import           Data.Time.Clock.POSIX
import           Control.Exception
import           System.Random
import           Data.Function
import qualified Data.Set as S
import qualified Database.HDBC as H
import qualified Database.HDBC.Sqlite3 as SQL
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Sequence as SQ
import           Text.Regex.Base
import           Text.Regex.TDFA
import qualified Data.Foldable as F

data YTState = YTState {
              queue      :: MVar YTQueue,
              skip       :: MVar Bool,
              play       :: MVar Bool,
              lastPlayed :: MVar YTQueue,
              apiKey     :: String,
              noPlay     :: Bool
              }

data YTMetadata = YTMetadata {
  ytID :: String,
  title :: String,
  thumbnailUrl :: String,
  duration :: Integer,
  durationStr :: String,
  embeddable :: Bool,
  restricted :: [String],
  allowed :: [String]
} deriving (Show, Read)

type YoutubeID = String
type YTStart = Integer
type YTRequest = (YoutubeID, YTStart)

type Requester = UserData

data YTQueueItem = YTQueueItem {
    ytmeta    :: YTMetadata,
    requester :: Requester,
    start     :: YTStart,
    timePlayed :: Integer
}   deriving (Show,Read)

type YTQueue = SQ.Seq YTQueueItem

sequenceMemory :: Int
sequenceMemory = 100

apiUrl :: String
apiUrl = "https://content.googleapis.com/youtube/v3/videos?part=snippet%2C+status%2C+contentDetails&id="

apiToken :: String -> String
apiToken apiKeyStr = "&key=" ++ apiKeyStr

getYtFun :: String -> String -> String ->  IO YTState
getYtFun apiKeyStr noplay room =
  do
  !a <- catch (readFile (room ++ "-queue")) (\(SomeException _) -> return "")
  let x = fromMaybe SQ.empty (maybeRead2 a :: Maybe YTQueue)
  que <- newMVar x
  skipV <- newEmptyMVar
  playV <- newEmptyMVar
  lastPlayedV <- newMVar SQ.empty
  let noPlay' = fromMaybe False (maybeRead2 noplay :: Maybe Bool)
  return $ YTState que skipV playV lastPlayedV apiKeyStr noPlay'

ytFunction :: YTState -> BotFunction
ytFunction ytState botState (SendEvent (MessageData time mesgID _ sndUser !content _ _ ))
   = case (let (z:zs) = words content in map toLower z : zs) of
     (stripPrefix "!dramaticskip"  -> Just _) :_      -> dramaticSkip ytState botState
     (stripPrefix "!dskip"         -> Just _) :_      -> dramaticSkip ytState botState
     (stripPrefix "!dumpq"         -> Just _) :_      -> dumpQueue ytState botState mesgID
     (stripPrefix "!queuefirst"    -> Just r) :x      -> queueSongs (r:x) botState ytState mesgID sndUser 1
     (stripPrefix "!restrict"      -> Just _) :x      -> showRestrictions ytState botState mesgID (fromMaybe "" $ safeHead x) False
     (stripPrefix "!allowed"       -> Just _) :x      -> showRestrictions ytState botState mesgID (fromMaybe "" $ safeHead x) True
     (stripPrefix "!qf"            -> Just r) :x      -> queueSongs (r:x) botState ytState mesgID sndUser 1
     (stripPrefix "!q"             -> Just r) :x      -> queueSongs (r:x) botState ytState mesgID sndUser (-1)
     (stripPrefix "!sub"           -> Just _) :n:y:_  -> replaceSong botState ytState mesgID sndUser n y
     (stripPrefix "!ins"           -> Just _) :n:x    -> insertSongs x botState ytState mesgID sndUser n
     (stripPrefix "!del"           -> Just _) :n:x    -> deleteSongs botState ytState mesgID sndUser n $ fromMaybe "1" $ safeHead x
     (stripPrefix "!rem"           -> Just _) :n:x    -> deleteSongs botState ytState mesgID sndUser n $ fromMaybe "1" $ safeHead x
     (stripPrefix "!rm"            -> Just _) :n:x    -> deleteSongs botState ytState mesgID sndUser n $ fromMaybe "1" $ safeHead x
     (stripPrefix "!list"          -> Just _) :x      -> listQueue ytState botState mesgID $ getOpts x
     (stripPrefix "!skip"          -> Just _) :_      -> skipSong ytState
     (stripPrefix "!kill"          -> Just _) :x:_    -> when (filter isAlphaNum x == filter isAlphaNum (botName botState)) (sendPacket botState (Send "Bot is kill." mesgID) >> closeConnection botState True)
     (stripPrefix "!neonlightshow" -> Just _) :_      -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x mesgID)
     (stripPrefix "!nls"           -> Just _) :_      -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x mesgID)
     (stripPrefix "!help"          -> Just _) :x:_    -> when (filter isAlphaNum x == filter isAlphaNum (botName botState))
                                                         $ sendPacket botState $ Send ( helpFun $ botName botState ) mesgID
     (stripPrefix "!help"          -> Just _) :_      -> sendPacket botState $ Send (helpFunShort $ botName botState) mesgID
     (stripPrefix "!switch"        -> Just _) :x      -> switchSongs ytState botState mesgID x
     (stripPrefix "!swap"          -> Just _) :x      -> switchSongs ytState botState mesgID x
     (stripPrefix "!test"          -> Just r) :_      -> queueSongs [r] botState ytState mesgID sndUser (-1)
     {-
      -(stripPrefix "!vsave"          -> Just _) :x:_    -> saveList botState ytState mesgID x
      -(stripPrefix "!vload"          -> Just _) :x:_    -> loadList botState ytState mesgID x
      -}
     xs -> do
           let playLink = findPlay xs
           unless  ( null playLink ) (
               do
               ytItem <- catch (retrieveRequest ytState (head playLink) sndUser time) (\ (SomeException e) -> return $ Left $ show e)
               case ytItem of
                 Left _ -> putStrLn "Impossible to parse yt api"
                 Right ytSong -> modifyMVarMasked_ (lastPlayed ytState) (return . addToBack ytSong) >>
                                 putMVar  (skip ytState)     False)

ytFunction ytState botState se@(SnapshotEvent {}) =
  forkIO (do
            _ <- readMVar (closedBot botState)
            ytq <- takeMVar (queue ytState)
            writeFile (botRoom botState ++ "-queue") $ show ytq ) >>
  when (not $ noPlay ytState) (
    do
    let playLink = take 1 $ filter (\(_,x) -> not $ null x ) $
                   map (\x -> (x, findPlay $ words $ contentMsg x)) $
                   sortBy (flip compare `on` timeRecieved) $ messages se

    ytLink <- if not $ null playLink then
                catch (retrieveRequest ytState (head $ snd $ head playLink) (sender $ fst $ head playLink) (timeRecieved $ fst $ head playLink) ) (\ (SomeException e) -> return $ Left $ show e)
              else
                return $ Left "No Links Found"

    case ytLink of
      Left _       -> return ()
      Right ytSong -> modifyMVarMasked_ (lastPlayed ytState) (return . addToBack ytSong)

    ytLoop botState ytState
  )

ytFunction _ _ _ = return ()

addToBack :: YTQueueItem -> SQ.Seq YTQueueItem -> SQ.Seq YTQueueItem
addToBack yt sq = if length sq < sequenceMemory then (SQ.take (SQ.length sq - 1) $ yt SQ.<| sq) else yt SQ.<| sq

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

getYtReq :: String -> Maybe YTRequest
getYtReq y = do
             let m1 = "youtube.com/watch?v=([A-Za-z0-9_\\-]{9,})" :: String
             let m2 = "youtu.be/([A-Za-z0-9_\\-]{9,})" :: String
             let t1 = "(?:&|\\?)?t=([0-9]+h)?([0-9]+m)?([0-9]+s?)?" :: String
             (before, after, _, groups)  <- y =~~ m1 <|> y =~~ m2 :: Maybe (String, String, String, [String])
             let startTime = maybe 0 parseTime ((after =~~  t1) :: Maybe (String, String, String, [String]))
             return $ (groups !! 0, startTime)
             where readFun x = fromMaybe 0 (maybeRead2 (takeWhile isNumber x) :: Maybe Integer)
                   parseTime (_, _, _, grp) = sum $ zipWith (*) [3600, 60, 1] $ map readFun grp


filterLinks :: [String] -> [YTRequest]
filterLinks = mapMaybe getYtReq

findPlay :: [String] -> [YTRequest]
findPlay xs = take 1 $ mapMaybe getYtReq $ dropWhile (/="!play") xs

parseISO8601 :: String -> Integer
parseISO8601 x =
  let sec  = readFun 'S'
      min' = readFun 'M'
      hour = readFun 'H'
      in (6 + sec + 60*min' + hour*3600)
  where readFun y = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/=y) $ reverse x ) :: Maybe Integer)

retrieveYtData :: YoutubeID -> YTState -> IO (Either String YTMetadata)
retrieveYtData ytId ytState = do
  --putStrLn ytId
  ytJson <- recoverAll limitedBackoff $ simpleHttp $ apiUrl ++ ytId ++ apiToken (apiKey ytState)
  --B.putStrLn ytJson
  return $ J.eitherDecode ytJson

retrieveRequest :: YTState -> YTRequest -> UserData -> Integer -> IO (Either String YTQueueItem)
retrieveRequest ytState (ytid, starttime) usr time = retrieveYtData ytid ytState >>= (return . fmap (\x -> YTQueueItem x usr starttime time))

limitedBackoff :: RetryPolicy
limitedBackoff = exponentialBackoff 50 <> limitRetries 5

ytLoop :: BotState -> YTState -> IO ()
ytLoop botState ytState = forever $ do
  waitSong ytState
  x <- takeMVar $ queue ytState
  putMVar (queue ytState) $ SQ.drop 1 x
  if SQ.null x then
    do
    _ <- takeMVar $ play ytState
    putStrLn "Queue started!"
  else
    do
    let rS = shorten 56 (showRestrict (ytmeta $ SQ.index x 0))
    let restr = if null rS then "" else rS ++ "\n"
    sendPacket botState
      $ Send (ytDescription (SQ.index x 0) ++ restr ++ "Next: " ++
              fromMaybe "Nothing" (titleAuthor <$> (safeHeadSeq $ SQ.drop 1 x))) ""
    putStrLn $ "Playing Song! " ++ title (ytmeta $ SQ.index x 0)
    lp <- takeMVar $ lastPlayed ytState
    curTime <- getPOSIXTime
    modifyMVarMasked_ (lastPlayed ytState)  (return . addToBack ((SQ.index x 0) {timePlayed = round curTime}))

waitSong :: YTState -> IO ()
waitSong ytState =
    do
    ct <- getTimeRemaining ytState
    a <- if ct <= 0 then return Nothing else timeout (1000000 * fromIntegral ct) $ takeMVar $ skip ytState
    case a of
      Just False -> waitSong ytState
      _ -> return ()

maybeRead2 :: Read a => String -> Maybe a
maybeRead2 = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

getWaitTimes :: YTQueue -> Integer -> SQ.Seq Integer
getWaitTimes ytList currentWait = SQ.take (SQ.length ytList - 1) $ SQ.scanl (\x y -> x + duration (ytmeta y)) currentWait ytList

getFormattedTime :: Integer -> String
getFormattedTime x = let hours =  div x 3600
                         minutes = div (x-hours*3600) 60
                         seconds = (x - hours*3600 - minutes*60)
                         in if x > 0 then
                             intercalate ":" $ map auxTime [hours,minutes,seconds]
                            else
                             "now"
                      where auxTime :: Integer -> String
                            auxTime x = (if x < 10 then "0" else "") ++ show x

helpIntro :: String -> String
helpIntro botName' =
    "I am @" ++ botName' ++ ", a bot created by viviff for use with rooms with video players.\n\
   \This bot continues the work of NeonDJBot, the original &music bot by Drex."

helpCommands :: String
helpCommands =
   "COMMANDS:\n\
   \‣ Commands are case insensitive and ignore suffixes.\n\
   \‣ Youtube.com links or ytLink's are of the form:\n  youtube.com/watch?v=FTQbiNvZqaY\n or simply the ytid, FTQbiNvZqaY, separated by a space or a comma.\n\
   \‣ Some link shorteners are accepted, like:\n  youtu.be/FTQbiNvZqaY\n\
   \‣ Not accepted in links: playlists or start-times."

helpHelp :: String -> String
helpHelp botName' = "Help:\n• !help @" ++ botName' ++" : This very help."

helpQ :: String
helpQ = "Queue Operation:\n\
   \• !q <ytLink> <ytLink>  [-id or -ytid] (!queue):\n  Queues single or multiple ytLinks at the queue's end.\n\
   \• !qf <ytLink> <ytLink>  [-id or -ytid] (!queuefirst):\n  Same as !q but queues at the start of the queue.\n\
   \• !list [-v or -verbose][-r or -restricted][-id or -ytid][-links][-comma][-space]:\n  Shows a list of the songs currently in the queue,\n\
   \  -verbose adds ytLinks while keeping the titles.\n\
   \  -links and -id show only the links or ids without other info, separated by -comma and/or -space [default]."

helpQAdv :: String
helpQAdv = "Advanced Queue Operation:\n\
   \• !ins <pos> <ytLink> <ytLink>  [-id or -ytid] (!insert):\n  Inserts the song(s) at position <pos>,\n  moving the existing songs down.\n\
   \• !sub <pos> <ytLink>  (!substitute):\n  Substitutes the song at position <pos>\n  with the new ytLink.\n\
   \• !del <pos> <num>  (!delete or !rem, !rm, !remove):\n  Deletes <num> songs from the queue\n  starting from the <pos> position.\n\
   \• !switch <pos1> <pos2> (!swap):\n  Swaps the position of the two songs in the queue."

helpPlay :: String
helpPlay = "Playback Operation:\n\
   \• !skip:\n  Skips the currently playing song,\n  if there is a next song in the queue.\n\
   \• !dskip  (!dramaticskip):\n  Skips in any case, humorously, like the old times :D\n\
   \• !dumpq  (!dumpqueue):\n  Dumps the queue.\n\
   \• !play <ytLink>:\n  If no bots are present, this plays a single song.\n  It interrupts any current song,\n  no link shorteners allowed."

helpCountry :: String
helpCountry = "Country Restrictions:\n\
   \Shows information for the current song, or optionally for one at position <pos>.\n\
   \• !restrict [<pos> or <ytLink>](!restrictions or !restricted):\n  Shows the countries in which the song is not playable.\n\
   \• !allowed [<pos>]:\n  Shows the countries in which the song is playable."

helpExtra :: String
helpExtra = "Extras:\n\
   \• !nls  (!neonlightshow): Light Show!"

helpBot :: String
helpBot = "Bot Operation:\n\
   \• !pause: Pauses the bot, temporarily.\n\
   \• !restore: Restores the bot, from a pause.\n\
   \• !kill: Kills the bot, forever.\n\
   \• !ping: Pong!"

helpIss :: String
helpIss = "Feel free to report a problem here -> https://gitreports.com/issue/MicheleCastrovilli/Euphs\n\
   \See the current status and issues here -> https://github.com/MicheleCastrovilli/Euphs/issues"

helpFun :: String -> String
helpFun botName' = intercalate "\n\n" [helpIntro botName', helpCommands, helpHelp botName',
                                       helpQ, helpQAdv, helpPlay, helpCountry, helpExtra, helpBot, helpIss]

helpFunShort :: String -> String
helpFunShort botName' =
 "◉ :arrow_forward: To play a song: !q <youtube.com link> (now accepts youtu.be !)\n\
 \◉ Use !help @" ++ botName' ++ " for more options ('tab' will auto-complete)"

ytDescription :: YTQueueItem -> String
ytDescription yt = titleAuthorDuration yt ++ "\n!play youtube.com/watch?v=" ++ ytID (ytmeta yt) ++ "\n"

titleAuthor :: YTQueueItem -> String
titleAuthor x = title (ytmeta x) ++ " from [" ++ name (requester x) ++  "]"

titleAuthorDuration :: YTQueueItem -> String
titleAuthorDuration x = "[" ++ getFormattedTime (duration $ ytmeta x) ++  "] " ++ titleAuthor x

safeHead :: [a] -> Maybe a
safeHead x = if null x then
              Nothing
             else
              Just $ head x

safeHeadSeq :: SQ.Seq a -> Maybe a
safeHeadSeq x = if SQ.null x then
                  Nothing
                else
                  Just $ SQ.index x 0

getTimeRemaining :: YTState -> IO Integer
getTimeRemaining ytState =
  do
  lastPlay <- takeMVar (lastPlayed ytState)
  putMVar (lastPlayed ytState) lastPlay
  curTime <- getPOSIXTime
  case SQ.null lastPlay of
    True -> return 0
    False -> let x = SQ.index lastPlay 0 in return $ duration (ytmeta x) - (round curTime - timePlayed x)

getRandomLightShow :: IO String
getRandomLightShow = do
                     randomNum <- getStdRandom (randomR (0, length lightShowlist -1))
                     return (lightShowlist !! randomNum)

dumpQueue :: YTState -> BotState -> MessageID -> IO()
dumpQueue ytState botState mesgID =
  do
  dumped <- takeMVar (queue ytState)
  putMVar (queue ytState) SQ.empty
  sendPacket botState $ Send ("Links : "  ++ F.concatMap (\y -> " youtube.com/watch?v=" ++ ytID (ytmeta y)) dumped) mesgID

listQueue :: YTState -> BotState -> MessageID -> [String] -> IO()
listQueue ytState botState mesgID opts =
  do
  let verbose = "verbose" `elem` opts || "v" `elem` opts
  let restr = "restricted" `elem` opts || "r" `elem` opts
  let ids = "ytid" `elem` opts || "id" `elem` opts
  let links = "links" `elem` opts
  let comma = "comma" `elem` opts
  let space = "space" `elem` opts
  ytSeq <- takeMVar $ queue ytState
  putMVar (queue ytState) ytSeq
  if SQ.null ytSeq then
   sendPacket botState (Send "Nothing Queued!" mesgID)
   else
    do
    timeRemaining <- getTimeRemaining ytState
    sendPacket botState
      (Send (if (ids || links)  && (not verbose) then
             "Queue: " ++ intercalate (if comma then if links || space then ", " else "," else " ")
                (map ((++) (if links then "youtube.com/watch?v=" else "" ) . ytID   . ytmeta) $ F.toList ytSeq)
             else
        "[ # ][ wait  time ]\n" ++
        unlines (F.toList $
          SQ.zipWith (\y z ->
           "[" ++ (if fst y < 10 then " " ++ show (fst y) ++ " " else show (fst y))  ++ "]" ++
           "[  "++ z ++ "  ] \"" ++
           title (ytmeta $ snd y) ++ "\" from [" ++ (name $ requester $ snd y) ++ "]" ++
            (if verbose then
              "\n                     " ++ (if ids then "YTID: " else "Link: youtube.com/watch?v=") ++ ytID (ytmeta $ snd y)
             else
              "") ++
            (let restrict = showRestrict (ytmeta $ snd y) in if restr && (not $ null restrict) then
              "\n                     " ++ shorten 56 restrict
             else
              "")
            )
         (SQ.mapWithIndex (,) ytSeq) $ fmap getFormattedTime $ getWaitTimes ytSeq timeRemaining))
       mesgID)


replaceSong :: BotState -> YTState -> MessageID -> UserData -> String -> String -> IO ()
replaceSong botState ytState mesgID sndUser num ytLink =
  do
  let numR = fromMaybe (-1) (maybeRead2 num :: Maybe Int)
  ytLinkP <- case getYtReq ytLink of
                Just ytLink' -> catch (retrieveRequest ytState ytLink' sndUser 0)
                                  (\ (SomeException e) -> return $ Left $ show e)
                Nothing     -> return $ Left "Not a valid link"
  case ytLinkP of
    Left err -> putStrLn err >> sendPacket botState (Send "Can't parse the link, invalid ids or impossible to contact google api" mesgID)
    Right yt -> do
                ytQ <- takeMVar (queue ytState)
                if numR < 0 || numR > length ytQ  then
                  sendPacket botState (Send "Number not in queue!" mesgID) >> putMVar (queue ytState) ytQ
                else
                  putMVar (queue ytState)  (SQ.update numR yt ytQ) >>
                          sendPacket botState ( Send ("Replaced ["++ num ++"] with : " ++ (title $ ytmeta yt)) mesgID)

skipSong :: YTState -> IO ()
skipSong ytState = do
                   x <- takeMVar (queue ytState)
                   putMVar (queue ytState) x
                   unless (null x) $ putMVar (skip ytState) True

dramaticSkip :: YTState -> BotState -> IO()
dramaticSkip ytState botState =
    do
    thisBot <- readMVar (botAgent botState)
    ytLink <- catch (retrieveRequest ytState ("a1Y73sPHKxw",0) thisBot 0) (\ (SomeException e) -> return $ Left $ show e)
    case ytLink of
      Left  _  -> return ()
      Right yt -> do
                  que <- takeMVar (queue ytState)
                  _ <- tryPutMVar (play ytState) True
                  putMVar (queue ytState) (yt SQ.<| que)
    putMVar (skip ytState) True

queueSongs :: [String] -> BotState -> YTState -> MessageID -> UserData -> Int -> IO ()
queueSongs text =
  let opts = getOpts text
      ids = "ytid" `elem` opts || "id" `elem` opts
  in if ids then
      queueSongsInt (take 100 $ map (\x -> (x,0)) $ filter (\x -> all (\y -> isAlphaNum y || '-' == y || '_' == y) x && length x > 9 ) $ reduceCommas text)
    else
      queueSongsInt (take 100 $ filterLinks $ reduceCommas text)


queueSongsInt :: [YTRequest] -> BotState -> YTState -> MessageID -> UserData -> Int -> IO ()
queueSongsInt (x:xs) bs ytState mesgID sndUser pos=
  do
  threadDelay 500000
  --putStrLn x
  ytData <- catch (retrieveRequest ytState x sndUser 0) (\ (SomeException e) -> return $ Left $ show e)
  case ytData of
    Left err -> do
                putStrLn err
                sendPacket bs (Send "Can't parse the link, invalid ids or impossible to contact google api" mesgID)
                queueSongsInt xs bs ytState mesgID sndUser pos
    Right yt -> if not $ embeddable $ ytmeta yt then
                  sendPacket bs (Send (":warning: Sorry, \"" ++ title (ytmeta yt) ++ "\" is not embeddable.") mesgID ) >>
                  queueSongsInt xs bs ytState mesgID sndUser pos
                else if null $ allowed $ ytmeta yt then
                  sendPacket bs (Send (":warning: Sorry, \"" ++ title (ytmeta yt) ++ "\" is not allowed anywhere.") mesgID ) >>
                  queueSongsInt xs bs ytState mesgID sndUser pos
                else
                  do
                  ytQ <- takeMVar (queue ytState)
                  let posT =  if pos /= -1 then pos else length ytQ + 1
                  let updatedQ = SQ.take (posT-1) ytQ SQ.>< SQ.singleton yt SQ.>< SQ.drop (posT-1) ytQ
                  putMVar (queue ytState) updatedQ
                  timeRemaining <- getTimeRemaining ytState
                  let (_ SQ.:> timeQueued) = SQ.viewr $ SQ.take posT $ getWaitTimes updatedQ timeRemaining
                  void $ tryPutMVar (play ytState) True
                  sendPacket bs (Send ("["++ show posT  ++  "] \""
                                  ++ title (ytmeta yt) ++ "\" will be played " ++
                                  if timeQueued <= 0 then "now"  else  "in " ++ getFormattedTime timeQueued
                                  ++ (let restr = showRestrict $ ytmeta yt  in if null restr then "" else "\n       " ++ shorten 56 restr) )
                                mesgID)
                  queueSongsInt xs bs ytState mesgID sndUser (posT + 1)

queueSongsInt [] _ _ _ _ _ = return ()

insertSongs :: [String] -> BotState -> YTState -> MessageID -> UserData -> String -> IO ()
insertSongs xs bs ytState mesgID sndUser pos =
  case maybeRead2 pos :: Maybe Int of
    Nothing -> queueSongs (pos:xs) bs ytState mesgID sndUser (-1)
    Just posI -> queueSongs xs bs ytState mesgID sndUser posI


deleteSongs :: BotState -> YTState -> MessageID -> UserData -> String -> String -> IO ()
deleteSongs botState ytState mesgID _ pos num =
   do
   let (posI, numI) = (fromMaybe (-1) (maybeRead2 pos :: Maybe Int) , abs $ fromMaybe 1 ( maybeRead2 num :: Maybe Int))
   ytQ <- takeMVar (queue ytState)
   if posI < 0 || posI > SQ.length ytQ  then
      sendPacket botState (Send "Index out of queue range!" mesgID) >> putMVar (queue ytState) ytQ
   else
      putMVar (queue ytState)  (SQ.take (posI -1 ) ytQ SQ.>< SQ.drop (posI - 1 + numI) ytQ ) >>
                          if numI == 1 then
                            sendPacket botState ( Send ("Deleted ["++ pos ++"] -> \"" ++ (title $ ytmeta $ SQ.index ytQ (posI-1) ) ++ "\"") mesgID)
                          else
                            sendPacket botState ( Send ("Deleted from ["++ pos ++"] to ["++ show (posI - 1 +  numI) ++ "]" ++
                              F.concatMap (\n -> "\n\"" ++ (title $ ytmeta $ SQ.index ytQ n) ++ "\"") [(posI-1)..(posI+numI-2)]
                            ) mesgID)

getOpts :: [String] -> [String]
getOpts x =
    map (dropWhile ('-' == )) $ filter (\y -> fromMaybe False (((==) '-') <$> safeHead y)) x

showRestrictions :: YTState -> BotState -> MessageID -> String -> Bool -> IO ()
showRestrictions ytState botState mesgID posR allowed' =
    do
    let pos = maybeRead2 posR :: Maybe Int
    lastPlayedSong <- case pos of
                        Nothing -> fmap safeHeadSeq $ readMVar $ lastPlayed ytState
                        Just x -> do
                                  tq <- readMVar $ queue ytState
                                  return (safeHeadSeq (SQ.drop (x-1) tq))
    let reply = case lastPlayedSong of
                Nothing -> "No song played or out of queue boundaries."
                Just x ->  let restr = if allowed' then showAllowed (ytmeta x) else showRestrict (ytmeta x)  in
                    "["++ fromMaybe "LP" (fmap show pos) ++ "] \"" ++
                    (title $ ytmeta x) ++ "\"\n        " ++ if null restr then "No Restrictions!" else restr
    sendPacket botState $ Send reply mesgID


shorten :: Int -> String -> String
shorten num str = if length str > num then take (num - 3) str ++ "..." else str

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


lightShowlist :: [String]
lightShowlist = ["http://i.imgur.com/eBZO67G.gif", "http://i.imgur.com/0bprD6k.gif",
                 "http://i.imgur.com/van2j15.gif", "http://i.imgur.com/sYjX7Qv.gif",
                 "http://i.imgur.com/sNm4j9n.gif", "http://i.imgur.com/uXSlR5b.gif",
                 "http://i.imgur.com/hGSXbsa.gif", "http://i.imgur.com/UlpqRbK.gif",
                 "http://i.imgur.com/Wmm7EZg.gif", "http://i.imgur.com/QdYSbbA.gif",
                 "http://i.imgur.com/Zy5heqF.gif", "http://i.imgur.com/H4vsVkh.gif"]

showRestrict :: YTMetadata -> String
showRestrict yt
   | null $ allowed yt          = "Allowed Nowhere! Why is this thing even on youtube?"
   | not $ null $ restricted yt = -- if length (restricted yt)  > length (allowed yt) then  "Allowed only in: " ++ (intercalate " - " (allowed yt))
                                   "Restricted in ["++ show (length $ restricted yt) ++ "]: " ++ (intercalate " - " (restricted yt))
   | otherwise                  = ""

showAllowed :: YTMetadata -> String
showAllowed yt
   | not $ null $ restricted yt = "Allowed in ["++ show (length $ allowed yt) ++ "]: " ++ (intercalate " - " (allowed yt))
   | otherwise                  = ""

saveList :: BotState -> YTState -> MessageID -> String -> IO ()
saveList botState ytState mesgID _ = sendPacket botState (Send "Not implemented yet. Sorry ;-;" mesgID)

loadList :: BotState -> YTState -> MessageID -> String -> IO ()
loadList botState ytState mesgID _ = sendPacket botState (Send "Not implemented yet. Sorry ;-;" mesgID)

reduceCommas :: [String] -> [String]
reduceCommas strs =
  concat $ map (splitOn ',') strs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn el lst =
  let a = elemIndex el lst
  in case a of
    Nothing -> lst:[]
    Just i -> let (b,c) = splitAt i lst in b : splitOn el (drop 1 c)


switchSongs :: YTState -> BotState -> MessageID -> [String] -> IO ()
switchSongs ytState botState mesgID x =
  do
  let num1 = fromMaybe (-1) $ safeHead x >>= (\y -> maybeRead2 y ::  Maybe Int)
  let num2 = fromMaybe (-1) $ safeHead (drop 1 x) >>= (\y -> maybeRead2 y ::  Maybe Int)
  curQ <- takeMVar $ queue ytState
  if ( and [num1 <= length curQ, num2 <= length curQ, num1 >= 1, num2 >= 1] ) then
    putMVar (queue ytState) (
    if num1 > num2 then
      seqSwap num2 num1 curQ
    else
      seqSwap num1 num2 curQ
    ) >>
    (sendPacket botState $ Send "Elements switched!" mesgID)
  else
    putMVar (queue ytState) curQ >>
    (sendPacket botState $ Send "Error on parsing the command or index out of ranges. Usage : !switch (or !swap) <pos1> <pos2>" mesgID)

swap :: Int -> Int -> [a] -> [a]
swap n1 n2 xs = zipWith selectElement [1..] xs
  where
    selectElement i x
         | i == n1     = xs !! (n2-1)
         | i == n2     = xs !! (n1-1)
         | otherwise = x

seqSwap :: Int -> Int -> SQ.Seq a -> SQ.Seq a
seqSwap n1 n2 s = SQ.update n2 (SQ.index s n1) $ SQ.update n1 (SQ.index s n2) s

------------------------------------- TAG BOT ---------------------------------------

type YoutubeLink = String
data Tags = Artist String | Theme String | Genre String | Longplay | Note String | SongNick String

data TagState = TagState {
    conn :: MVar SQL.Connection
  , list :: MVar [(YoutubeLink,[Tags])]
}

getTagFunction :: IO TagState
getTagFunction = liftM2 TagState newEmptyMVar newEmptyMVar


tagFunction :: TagState -> BotFunction
tagFunction ts botState (SendEvent message) =
    case (contentMsg message) of
      "!connect" ->  SQL.connectSqlite3 "test1.db" >>= putMVar (conn ts) >> (sendPacket botState $ Send "Connected!" (msgID message))
      "!disconnect" -> takeMVar (conn ts) >>= H.disconnect  >> (sendPacket botState $ Send "Disconnected!" (msgID message))
      _ -> return ()

tagFunction _ _ _ = return ()
