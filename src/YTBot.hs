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

data YTState = YTState {
              queue    :: MVar YTQueue,
              skip     :: MVar Bool,
              play     :: MVar Bool,
              lastPlay :: MVar Integer,
              lastSong :: MVar (Maybe YTMetadata),
              apiKey   :: String,
              noPlay   :: Bool
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

type Requester = String
type YTQueue = [(YTMetadata, Requester)]

apiUrl :: String
apiUrl = "https://content.googleapis.com/youtube/v3/videos?part=snippet%2C+status%2C+contentDetails&id="

apiToken :: String -> String
apiToken apiKeyStr = "&key=" ++ apiKeyStr

getYtFun :: String -> String -> String ->  IO YTState
getYtFun apiKeyStr noplay room =
  do
  !a <- catch (readFile (room ++ "-queue")) (\(SomeException _) -> return "")
  let x = fromMaybe [] (maybeRead2 a :: Maybe YTQueue)
  que <- newMVar x
  skipV <- newEmptyMVar
  playV <- newEmptyMVar
  lastPlayV <- newMVar 0
  lastSongV <- newMVar Nothing
  let noPlay' = fromMaybe False (maybeRead2 noplay :: Maybe Bool)
  return $ YTState que skipV playV lastPlayV lastSongV apiKeyStr noPlay'

ytFunction :: YTState -> BotFunction
ytFunction ytState botState (SendEvent (MessageData time mesgID _ sndUser !content _ _ ))
   = case (let (z:zs) = words content in map toLower z : zs) of
     (stripPrefix "!dramaticskip"  -> Just _) :_      -> dramaticSkip ytState botState
     (stripPrefix "!dskip"         -> Just _) :_      -> dramaticSkip ytState botState
     (stripPrefix "!dumpq"         -> Just _) :_      -> dumpQueue ytState botState mesgID
     (stripPrefix "!queuefirst"    -> Just r) :x      -> queueSongs (r:x) botState ytState mesgID sndUser 1
     (stripPrefix "!restricted"    -> Just _) :x      -> showRestrictions ytState botState mesgID (fromMaybe "" $ safeHead x) False
     (stripPrefix "!allowed"       -> Just _) :x      -> showRestrictions ytState botState mesgID (fromMaybe "" $ safeHead x) True
     (stripPrefix "!qf"            -> Just r) :x      -> queueSongs (r:x) botState ytState mesgID sndUser 1
     (stripPrefix "!q"             -> Just r) :x      -> queueSongs (r:x) botState ytState mesgID sndUser (-1)
     (stripPrefix "!sub"           -> Just _) :n:y:_  -> replaceSong botState ytState mesgID sndUser n y
     (stripPrefix "!ins"           -> Just _) :n:x    -> insertSongs x botState ytState mesgID sndUser n
     (stripPrefix "!del"           -> Just _) :n:x    -> deleteSongs botState ytState mesgID sndUser n $ fromMaybe "1" $ safeHead x
     (stripPrefix "!list"          -> Just _) :x      -> listQueue ytState botState mesgID $ getOpts x
     (stripPrefix "!skip"          -> Just _) :_      -> skipSong ytState
     (stripPrefix "!kill"          -> Just _) :x:_    -> when (filter isAlphaNum x == filter isAlphaNum (botName botState)) (sendPacket botState (Send "Bot is kill." mesgID) >> closeConnection botState True)
     (stripPrefix "!neonlightshow" -> Just _) :_      -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x mesgID)
     (stripPrefix "!nls"           -> Just _) :_      -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x mesgID)
     (stripPrefix "!help"          -> Just _) :x:_    -> when (filter isAlphaNum x == filter isAlphaNum (botName botState))
                                                         $ sendPacket botState $ Send ( helpFun $ botName botState ) mesgID
     (stripPrefix "!help"          -> Just _) :_      -> sendPacket botState $ Send (helpFunShort $ botName botState) mesgID
     (stripPrefix "!switch"        -> Just _) :x      -> switchSongs ytState botState mesgID x
     (stripPrefix "!test"          -> Just r) :_      -> queueSongs [r] botState ytState mesgID sndUser (-1)
     {-
      -(stripPrefix "!vsave"          -> Just _) :x:_    -> saveList botState ytState mesgID x
      -(stripPrefix "!vload"          -> Just _) :x:_    -> loadList botState ytState mesgID x
      -}
     xs -> do
           let playLink = findPlay xs
           unless  ( null playLink ) (
               do
               ytLink <- catch (retrieveYtData (head playLink) ytState) (\ (SomeException e) -> return $ Left $ show e)
               case ytLink of
                 Left _ -> putStrLn "Impossible to parse yt api"
                 Right ytSong -> do
                                 _ <- takeMVar (lastSong ytState)
                                 putMVar  (lastSong ytState) $ Just ytSong
                                 _ <- takeMVar (lastPlay ytState)
                                 putMVar  (lastPlay ytState) time
                                 putMVar  (skip ytState)     False )

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
                catch (retrieveYtData (head $ snd $ head playLink) ytState) (\ (SomeException e) -> return $ Left $ show e)
              else
                return $ Left "No Links Found"

    case ytLink of
      Left _       -> return ()
      Right ytSong -> do
                      _ <- takeMVar $ lastSong ytState
                      putMVar (lastSong ytState) $ Just ytSong
                      _ <- takeMVar $ lastPlay ytState
                      putMVar (lastPlay ytState)
                       (fromMaybe 0 $ (timeRecieved . fst) <$> safeHead playLink)

    ytLoop botState ytState
  )

ytFunction _ _ _ = return ()


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

type YoutubeID = String

getYtID :: String -> Maybe YoutubeID
getYtID y = let x | "youtube.com/watch?" `isInfixOf` y = takeWhile (\z -> isAlphaNum z || z == '_' || z == '-') $ drop 2 $  dropWhile ( 'v' /= ) y
                  | "youtu.be"           `isInfixOf` y = takeWhile (\z -> isAlphaNum z || z == '_' || z == '-') $ drop 4 $  dropWhile ( '.' /=  ) y
                  | otherwise                          = ""
                  in if length x >= 9 then Just x else Nothing

filterLinks :: [String] -> [String]
filterLinks = mapMaybe getYtID

findPlay :: [String] -> [String]
findPlay xs = take 1 $ mapMaybe getYtID $ dropWhile (/="!play") xs

parseISO8601 :: String -> Integer
parseISO8601 x =
  let sec  = readFun 'S'
      min' = readFun 'M'
      hour = readFun 'H'
      in (6 + sec + 60*min' + hour*3600)
  where readFun y = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/=y) $ reverse x ) :: Maybe Integer)

retrieveYtData :: String -> YTState -> IO (Either String YTMetadata)
retrieveYtData ytId ytState = do
  --putStrLn ytId
  ytJson <- simpleHttp $  apiUrl ++ ytId ++ apiToken ( apiKey ytState)
  --B.putStrLn ytJson
  return $ J.eitherDecode ytJson

ytLoop :: BotState -> YTState -> IO ()
ytLoop botState ytState = forever $ do
  waitSong ytState
  x <- takeMVar $ queue ytState
  putMVar (queue ytState) $ drop 1 x
  if null x then
    do
    --putStrLn "Waiting for queue!"
    _ <- takeMVar $ play ytState
    putStrLn "Queue started!"
  else
    do
    --putStrLn "Waiting for current song to finish"
    --putStrLn "Current song should have finished"
    let rS = shorten 56 (showRestrict (fst $ head x))
    let restr = if null rS then "" else rS ++ "\n"
    sendPacket botState
      $ Send (ytDescription (head x) ++ restr ++ "Next: " ++
              fromMaybe "Nothing"
                ((\y -> title (fst y) ++ " from [" ++ snd y ++  "]")  <$> safeHead (tail x))) ""
    putStrLn $ "Playing Song! " ++ title ( fst $ head x)
    _ <- takeMVar $ lastPlay ytState
    curTime <- getPOSIXTime
    putMVar (lastPlay ytState) $ round curTime
    _ <- takeMVar $ lastSong ytState
    putMVar (lastSong ytState) $ Just $ fst $ head x

waitSong :: YTState -> IO ()
waitSong ytState =
    do
    ct <- getTimeRemaining ytState
    --putStrLn ("Waiting for " ++ show ct)
    a <- if ct <= 0 then return Nothing else timeout (1000000 * fromIntegral ct) $ takeMVar $ skip ytState
    case a of
      Just False -> waitSong ytState
      _ -> return ()

maybeRead2 :: Read a => String -> Maybe a
maybeRead2 = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

getWaitTimes :: YTQueue -> Integer -> [Integer]
getWaitTimes ytList currentWait = init $ scanl (\x y -> x + duration (fst y)) currentWait ytList

getFormattedTime :: Integer -> String
getFormattedTime x = let hours =  div x 3600
                         minutes = div (x-hours*3600) 60
                         seconds = (x - hours*3600 - minutes*60)
                         in if x > 0 then
                             (if hours   < 10 then "0" else "") ++ show hours   ++ ":" ++
                             (if minutes < 10 then "0" else "") ++ show minutes ++ ":" ++
                             (if seconds < 10 then "0" else "") ++ show seconds
                            else
                             "now"

helpFun :: String -> String
helpFun botName' =
   "I am @" ++ botName' ++ ", a bot created by viviff for use with rooms with video players.\n\
   \This bot replaces NeonDJBot, the original &music bot by Drex.\n\n\
   \COMMANDS:\n\
   \‣ Commands are case insensitive.\n\
   \‣ Youtube.com links or ytLink's are of the form:\n  youtube.com/watch?v=FTQbiNvZqaY\n or simply the ytid, FTQbiNvZqaY, separated by a space or a comma.\n\
   \‣ Some link shorteners are accepted, like:\n  youtu.be/FTQbiNvZqaY\n\
   \‣ Not accepted in links: playlists or start-times.\n\n\
   \Help:\n\
   \• !help @" ++ botName' ++" : This very help.\n\n\
   \Queue Operation:\n\
   \• !q <ytLink> <ytLink>  [-id or -ytid] (!queue):\n  Queues single or multiple ytLinks at the queue's end.\n\
   \• !qf <ytLink> <ytLink>  [-id or -ytid] (!queuefirst):\n  Same as !q but queues at the start of the queue.\n\
   \• !ins <pos> <ytLink> <ytLink>  [-id or -ytid] (!insert):\n  Inserts the song(s) at position <pos>,\n  moving the existing songs down.\n\
   \• !sub <pos> <ytLink>  (!substitute):\n  Substitutes the song at position <pos>\n  with the new ytLink.\n\
   \• !del <pos> <num>  (!delete):\n  Deletes <num> songs from the queue\n  starting from the <pos> position.\n\
   \• !list [-v or -verbose][-r or -restricted][-id or -ytid][-links][-comma][-space]:\n  Shows a list of the songs currently in the queue,\n\
   \  -verbose adds ytLinks while keeping the titles.\n\
   \  -links and -id show only the links or ids without other info, separated by -comma and/or -space [default].\n\n\
   \Playback Operation:\n\
   \• !skip:\n  Skips the currently playing song,\n  if there is a next song in the queue.\n\
   \• !dskip  (!dramaticskip):\n  Skips in any case, humorously, like the old times :D\n\
   \• !dumpq  (!dumpqueue):\n  Dumps the queue.\n\
   \• !play <ytLink>:\n  If no bots are present, this plays a single song.\n  It interrupts any current song,\n  no link shorteners allowed.\n\n\
   \Country Restrictions:\n\
   \Shows information for the current song, or optionally for one at position <pos>.\n\
   \• !restricted [<pos>]:\n  Shows the countries in which the song is not playable.\n\
   \• !allowed [<pos>]:\n  Shows the countries in which the song is playable.\n\n\
   \Extras:\n\
   \• !nls  (!neonlightshow): Light Show!\n\n\
   \Bot Operation:\n\
   \• !pause: Pauses the bot, temporarily.\n\
   \• !restore: Restores the bot, from a pause.\n\
   \• !kill: Kills the bot, forever.\n\
   \• !ping: Pong!\n"

helpFunShort :: String -> String
helpFunShort botName' =
 "◉ :arrow_forward: To play a song: !q <youtube.com link> (now accepts youtu.be !)\n\
 \◉ Use !help @" ++ botName' ++ " for more options ('tab' will auto-complete)"

ytDescription :: (YTMetadata,String) -> String
ytDescription yt = "[" ++ getFormattedTime (duration $ fst yt) ++  "] " ++
                           title (fst yt) ++ " from [" ++ snd yt ++"]\n" ++
                   "!play youtube.com/watch?v=" ++ ytID (fst yt) ++ "\n"

safeHead :: [a] -> Maybe a
safeHead x = if null x then
              Nothing
             else
              Just $ head x

getTimeRemaining :: YTState -> IO Integer
getTimeRemaining ytState =
  do
  lastPlayedTime <- takeMVar (lastPlay ytState)
  putMVar (lastPlay ytState) lastPlayedTime
  curTime <- getPOSIXTime
  lastSongPlayed <- takeMVar $ lastSong ytState
  putMVar (lastSong ytState) lastSongPlayed
  case lastSongPlayed of
    Nothing -> return 0
    Just x -> return $ duration x - (round curTime - lastPlayedTime)

getRandomLightShow :: IO String
getRandomLightShow = do
                     randomNum <- getStdRandom (randomR (0, length lightShowlist -1))
                     return (lightShowlist !! randomNum)



dumpQueue :: YTState -> BotState -> MessageID -> IO()
dumpQueue ytState botState mesgID =
  do
  dumped <- takeMVar (queue ytState)
  putMVar (queue ytState) []
  sendPacket botState $ Send ("Links : "  ++ concatMap (\y -> " youtube.com/watch?v=" ++ ytID (fst y)) dumped) mesgID

listQueue :: YTState -> BotState -> MessageID -> [String] -> IO()
listQueue ytState botState mesgID opts =
  do
  let verbose = "verbose" `elem` opts || "v" `elem` opts
  let restr = "restricted" `elem` opts || "r" `elem` opts
  let ids = "ytid" `elem` opts || "id" `elem` opts
  let links = "links" `elem` opts
  let comma = "comma" `elem` opts
  let space = "space" `elem` opts
  ytList <- takeMVar $ queue ytState
  putMVar (queue ytState) ytList
  if null ytList then
   sendPacket botState (Send "Nothing Queued!" mesgID)
   else
    do
    timeRemaining <- getTimeRemaining ytState
    sendPacket botState
      (Send (if (ids || links)  && (not verbose) then
             "Queue: " ++ intercalate (if comma then if links || space then ", " else "," else " ")
                (map ((++) (if links then "youtube.com/watch?v=" else "" ) . ytID   . fst) ytList)
             else
        "[ # ][ wait  time ]\n" ++
        unlines (
          zipWith3 (\x y z ->
           "[" ++ (if x < 10 then " " ++ show x ++ " " else show x)  ++ "]" ++
           "[  "++ z ++ "  ] \"" ++
           title (fst y) ++ "\" from [" ++ snd y ++ "]" ++
            (if verbose then
              "\n                     " ++ (if ids then "YTID: " else "Link: youtube.com/watch?v=") ++ ytID (fst y)
             else
              "") ++
            (let restrict = showRestrict (fst y) in if restr && (not $ null restrict) then
              "\n                     " ++ shorten 56 restrict
             else
              "")
            )
         ([1..]::[Int]) ytList $ map getFormattedTime $ getWaitTimes ytList timeRemaining))
       mesgID)


replaceSong :: BotState -> YTState -> MessageID -> UserData -> String -> String -> IO ()
replaceSong botState ytState mesgID sndUser num ytLink =
  do
  let numR = fromMaybe (-1) (maybeRead2 num :: Maybe Int)
  let ytLinkID = fromMaybe "" $ getYtID ytLink
  ytLinkP <- catch (retrieveYtData ytLinkID ytState) (\ (SomeException e) -> return $ Left $ show e)
  case ytLinkP of
    Left err -> putStrLn err >> sendPacket botState (Send "Can't parse the link, invalid ids or impossible to contact google api" mesgID)
    Right yt -> do
                ytQ <- takeMVar (queue ytState)
                if numR < 0 || numR > length ytQ  then
                  sendPacket botState (Send "Number not in queue!" mesgID) >> putMVar (queue ytState) ytQ
                else
                  putMVar (queue ytState)  (take (numR -1) ytQ ++ [(yt, name sndUser)] ++ drop numR ytQ ) >>
                          sendPacket botState ( Send ("Replaced ["++ num ++"] with : " ++ title yt) mesgID)

skipSong :: YTState -> IO ()
skipSong ytState = do
                   x <- takeMVar (queue ytState)
                   putMVar (queue ytState) x
                   unless (null x) $ putMVar (skip ytState) True

dramaticSkip :: YTState -> BotState -> IO()
dramaticSkip ytState botState =
    do
    ytLink <- catch (retrieveYtData "a1Y73sPHKxw" ytState) (\ (SomeException e) -> return $ Left $ show e)
    case ytLink of
      Left  _  -> return ()
      Right yt -> do
                  que <- takeMVar (queue ytState)
                  _ <- tryPutMVar (play ytState) True
                  putMVar (queue ytState) ((yt,botName botState):que)
    putMVar (skip ytState) True

queueSongs :: [String] -> BotState -> YTState -> MessageID -> UserData -> Int -> IO ()
queueSongs text =
  let opts = getOpts text
      ids = "ytid" `elem` opts || "id" `elem` opts
  in if ids then
      queueSongsInt (filter (\x -> all (\y -> isAlphaNum y || '-' == y || '_' == y) x && length x > 9 ) $ reduceCommas text)
    else
      queueSongsInt (filterLinks $ reduceCommas text)


queueSongsInt :: [String] -> BotState -> YTState -> MessageID -> UserData -> Int -> IO ()
queueSongsInt (x:xs) bs ytState mesgID sndUser pos=
  do
  threadDelay 1000000
  --putStrLn x
  ytData <- catch (retrieveYtData x ytState) (\ (SomeException e) -> return $ Left $ show e)
  case ytData of
    Left err -> do
                putStrLn err
                sendPacket bs (Send "Can't parse the link, invalid ids or impossible to contact google api" mesgID)
                queueSongsInt xs bs ytState mesgID sndUser pos
    Right yt -> if not $ embeddable yt then
                  sendPacket bs (Send (":warning: Sorry, \"" ++ title yt ++ "\" is not embeddable.") mesgID ) >>
                  queueSongsInt xs bs ytState mesgID sndUser pos
                else if null $ allowed yt then
                  sendPacket bs (Send (":warning: Sorry, \"" ++ title yt ++ "\" is not allowed anywhere.") mesgID ) >>
                  queueSongsInt xs bs ytState mesgID sndUser pos
                else
                  do
                  ytQ <- takeMVar (queue ytState)
                  let posT =  if pos /= -1 then pos else length ytQ + 1
                  let updatedQ = take (posT-1) ytQ ++ (yt,name sndUser): drop (posT-1) ytQ
                  putMVar (queue ytState) updatedQ
                  timeRemaining <- getTimeRemaining ytState
                  let timeQueued = last $ take posT $ getWaitTimes updatedQ timeRemaining
                  void $ tryPutMVar (play ytState) True
                  sendPacket bs (Send ("["++ show posT  ++  "] \""
                                  ++ title yt ++ "\" will be played " ++
                                  if timeQueued <= 0 then "now"  else  "in " ++ getFormattedTime timeQueued
                                  ++ (let restr = showRestrict yt  in if null restr then "" else "\n       " ++ shorten 56 restr) )
                                mesgID)
                  queueSongsInt xs bs ytState mesgID sndUser (posT + 1)

queueSongsInt [] _ _ _ _ _ = return ()

insertSongs :: [String] -> BotState -> YTState -> MessageID -> UserData -> String -> IO ()
insertSongs xs bs ytState mesgID sndUser pos =
  case getYtID pos of
    Nothing -> queueSongs xs bs ytState mesgID sndUser $ fromMaybe (-1) (maybeRead2 pos :: Maybe Int)
    Just x ->  queueSongs (x:xs) bs ytState mesgID sndUser (-1)


deleteSongs :: BotState -> YTState -> MessageID -> UserData -> String -> String -> IO ()
deleteSongs botState ytState mesgID _ pos num =
   do
   let (posI, numI) = (fromMaybe (-1) (maybeRead2 pos :: Maybe Int) , abs $ fromMaybe 1 ( maybeRead2 num :: Maybe Int))
   ytQ <- takeMVar (queue ytState)
   if posI < 0 || posI > length ytQ  then
      sendPacket botState (Send "Index out of queue range!" mesgID) >> putMVar (queue ytState) ytQ
   else
      putMVar (queue ytState)  (take (posI -1 ) ytQ ++ drop (posI - 1 + numI) ytQ ) >>
                          if numI == 1 then
                            sendPacket botState ( Send ("Deleted ["++ pos ++"] -> \"" ++ (title $ fst $ ytQ !! (posI-1) ) ++ "\"") mesgID)
                          else
                            sendPacket botState ( Send ("Deleted from ["++ pos ++"] to ["++ show (posI - 1 +  numI) ++ "]" ++
                              concatMap (\n -> "\n\"" ++ (title $ fst $ ytQ !! n) ++ "\"") [(posI-1)..(posI+numI-2)]
                            ) mesgID)

getOpts :: [String] -> [String]
getOpts x =
    map (dropWhile ('-' == )) $ filter (\y -> fromMaybe False (((==) '-') <$> safeHead y)) x

showRestrictions :: YTState -> BotState -> MessageID -> String -> Bool -> IO ()
showRestrictions ytState botState mesgID posR allowed=
    do
    let pos = maybeRead2 posR :: Maybe Int
    lastPlayedSong <- case pos of
                        Nothing -> readMVar $ lastSong ytState
                        Just x -> do
                                  tq <- readMVar $ queue ytState
                                  return (fst <$> safeHead (drop (x-1) tq))
    let reply = case lastPlayedSong of
                Nothing -> "No song played or out of queue boundaries."
                Just x ->  let restr = if allowed then showAllowed x else showRestrict x  in
                             if null restr then "No Restrictions!" else restr
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
      swap num2 num1 curQ
    else
      swap num1 num2 curQ
    ) >>
    (sendPacket botState $ Send "Elements switched!" mesgID)
  else
    putMVar (queue ytState) curQ >>
    (sendPacket botState $ Send "Error on parsing the command or index out of ranges. Usage : !switch <pos1> <pos2>" mesgID)

swap :: Int -> Int -> [a] -> [a]
swap n1 n2 xs = zipWith selectElement [1..] xs
  where
    selectElement i x
         | i == n1     = xs !! n2
         | i == n2     = xs !! n1
         | otherwise = x

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
