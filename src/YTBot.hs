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
import           Control.Monad      (forever, mzero, void, when, unless )
import           Data.List
import           Network.HTTP.Conduit
import           Data.Char
import           Data.Maybe
import           System.Timeout
import           Data.Time.Clock.POSIX
import           Control.Exception
import           System.Random
import           Data.Function

data YTState = YTState {
              queue    :: MVar YTQueue,
              skip     :: MVar Bool,
              play     :: MVar Bool,
              lastPlay :: MVar Integer,
              lastSong :: MVar (Maybe YTMetadata),
              apiKey   :: String
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

getYtFun :: String -> IO BotFunction
getYtFun apiKeyStr =
  do
  !a <- readFile "tmpQueue"
  let x = fromMaybe [] (maybeRead2 a :: Maybe YTQueue)
  que <- newMVar x
  skipV <- newEmptyMVar
  playV <- newEmptyMVar
  lastPlayV <- newMVar 0
  lastSongV <- newMVar Nothing
  return $ ytFunction $ YTState que skipV playV lastPlayV lastSongV apiKeyStr

ytFunction :: YTState -> BotFunction
ytFunction ytState botState (SendEvent (MessageData time mesgID _ sndUser !content _ _ ))
   = case (let (z:zs) = words content in map toLower z : zs) of
     (stripPrefix "!vdramaticskip"  -> Just _) :_      -> dramaticSkip ytState botState
     (stripPrefix "!vds"            -> Just _) :_      -> dramaticSkip ytState botState
     (stripPrefix "!vdump"          -> Just _) :_      -> dumpQueue ytState botState mesgID
     (stripPrefix "!vqueuefirst"    -> Just r) :x      -> queueSongs (filter (/="") $  map getYtID $ r:x) botState ytState mesgID sndUser 1
     (stripPrefix "!vrestrictions"  -> Just _) :_      -> showRestrictions ytState botState mesgID
     (stripPrefix "!vqf"            -> Just r) :x      -> queueSongs (filter (/="") $  map getYtID $ r:x) botState ytState mesgID sndUser 1
     (stripPrefix "!vq"             -> Just r) :x      -> queueSongs (filter (/="") $  map getYtID $ r:x) botState ytState mesgID sndUser (-1)
     (stripPrefix "!vr"             -> Just _) :n:y:_  -> replaceSong botState ytState mesgID sndUser n y
     (stripPrefix "!vi"             -> Just _) :n:x    -> insertSongs (filter (/="") $  map getYtID x) botState ytState mesgID sndUser n
     (stripPrefix "!vd"             -> Just _) :n:x    -> deleteSongs botState ytState mesgID sndUser n $ fromMaybe "1" $ safeHead x
     (stripPrefix "!vlist"          -> Just _) :x      -> listQueue ytState botState mesgID $ getOpts x
     (stripPrefix "!vskip"          -> Just _) :_      -> skipSong ytState
     (stripPrefix "!vkill"          -> Just _) :_      -> closeConnection botState
     (stripPrefix "!vneonlightshow" -> Just _) :_      -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x mesgID)
     (stripPrefix "!vnls"           -> Just _) :_      -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x mesgID)
     (stripPrefix "!help"           -> Just _) :x:_    -> when (filter isAlphaNum x == filter isAlphaNum (botName botState))
                                                          $ sendPacket botState $ Send ( helpFun $ botName botState ) mesgID
     (stripPrefix "!vhelp"          -> Just _) :_      -> sendPacket botState $ Send ( helpFun $ botName botState ) mesgID
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

  _ <- forkIO (do
          _ <- readMVar (closedBot botState)
          ytq <- takeMVar (queue ytState)
          writeFile "tmpQueue" $ show ytq
          )
  ytLoop botState ytState

ytFunction _ _ _ = return ()


instance J.FromJSON YTMetadata where
  parseJSON (J.Object v) = do
    tmp <-  safeHead <$> v J..: "items"
    case tmp of
      Nothing -> mzero
      Just ytl -> do
                  snippet <- ytl J..: "snippet"
                  YTMetadata <$> ( ytl J..: "id" )
                             <*> ( snippet J..: "title" )
                             <*> ( snippet J..: "thumbnails" >>= (J..: "default") >>= (J..: "url"))
                             <*> ( parseISO8601 <$> ( ytl J..: "contentDetails" >>= (J..: "duration")))
                             <*> ( ytl J..: "contentDetails" >>= (J..: "duration"))
                             <*> ( ytl J..: "status" >>= (J..: "embeddable"))
                             <*> ((ytl J..: "contentDetails"  >>=  (J..: "regionRestriction") >>=  (J..: "blocked")) <|> return [])
                             <*> ((ytl J..: "contentDetails"  >>=  (J..: "regionRestriction") >>=  (J..: "allowed")) <|> return [])
  parseJSON _ = mzero

getYtID :: String -> String
getYtID y = let x | "youtube.com/watch?" `isInfixOf` y = takeWhile (\z -> isAlphaNum z || z == '_' || z == '-') $ drop 2 $  dropWhile ( 'v' /= ) y
                  | "youtu.be"           `isInfixOf` y = takeWhile (\z -> isAlphaNum z || z == '_' || z == '-') $ drop 4 $  dropWhile ( '.' /=  ) y
                  | otherwise                          = ""
            in if length x >= 10 then x else ""


findPlay :: [String] -> [String]
findPlay xs = take 1 $ dropWhile (== "") $ map getYtID $ dropWhile (/="!play") xs

parseISO8601 :: String -> Integer
parseISO8601 x =
  let revS = reverse x
      sec  = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='S') revS ) :: Maybe Integer)
      min'  = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='M') revS ) :: Maybe Integer)
      hour = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='H') revS ) :: Maybe Integer)
      in (6 + sec + 60*min' + hour*3600)

retrieveYtData :: String -> YTState -> IO (Either String YTMetadata)
retrieveYtData ytId ytState = do
  ytJson <- simpleHttp $  apiUrl ++ ytId ++ apiToken ( apiKey ytState)
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
    sendPacket botState
      $ Send (ytDescription (head x) ++ "Next: " ++ fromMaybe "Nothing"
                        ((\y -> title (fst y) ++ " from [" ++ snd y ++  "]")  <$> safeHead (tail x)) ++
                        if not $ null $ restricted $ fst $ head x  then
                          "\nRestricted: " ++ shorten 20 (intercalate " - " (restricted $ fst $ head x))
                        else if not $ null $ allowed $ fst $ head x then
                          "\nAllowed: " ++ shorten 20 (intercalate " - " (allowed $ fst $ head x))
                        else"") ""
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
   "I am @" ++ botName' ++ ", a bot created by viviff for use with rooms with video players.\n" ++
   "This bot replaces NeonDJBot, the original &music bot by Drex.\n" ++
   "Accepts also youtu.be links. Does not accept playlists or start times.\n\n" ++
   "COMMANDS: \n"++
   "• !help @"++ botName' ++" (!vhelp)\n  This very help. Commands are also case insensitive.\n\n" ++
   "Queue Operators:\n" ++
   "• !vq <ytLink>... <ytLink> (!vqueue)\n  Queues multiple youtube links at the end of the queue, separated by spaces.\n" ++
   "• !vqf <ytLink> ... <ytLink> (!vqueuefirst)\n  Same thing as !vq but queues at the start of the queue.\n" ++
   "• !vi <pos> <ytLink> ... <ytLink> (!vinsert)\n  Inserts the song(s) at position <pos>, moving the existing songs down.\n" ++
   "• !vr <pos> <ytLink> (!vreplace)\n  Replaces the song at position <pos> with the new ytLink. \n" ++
   "• !vd <pos> <num> (!vdelete)\n  Removes <num> songs from the queue starting from the <pos> position.\n" ++
   "• !vlist [-v (-verbose)]\n  Shows a list of the songs currently in the queue. Verbose adds links.\n\n" ++
   "• !vskip\n  Skips the currently playing song.\n" ++
   "• !vds (!vdramaticskip or !vdskip)\n  Skips no matter what, humorously, like the old times :D\n" ++
   "• !vdump (!vdumpqueue)\n  Dumps the queue.\n" ++
   "• !play <ytLink>\n  If no bots are present, use this to play a single song.\n  It interrupts any current song, no link shorteners allowed.\n" ++
   "Extras:\n" ++
   "• !vnls (!vneonlightshow) : Light Show!\n\n" ++
   "Bot Operation:\n" ++
   "• !vkill : Kills the bot, forever\n" ++
   "• !ping : Pong!\n"

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
  let links = "verbose" `elem` opts || "v" `elem` opts
  ytList <- takeMVar $ queue ytState
  putMVar (queue ytState) ytList
  if null ytList then
   sendPacket botState (Send "Nothing Queued!" mesgID)
   else
    do
    timeRemaining <- getTimeRemaining ytState
    sendPacket botState
      (Send ("[ # ][ wait  time ]\n" ++
        unlines (
          zipWith3 (\x y z ->
           "[" ++ (if x < 10 then " " ++ show x ++ " " else show x)  ++ "]" ++
           "[  "++ z ++ "  ]" ++
           " \"" ++ title (fst y) ++
           "\" from [" ++ snd y ++ "]" ++ if links then "\n                     Link: youtube.com/watch?v=" ++ ytID (fst y) else "")
         ([1..]::[Int]) ytList $ map getFormattedTime $ getWaitTimes ytList timeRemaining))
       mesgID)


replaceSong :: BotState -> YTState -> MessageID -> UserData -> String -> String -> IO ()
replaceSong botState ytState mesgID sndUser num ytLink =
  do
  let numR = fromMaybe (-1) (maybeRead2 num :: Maybe Int)
  let ytLinkID = getYtID ytLink
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
queueSongs (x:xs) bs ytState mesgID sndUser pos =
  do
  threadDelay 1000000
  ytData <- catch (retrieveYtData x ytState) (\ (SomeException e) -> return $ Left $ show e)
  case ytData of
    Left err -> do
                putStrLn err
                sendPacket bs (Send "Can't parse the link, invalid ids or impossible to contact google api" mesgID)
                queueSongs xs bs ytState mesgID sndUser pos
    Right yt -> if embeddable yt then
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
                                  if timeQueued <= 0 then "now"  else  "in " ++ getFormattedTime timeQueued  )
                                mesgID)
                  queueSongs xs bs ytState mesgID sndUser (posT + 1)
                else
                  sendPacket bs (Send ("Sorry, \"" ++ title yt ++ "\" is not embeddable.") mesgID ) >>
                  queueSongs xs bs ytState mesgID sndUser pos

queueSongs [] _ _ _ _ _ = return ()

insertSongs :: [String] -> BotState -> YTState -> MessageID -> UserData -> String -> IO ()
insertSongs xs bs ytState mesgID sndUser pos =
  case getYtID pos of
    "" -> queueSongs xs bs ytState mesgID sndUser $ fromMaybe (-1) (maybeRead2 pos :: Maybe Int)
    x ->  queueSongs (x:xs) bs ytState mesgID sndUser (-1)


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
                            sendPacket botState ( Send ("Deleted ["++ pos ++"]") mesgID)
                          else
                            sendPacket botState ( Send ("Deleted from ["++ pos ++"] to ["++ show (posI - 1 +  numI) ++ "]") mesgID)

getOpts :: [String] -> [String]
getOpts x =
    map (dropWhile ('-' == )) $ filter ((==) '-' . head) x

showRestrictions :: YTState -> BotState -> MessageID -> IO ()
showRestrictions ytState botState mesgID =
    do
    lastPlayedSong <- readMVar $ lastSong ytState
    let reply = case lastPlayedSong of
                Nothing -> "No song played."
                Just x | not $ null $ allowed x -> "Allowed: " ++ intercalate " - " (allowed x)
                       | not $ null $ restricted x -> "Restricted: " ++ intercalate " - " (restricted x)
                       | otherwise -> "No restrictions!"
    sendPacket botState $ Send reply mesgID


shorten :: Int -> String -> String
shorten num str = if length str > num then take (num - 3) str ++ "..." else str

countries :: [String]
countries = ["MF","AQ","WS","HK","IQ","MH","HN","HM","WF","HR","NL","HT","HU","GB","GA",
             "GG","GF","GE","GD","GI","GH","GN","GM","GL","GS","GR","GQ","GP","GW","GU",
             "GT","GY","RU","RW","SB","RS","RO","NF","OM","RE","NG","MM","BL","BM","BN",
             "BO","BH","BI","BJ","BD","BE","BF","BG","BA","BB","BY","BZ","BT","ZM","BV",
             "BW","BR","BS","JE","QA","JM","JO","JP","MO","VE","YT","KP","MP","YE","AG",
             "PF","TV","TW","TT","IN","TR","AS","AR","IE","ID","TZ","AX","AZ","AE","AD",
             "TD","AF","TC","TN","IT","TL","TM","AI","TK","IS","IR","LU","DO","DM","DJ",
             "DK","SR","ZW","TO","DZ","SZ","SY","LY","LV","SV","LT","ST","LR","LS","SO",
             "SN","SM","SL","SK","LK","SI","LI","SG","SE","SD","LB","LC","SA","LA","CO",
             "AU","CM","CL","CK","CI","AT","CG","CF","SH","CC","AW","CA","TJ","CY","CX",
             "CV","CU","CR","MD","KG","KE","VU","KN","KM","IL","KI","KH","KW","VA","VC",
             "KR","VG","VI","KZ","VN","ZA","CN","FI","FJ","FK","FM","FO","TH","FR","KY",
             "MN","UY","TF","NZ","UZ","TG","NP","US","CH","NU","MK","NI","UM","NO","UA",
             "NA","IM","NC","SJ","NE","UG","PW","EH","MZ","CD","MW","AO","EC","EE","EG",
             "AM","SC","AL","IO","ES","ER","ET","MA","MC","PA","ME","PG","MG","PE","PK",
             "PH","PN","ML","PL","PM","MQ","PS","MS","MR","MU","MT","PT","MV","MY","MX",
             "PR","PY","NR","CZ","DE"]

lightShowlist :: [String]
lightShowlist = ["http://i.imgur.com/eBZO67G.gif", "http://i.imgur.com/0bprD6k.gif",
                 "http://i.imgur.com/van2j15.gif", "http://i.imgur.com/sYjX7Qv.gif",
                 "http://i.imgur.com/sNm4j9n.gif", "http://i.imgur.com/uXSlR5b.gif",
                 "http://i.imgur.com/hGSXbsa.gif", "http://i.imgur.com/UlpqRbK.gif",
                 "http://i.imgur.com/Wmm7EZg.gif", "http://i.imgur.com/QdYSbbA.gif",
                 "http://i.imgur.com/Zy5heqF.gif", "http://i.imgur.com/H4vsVkh.gif"]
