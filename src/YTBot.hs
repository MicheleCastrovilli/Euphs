{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module YTBot where

import qualified Data.Aeson as J
import           Euphoria.Bot
import           Euphoria.Events
import           Euphoria.Types
import           Euphoria.Commands
import           Control.Concurrent
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
  restricted :: String
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
ytFunction ytState botState (SendEvent (MessageData time msgID parentMsg sender !content _ _ ))
   =  case (let (x:xs) = words content in map toLower x : xs) of 
      "!vq"  : x  -> queueSongs x botState ytState msgID sender False
      "!vqueue"  : x  -> queueSongs x botState ytState msgID sender False
      "!vqf" : x  -> queueSongs x botState ytState msgID sender True
      "!vqueuefirst" : x  -> queueSongs x botState ytState msgID sender True
      "!vr"  : num : ytLink:_ ->  replaceSong botState ytState msgID sender num ytLink
      "!vreplace"  : num : ytLink:_ ->  replaceSong botState ytState msgID sender num ytLink
      "!vlist" : x -> listQueue ytState botState msgID
      "!vskip" : x -> skipSong ytState
      "!vdump"      :x  -> dumpQueue ytState botState msgID
      "!vdumpqueue" :x  -> dumpQueue ytState botState msgID
      "!vkill":x  -> closeConnection botState 
      "!vdramaticskip":_ -> dramaticSkip ytState botState
      "!vneonlightshow":_ -> getRandomLightShow >>= (\x -> sendPacket botState $ Send x msgID)
      "!help" : x : _ -> when (("@" ++ filter isAlphaNum x) == ("@" ++ (filter isAlphaNum $ botName botState))) 
                                    $ sendPacket botState $ Send ( helpFun $ botName botState ) msgID
      "!vhelp": _ -> sendPacket botState $ Send ( helpFun $ botName botState ) msgID 
      
      xs -> do
            let playLink = findPlay xs
            unless  ( null playLink ) (
                do
                {-sendPacket botState $ Send "Found a play command, i now wait for the song to end" msgID-}
                ytLink <- catch (retrieveYtData (head playLink) ytState) (\ (SomeException e) -> return $ Left $ show e)
                case ytLink of 
                  Left e -> putStrLn "Impossible to parse yt api"
                  Right ytSong -> do
                                  takeMVar (lastSong ytState) 
                                  putMVar  (lastSong ytState) $ Just ytSong
                                  takeMVar (lastPlay ytState)
                                  putMVar  (lastPlay ytState) time
                                  putMVar  (skip ytState)     False )

      _ -> return () 

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
    Left err     -> return ()
    Right ytSong -> do
                    takeMVar $ lastSong ytState
                    putMVar (lastSong ytState) $ Just ytSong
                    takeMVar $ lastPlay ytState
                    putMVar (lastPlay ytState) 
                     (fromMaybe 0 $ (timeRecieved . fst) <$> safeHead playLink)

  forkIO (do
          readMVar (closedBot botState)
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
                             <*>   return ""  -- ( ytl J..: "contentDetails"  >>=  (J..:? "regionRestriction") J..!= mzero >>=  (J..:? "blocked")  J..!= "")

isYtLink :: String -> Bool
isYtLink x = isInfixOf "youtube.com/watch?" x || isInfixOf "youtu.be/" x

getYtID :: String -> String
getYtID = takeWhile (\x -> isAlphaNum x || x == '_') . drop 2 . dropWhile  ( 'v' /= )

queueSongs :: [String] -> BotState -> YTState -> MessageID -> UserData -> Bool -> IO ()
queueSongs x botState ytState msgID sender first = 
    mapM_ (\x -> queueSong x botState msgID ytState first $ name sender) 
       $ (if first then reverse else id ) $ map getYtID $ filter isYtLink x



findPlay :: [String] -> [String] 
findPlay xs = map getYtID $ take 1 $ dropWhile (not . isYtLink) $ dropWhile (/="!play") xs

parseISO8601 :: String -> Integer
parseISO8601 x = 
  let revS = reverse x
      sec  = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='S') revS ) :: Maybe Integer)
      min  = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='M') revS ) :: Maybe Integer)
      hour = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='H') revS ) :: Maybe Integer)
      in (6 + sec + 60*min + hour*3600)

retrieveYtData :: String -> YTState -> IO (Either String YTMetadata)
retrieveYtData id ytState = do 
  ytJson <- simpleHttp $  apiUrl ++ id ++ apiToken ( apiKey ytState)
  return $ J.eitherDecode ytJson 

 -- x is the ytd id
queueSong :: String -> BotState -> String -> YTState -> Bool -> String -> IO()
queueSong x bs idRepl ytState first requester = 
  do
  threadDelay 1000000
  ytData <- catch (retrieveYtData x ytState) (\ (SomeException e) -> return $ Left $ show e)
  case ytData of
    Left err -> do
                putStrLn err
                sendPacket bs (Send "Can't parse the link, invalid ids or impossible to contact google api" idRepl)
    Right yt -> if embeddable yt then
                  do
                  ytQ <- takeMVar (queue ytState)
                  let updatedQ = if first then (yt,requester):ytQ else ytQ ++ [(yt,requester)]
                  putMVar (queue ytState) updatedQ
                  let pos =  if first then 1 else length ytQ + 1
                  timeRemaining <- getTimeRemaining ytState
                  let timeQueued = last $ take pos $ getWaitTimes updatedQ timeRemaining
                  void $ tryPutMVar (play ytState) True
                  sendPacket bs (Send ("["++ show pos  ++  "] \""
                                  ++ title yt ++ "\" will be played " ++ 
                                  if timeQueued <= 0 then "now"  else  "in " ++ getFormattedTime timeQueued  ) 
                                idRepl)
                else
                  sendPacket bs $ Send ("Sorry, \"" ++ title yt ++ "\" is not embeddable.") idRepl

{-queueSongList :: [IO (Either String YTMetadata)] -> BotState -> String -> YTState -> String -> Int -> IO ()-}
{-queueSongList ioList@(_:_) bs idRepl ytState requester pos = -}
  {-do-}
  {-mapM (\x -> putStrLn x >> -}
               {-sendPacket bs (Send "Can't parse the link, invalid ids or impossible to contact google api" idRepl)-}
             {-) (lefts list)-}
  {-ytQueue <- takeMVar (queue ytState)-}
  {-let songs = map (\y -> (y, requester )) (rights list)-}
  {-putMVar (queue ytState) (songs ++ ytQueue)-}
  {-timeRemaining <- getTimeRemaining ytState-}
  {-let timeQueue = getWaitTimes (songs ++ ytQueue) timeRemaining-}
  {-mapM_ ( \x -> sendPacket bs (Send x idRepl) )-}
     {-(zipWith3 (\yt timeQueued pos -> "["++ show pos  ++  "] \"" ++ title yt ++ -}
                {-"\" will be played " ++ -}
                {-if(timeQueued <= 0) then "now" -}
                {-else ("in " ++ getFormattedTime timeQueued ))-}
          {-(rights list) timeQueue [1..])-}
                          

 
queueSongList _ bs idRepl ytState requester pos = 
    sendPacket bs (Send "Couldn't parse any link" idRepl)


ytLoop botState ytState = forever $ do
  waitSong ytState
  x <- takeMVar $ queue ytState
  putMVar (queue ytState) $ drop 1 x
  if null x then
    do
    --putStrLn "Waiting for queue!"
    takeMVar $ play ytState
    putStrLn "Queue started!"
  else
    do
    --putStrLn "Waiting for current song to finish"
    --putStrLn "Current song should have finished"
    sendPacket botState 
      $ Send (ytDescription (head x) ++ "Next: " ++ fromMaybe "Nothing" 
                        ((\x -> title (fst x) ++ " from [" ++ snd x ++  "]")  <$> (safeHead $ tail x ))) ""
    putStrLn $ "Playing Song! " ++ title ( fst $ head x)
    takeMVar $ lastPlay ytState
    curTime <- getPOSIXTime
    putMVar (lastPlay ytState) $ round curTime
    takeMVar $ lastSong ytState
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
getWaitTimes ytList currentWait = init $ scanl (\x y -> x + (duration $ fst y)) currentWait ytList

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
helpFun botName = 
   "I am @" ++ botName ++ ", a bot created by viviff.\n" ++
   "This bot is a replacement for NeonDJBot since it has been missing lately.\n\n" ++
   "Commands: \n!vq <ytLink>... <ytLink> -> Queues all the youtube links found in the message at the end of the queue.\n" ++
   "!vqf <ytLink> ... <ytLink> -> Same thing as !vq but queues at the start of the queue.\n" ++
   "!vlist -> Shows a list of the songs currently in the queue.\n" ++
   "!vskip -> Skips the currently playing song. \n" ++
   "!vdramaticskip -> Skips like the old times :D\n" ++
   "!vr <num> <ytLink> -> Replaces the song at position <num> with the new ytLink. \n" ++
   "!vdump -> Dumps the queue\n" ++
   "!vkill -> Kills the bot, forever\n" ++
   "!ping  -> Pong!\n" ++
   "!vneonlightshow -> Light Show!\n" ++
   "!help @"++ botName ++" -> This very help."

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

lightShowlist :: [String]
lightShowlist = ["http://i.imgur.com/eBZO67G.gif", "http://i.imgur.com/0bprD6k.gif",
                 "http://i.imgur.com/van2j15.gif", "http://i.imgur.com/sYjX7Qv.gif",
                 "http://i.imgur.com/sNm4j9n.gif", "http://i.imgur.com/uXSlR5b.gif",
                 "http://i.imgur.com/hGSXbsa.gif", "http://i.imgur.com/UlpqRbK.gif",
                 "http://i.imgur.com/Wmm7EZg.gif", "http://i.imgur.com/QdYSbbA.gif",
                 "http://i.imgur.com/Zy5heqF.gif", "http://i.imgur.com/H4vsVkh.gif"]


dumpQueue :: YTState -> BotState -> MessageID -> IO()
dumpQueue ytState botState msgID = 
  do
  dumpQueue <- takeMVar (queue ytState)
  putMVar (queue ytState) []
  sendPacket botState $ Send ("Links : "  ++ concatMap (\y -> " youtube.com/watch?v=" ++ ytID (fst y)) dumpQueue) msgID

listQueue :: YTState -> BotState -> MessageID -> IO()
listQueue ytState botState msgID = 
  do
  ytList <- takeMVar $ queue ytState
  putMVar (queue ytState) ytList
  if null ytList then 
   sendPacket botState (Send "Nothing Queued!" msgID)
   else 
    do
    timeRemaining <- getTimeRemaining ytState
    sendPacket botState 
      (Send ("[ # ][ wait  time ]\n" ++
        unlines ( 
          zipWith3 (\x y z -> 
           "[" ++ (if x < 10 then " " ++ show x ++ " " else show x)  ++ "]" ++
           "[  "++ z ++ "  ]" ++
           " \"" ++title (fst y) ++ 
           "\" from [" ++ snd y ++ "]")
         [1..] ytList $ map getFormattedTime $ getWaitTimes ytList timeRemaining))
       msgID)


replaceSong :: BotState -> YTState -> MessageID -> UserData -> String -> String -> IO () 
replaceSong botState ytState msgID sender num ytLink = 
  do
  let numR = fromMaybe (-1) (maybeRead2 num :: Maybe Int)
  let ytLinkID = if isYtLink ytLink then getYtID ytLink else ""
  ytLinkP <- catch (retrieveYtData ytLinkID ytState) (\ (SomeException e) -> return $ Left $ show e)
  case ytLinkP of
    Left err -> putStrLn err >> sendPacket botState (Send "Can't parse the link, invalid ids or impossible to contact google api" msgID)
    Right yt -> do
                ytQ <- takeMVar (queue ytState)
                if numR < 0 || numR > length ytQ  then
                  sendPacket botState (Send "Number not in queue!" msgID) >> putMVar (queue ytState) ytQ
                else
                  (putMVar (queue ytState) $ take (numR -1) ytQ ++ [(yt, name sender)] ++ drop numR ytQ ) >> (
                          sendPacket botState $ Send ("Replaced ["++ num ++"] with : " ++ title yt) msgID)

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
                  tryPutMVar (play ytState) True
                  putMVar (queue ytState) ((yt,botName botState):que)
    putMVar (skip ytState) True

