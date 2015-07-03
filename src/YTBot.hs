{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module YTBot where

import qualified Data.Aeson as J
import           Euphoria.Bot
import           Euphoria.Events
import           Euphoria.Types
import           Euphoria.Commands
import           Control.Concurrent
import           Control.Monad      (forever,liftM,mzero, void, when )
import           Data.List
import           Network.HTTP.Conduit
import           Data.Char
import           Data.Maybe
import           System.IO.Unsafe
import           System.Timeout
import           Data.Time.Clock.POSIX
import           Control.Exception

data YTState = YTState {
              queue    :: MVar YTQueue,
              skip     :: MVar Bool,
              play     :: MVar Bool,
              lastPlay :: MVar Int,
              lastSong :: MVar (Maybe YTMetadata),
              apiKey   :: String
              }

data YTMetadata = YTMetadata {
  ytID :: String,
  title :: String,
  thumbnailUrl :: String,
  duration :: Int,
  durationStr :: String,
  embeddable :: Bool,
  restricted :: String
} deriving (Show, Read)

type Requester = String
type YTQueue = [(YTMetadata, Requester)]

apiUrl :: String 
apiUrl = "https://content.googleapis.com/youtube/v3/videos?part=snippet%2C+status%2C+contentDetails&id="

apiToken :: String -> String
apiToken apiKey = "&key=" ++ apiKey

getYtFun :: String -> IO BotFunction
getYtFun apiKey = 
  do
  !a <- readFile "tmpQueue"
  let x = fromMaybe [] (maybeRead2 a :: Maybe YTQueue)
  que <- newMVar x
  skip <- newEmptyMVar
  play <- newEmptyMVar
  lastPlay <- newMVar 0
  lastSong <- newMVar Nothing
  return $ ytFunction $ YTState que skip play lastPlay lastSong apiKey

ytFunction :: YTState -> BotFunction
ytFunction ytState botState (SendEvent (MessageData time msgID parentMsg sender !content _ _ ))
   =  case words content of 
      "!vq" : x ->
        do
        -- putStrLn $ unwords x
        let ytLink = map getYtID $ filter isYtLink x
        mapM_ (\x -> queueSong x botState msgID ytState False (name sender)) ytLink
      "!vqf" : x ->
        do
        -- putStrLn $ unwords x
        let ytLink = map getYtID $ filter isYtLink x
        mapM_ (\x -> queueSong x botState msgID ytState True (name sender)) ytLink 
        void $ tryPutMVar (play ytState) True
      "!r" : num : ytLink:_ -> do
        let numR = fromMaybe (-1) (maybeRead2 num :: Maybe Int)
        let ytLinkID = if isYtLink ytLink then getYtID ytLink else ""
        ytLinkP <- catch (retrieveYtData ytLinkID ytState) (\ (SomeException e) -> return $ Left $ show e)
        case ytLinkP of
          Left err -> putStrLn err >> sendPacket botState (Send "Can't parse the link, invalid ids or impossible to contact google api" msgID)
          Right yt -> do
                      ytQ <- takeMVar (queue ytState)
                      if numR < 0 || numR > length ytQ  then
                        do
                        sendPacket botState (Send "Number not in queue!" msgID)
                        putMVar (queue ytState) ytQ
                      else
                        do
                        putMVar (queue ytState) (take (numR -1) ytQ ++ [(yt, (name sender))] ++ drop numR ytQ)
                        sendPacket botState (Send ("Replaced ["++ num ++"] with : " ++ title yt) msgID) 
      "!vlist" : x -> do
        ytList <- takeMVar $ queue ytState
        putMVar (queue ytState) ytList
        if null ytList then 
          sendPacket botState (Send "Nothing Queued!" msgID)
        else 
          do
          timeRemaining <- getTimeRemaining ytState
          sendPacket botState 
            (Send ("[ # ][wait time]\n" ++ (unlines $ 
              zipWith3 (\x y z -> 
                "[" ++ (if x < 10 then " " ++ show x ++ " " else show x)  ++ "]" ++
                "["++ z ++ "]" ++
                " \"" ++title (fst y) ++ "\" " ++
                " - from [" ++ snd y ++ "]")
                [1..] ytList $ map (\x -> if(x <= 0) then "now" else (getFormattedTime x))   $ getWaitTimes ytList timeRemaining))
             msgID)
      "!vskip" : x -> do
                     x <- takeMVar (queue ytState)
                     putMVar (queue ytState) x
                     when (not $ null x) $ putMVar (skip ytState) True 
      "!vdump":x  -> do
                    x <- takeMVar (queue ytState)
                    putMVar (queue ytState) []
                    sendPacket botState (Send ("Links : "  ++ concat (map (\y -> " youtube.com/watch?v=" ++ (ytID $ fst y)) x)) msgID)
      "!vkill":x  -> closeConnection botState 
      "!vdramaticskip":_ -> do
              ytLink <- retrieveYtData "a1Y73sPHKxw" ytState
              case ytLink of
                Left err -> return ()
                Right yt -> do 
                            que <- takeMVar (queue ytState)
                            tryPutMVar (play ytState) True
                            putMVar (queue ytState) ((yt,botName botState):que)
              putMVar (skip ytState) True
      "!help" : x : xs -> when ("@" ++ (filter isAlphaNum x) == "@" ++ (filter isAlphaNum $ botName botState)) (sendPacket botState (Send (helpFun $ botName botState ) msgID))
      _ -> return () 

ytFunction ytState botState (SnapshotEvent {}) = 
  do

  forkIO (do
          readMVar (closedBot botState)
          a <- takeMVar (lastSong ytState)
          ytq <- takeMVar (queue ytState)

          writeFile "tmpQueue" $ case a of 
                                 Nothing -> show ytq
                                 Just x -> show ((x, botName botState):ytq)
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
                  YTMetadata <$> (  ytl J..: "id" )
                             <*> (  snippet J..: "title" )
                             <*> (  snippet J..: "thumbnails" >>= (J..: "default") >>= (J..: "url"))
                             <*> ( parseISO8601 <$> ( ytl J..: "contentDetails" >>= (J..: "duration")))
                             <*> ( ytl J..: "contentDetails" >>= (J..: "duration"))
                             <*> (  ytl J..: "status" >>= (J..: "embeddable"))
                             <*> (return "") -- ( ytl J..: "contentDetails"  >>=  (J..:? "regionRestriction") J..!= mzero >>=  (J..:? "blocked")  J..!= "")

isYtLink :: String -> Bool
isYtLink x = isInfixOf "youtube.com/watch?v=" x || isInfixOf "youtu.be/watch?v=" x

getYtID :: String -> String
getYtID = takeWhile (/= '&') . drop 1 . dropWhile  ( '=' /= )

parseISO8601 :: String -> Int
parseISO8601 x = 
  let revS = reverse x
      sec  = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='S') revS ) :: Maybe Int)
      min  = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='M') revS ) :: Maybe Int)
      hour = fromMaybe 0 (maybeRead2 (reverse $ takeWhile isNumber $ drop 1 $ dropWhile (/='H') revS ) :: Maybe Int)
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
    Right yt -> do
                ytQ <- takeMVar (queue ytState)
                let updatedQ = if first then (yt,requester):ytQ else ytQ ++ [(yt,requester)]
                putMVar (queue ytState) updatedQ
                let pos =  if first then 1 else length ytQ + 1
                timeRemaining <- getTimeRemaining ytState
                let timeQueued = last $ take pos $ getWaitTimes updatedQ timeRemaining
                void $ tryPutMVar (play ytState) True
                sendPacket bs (Send ("Queued! ["++ show pos  ++  "] \"" ++ title yt ++ "\" will be played " ++ if(timeQueued <= 0) then "now" else ("in " ++ getFormattedTime timeQueued )) idRepl)

ytLoop botState (YTState queue skip play lastPlay lastSong _) = forever $ do
  x <- takeMVar queue
  putMVar queue $ drop 1 x
  if null x then
    do
    putStrLn "Waiting for queue!"
    takeMVar play
    putStrLn "Queue started!"
  else
    do
    sendPacket botState (Send ("!play youtube.com/watch?v=" ++ ytID (fst $  head  x) ++ " - " ++ ytDescription (head x) ++ "\nNext: " ++ fromMaybe "Nothing" ((\x -> title (fst x) ++ " from " ++ (snd x))  <$> safeHead(tail x))) "")
    putStrLn $ "Playing Song! " ++ title ( fst $ head x)
    takeMVar lastPlay
    curTime <- getPOSIXTime
    putMVar lastPlay $ round curTime
    takeMVar lastSong 
    putMVar lastSong $ Just $ fst $ head x
    void $ timeout (1000000 * (duration $ fst $ head x)) $ takeMVar skip


maybeRead2 :: Read a => String -> Maybe a
maybeRead2 = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

getWaitTimes :: YTQueue -> Int -> [Int]
getWaitTimes ytList currentWait = init $ scanl (\x y -> x + (duration $ fst y)) currentWait ytList

getFormattedTime :: Int -> String
getFormattedTime x = let hours =  div x 3600
                         minutes = div (x-hours*3600) 60
                         seconds = (x - hours*3600 - minutes*60)
                         in show hours ++ ":" ++ (if (minutes < 10) then "0"  else "") ++ show minutes ++ ":" ++ (if (seconds < 10) then "0"  else "") ++ show seconds


helpFun :: String -> String
helpFun botName = 
  "I am @" ++ botName ++ ", a bot created by @viviff.\n\n" ++
  "Commands: \n!vq <ytLink>... <ytLink> -> Queues all the youtube links found in the message at the end of the queue.\n" ++
  "!vqf <ytLink> ... <ytLink> -> Same thing as !vq but queues at the start of the queue.\n" ++
  "!vlist -> Shows a list of the songs currently in the queue.\n" ++
  "!vskip -> Skips the currently playing song. \n" ++
  "!vdramaticskip -> Skips like the old times :D\n" ++
  "!r <num> <ytLink> -> Replaces the song at position <num> with the new ytLink. \n" ++
  "!vdump -> Dumps the queue\n" ++
  "!vkill -> Kills the bot, forever\n" ++
  "!ping  -> Pong!\n" ++
  "!help @"++ botName ++" -> This very help."

ytDescription :: (YTMetadata,String) -> String
ytDescription yt = "Duration: " ++ getFormattedTime (duration $ fst yt) ++
                   "\nTitle : " ++ title (fst yt) ++ " - from " ++ snd yt 

safeHead :: [a] -> Maybe a
safeHead x = if null x then
              Nothing
             else
              Just $ head x

getTimeRemaining :: YTState -> IO Int
getTimeRemaining ytState =
  do
  lastPlayedTime <- takeMVar (lastPlay ytState)
  putMVar (lastPlay ytState) lastPlayedTime
  curTime <- getPOSIXTime
  lastSongPlayed <- takeMVar $ lastSong ytState
  putMVar (lastSong ytState) lastSongPlayed
  case lastSongPlayed of
    Nothing -> return 0
    Just x -> return $ (duration x) - (round curTime - lastPlayedTime)

