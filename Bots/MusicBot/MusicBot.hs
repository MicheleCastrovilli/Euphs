{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Euphs.Bot
import           Euphs.Events
import           Euphs.Types
import           Euphs.Commands
import           Euphs.Options

import           YoutubeAPI
import           Types
import           Countries
import           Help
import           Utils

import qualified Data.Sequence as SQ
import qualified Data.Set as S
import           Data.Char (toLower)
import           Data.List (sortBy, isPrefixOf, isSuffixOf, stripPrefix,intercalate, mapAccumR)
import           Data.Maybe (fromMaybe, mapMaybe, isJust)
import           Data.Function (on)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Safe

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT, asks)
import           Control.Monad (void, guard)

import           Control.Concurrent.STM
import           Control.Exception (catch, SomeException)

import           System.Timeout

main :: IO ()
main = do
    opts <- getOpts (defaults { config = "MusicBot.yaml"
                        , botNick = "♪|FakeDJ"
                        , roomList = "haskell"}) options
    config' <- getBotConfig opts :: IO (Maybe MConfig)
    case config' of
        Nothing -> putStrLn "Error on reading config file"
        Just conf -> makeBot conf opts >>= (flip botWithOpts opts)

makeBot :: MConfig -> Opts ->  IO BotFunctions
makeBot config' opts = do
    let room = takeWhile (/='-') (roomList opts)
    prevQueue <- catch (readFile $ roomQueue room) (\x -> print (x :: SomeException) >> return "")
    let !x = fromMaybe SQ.empty $ maybeRead prevQueue :: Queue
    qv  <- atomically $ newTVar x
    lpv <- atomically $ newTVar SQ.empty
    who <- atomically $ newTVar S.empty
    sk  <- atomically $ newEmptyTMVar
    let ms = MusicState qv lpv config' who sk
    return $ BotFunctions {
            eventsHook = musicHook ms
        ,   dcHook = Just $ cleanUp room ms
        ,   helpShortHook = Just $ const helpFunShort
        ,   helpLongHook = Just $ const helpFun
        }

cleanUp :: String -> MusicState -> IO ()
cleanUp r ms = do
               saveQueue <- atomically $ readTVar $ queue ms
               writeFile (roomQueue r) $ show saveQueue

musicHook :: MusicState -> EuphEvent -> Net ()
musicHook ms (SendEvent msg) = runReaderT (musicCommand msg) ms
musicHook ms s@SnapshotEvent{} = runReaderT (musicInit s >> musicLoop) ms
musicHook ms (JoinEvent user) = runReaderT (musicUserJoin user) ms
musicHook ms (PartEvent user) = runReaderT (musicUserPart user) ms
musicHook ms (NickEvent user _) = runReaderT (musicUserChange user) ms
musicHook _ _ = return ()

musicInit :: EuphEvent -> MusicBot ()
musicInit (SnapshotEvent _ _ _ users' msgs) = do
    let sortedMsg = sortBy (compare `on` timeRecieved) msgs
    let lastPlayedSong = headMay $ mapMaybe auxMay sortedMsg
    peoplePresent <- asks peopleSet
    let pSet = S.fromList $ map (flip User Nothing) users' --TODO : Mantain a list of People <> Country associations
    liftIO $ atomically $ writeTVar peoplePresent pSet
    case lastPlayedSong of
        Nothing -> return ()
        Just v -> do
                  let rq = fst v
                  video <- retrieveYoutube $ youtubeID rq
                  case video of
                      One meta -> do
                                  backlog <- asks previousQueue
                                  mc <- asks musicConfig
                                  let qi = QueueItem meta (sender $ snd v) (fromMaybe 0 $ startTimeReq rq)
                                                          (fromMaybe (duration meta) $ stopTimeReq rq)
                                  let qdi = QueuedItem qi (timeRecieved $ snd v)
                                  liftIO $ atomically $ modifyTVar' backlog $ pqAdd mc qdi
                      _ -> return ()
    where auxMay m = case parsePlayMay (contentMsg m) of
                         Just x -> Just (x,m)
                         Nothing -> Nothing
musicInit _ = return ()

-- TODO: Get Proper User country.
musicUserJoin :: UserData -> MusicBot ()
musicUserJoin user = do peoplePresent <- asks peopleSet
                        liftIO $ atomically $ modifyTVar' peoplePresent $ S.insert $ User user Nothing

musicUserPart :: UserData -> MusicBot ()
musicUserPart user = do peoplePresent <- asks peopleSet
                        liftIO $ atomically $ modifyTVar' peoplePresent $ S.delete $ User user Nothing

musicUserChange :: UserData -> MusicBot ()
musicUserChange user = do peoplePresent <- asks peopleSet
                          liftIO $ atomically $ modifyTVar' peoplePresent $ S.insert $ User user Nothing

setUserCountry :: MessageData -> MusicBot ()
setUserCountry m = let c = (stripPrefix "!setcountry" $ contentMsg m) >>= maybeRead :: Maybe Country in
                   case c of
                     Nothing -> lift $ void $ sendError m "Invalid country code."
                     i@(Just _) -> do peoplePresent <- asks peopleSet
                                      io $ atomically $ modifyTVar' peoplePresent $ S.insert $ User (sender m) i

sendError :: MessageData -> String -> Net EuphEvent
sendError m = sendReply m . (++) "Error: "

musicLoop :: MusicBot ()
musicLoop = do timeRemaining <- getEndSongTime
               waitFor timeRemaining
               playFirstSong
               musicLoop

waitFor :: Int -> MusicBot ()
waitFor t = do skip' <- asks skipSong
               liftIO $ void $ timeout t $ atomically $ takeTMVar skip'

playFirstSong :: MusicBot ()
playFirstSong = do q <- asks queue
                   qd <- asks previousQueue
                   mc <- asks musicConfig
                   (Just vid, nx) <- liftIO $ atomically $ do
                                     q' <- readTVar q
                                     let v = sqHeadMay q'
                                     let n = sqHeadMay $ SQ.drop 1 q'
                                     guard $ isJust v
                                     writeTVar q $ SQ.drop 1 q'
                                     return (v,n)
                   reply <- prettyPlay vid nx -- TODO: Add case expression for matching reply.
                   let qdi = QueuedItem vid (timeRecieved $ msgData reply)
                   liftIO $ atomically $ modifyTVar qd $ pqAdd mc qdi

prettyPlay :: QueueItem -> Maybe QueueItem -> MusicBot EuphEvent
prettyPlay video next = lift $ sendPacket $ flip Send "" prettify
    where prettify = unlines [intercalate " " $ map ($ video) [showDuration, showTitle, showRequester],
                              showPlay video, maybeShow next]
          showDuration v = '[':showTime (stopTime video - Types.startTime v) ++ "]"
          showTitle v = title $ metadata v
          showRequester v = "from [" ++ (name $ requester v) ++ "]"
          maybeShow (Just x) = "Next: " ++ showTitle x ++ " " ++ showRequester x
          maybeShow Nothing  = "Next: Nothing"
          showPlay v= "!play " ++ playFormat v

showTime :: Int -> String
showTime i | i > 0 = let (h,[m,s]) = mapAccumR quotRem i [60,60] in
                     intercalate ":" $ map showDigit $ [h,m,s]
           | i > -3 = "now"
           | otherwise = "past"
    where showDigit x = (if x < 10 then "0" else "") ++ show x


getEndSongTime :: MusicBot Int
getEndSongTime = do
    pastQ' <- asks previousQueue
    pastQ <- liftIO $ atomically $ readTVar pastQ'
    let h = sqHeadMay pastQ
    curT <- round <$> (liftIO getPOSIXTime)
    case h of
        Nothing -> return 0
        Just qd -> return $ calcTime qd curT
    where qi i = queuedItem i
          calcTime qd curT = fromIntegral (timePlayed qd - curT) + (stopTime (qi qd) - Types.startTime (qi qd))

musicCommand :: MessageData -> MusicBot ()
musicCommand msg = matchCommand msg >> matchPlay msg

matchCommand :: MessageData ->  MusicBot ()
matchCommand md = case contentMsg md of
                    '!':_ -> maybe (return ()) (flip ($) md . snd) $ headMay $ filter (\(x,_) -> x md) commandList
                    _ -> return ()

commandList :: [(MessageData -> Bool , MessageData -> MusicBot ())]
commandList = [] ++ map (\(x,y) -> (x . contentMsg, y)) textCommands

textCommands :: [(String -> Bool, MessageData -> MusicBot ())]
textCommands =
    map (applyWeird $ \x x' -> auxMay $ matchPrefixes x x') nonCasePrefix ++
    map (applyFirst $ auxMay . allPrefixes) nonCasePrefixOnly ++
    map (applyFirst $ auxMay . noCase) nonCase ++
    map (applyFirst $ auxMay . exactCase) exactWord
        where applyWeird f (x,x',y) = (f x x', y)
              applyFirst f (x,y) = (f x,y)
              firstWord x = headMay $ words x
              auxMay f x = maybe False f $ firstWord x
              exactCase x y = x == y
              noCase x y = x == map toLower y
              allPrefixes x y = isPrefixOf x $ map toLower y
              matchPrefixes x y z = and (zipWith (==) z y) && length z >= length x

nonCasePrefix :: [(String,String, MessageData -> MusicBot ())]
nonCasePrefix =
    [ ("!dumpq"  , "!dumpqueue"    , dumpQ)
    , ("!queuef" , "!queuefirst"   , queueFirst)
    , ("!restr"  , "!restricted"   , restrict)
    , ("!restr"  , "!restrictions" , restrict)
    , ("!allow"  , "!allowed"      , allow)
    , ("!sub"    , "!substitute"   , substitute)
    , ("!rep"    , "!replace"      , substitute)
    , ("!ins"    , "!insert"       , insert)
    , ("!del"    , "!delete"       , delete)
    , ("!rem"    , "!remove"       , delete)
    ]

nonCasePrefixOnly :: [(String, MessageData -> MusicBot ())]
nonCasePrefixOnly =
    [ ("!qf"                       , queueFirst)
    , ("!q"                        , queueSong)
    , ("!skip"                     , skip)
    ]

nonCase :: [(String, MessageData -> MusicBot ())]
nonCase =
    [ ("!dramaticskip"             , dSkip)
    , ("!dskip"                    , dSkip)
    , ("!rm"                       , delete)
    , ("!list"                     , list)
    , ("!kill"                     , kill)
    , ("!nls"                      , neonLights)
    , ("!neonlightshow"            , neonLights)
    , ("!switch"                   , swap)
    , ("!swap"                     , swap)
    , ("!test"                     , test)
    , ("!setcountry"               , setUserCountry)
    ]

exactWord :: [(String, MessageData -> MusicBot ())]
exactWord = []

dSkip :: MessageData -> MusicBot ()
dSkip x = lift $ void $ sendReply x "Skipped something, pretend"

dumpQ :: MessageData -> MusicBot ()
dumpQ x = lift $ void $ sendReply x "Dumped the queue, pretend"

queueFirst :: MessageData -> MusicBot ()
queueFirst x = lift $ void $ sendReply x "Queued thing first, pretend"

queueSong :: MessageData -> MusicBot ()
queueSong x = lift $ void $ sendReply x "Queued thing, pretend"

restrict :: MessageData -> MusicBot ()
restrict x = lift $ void $ sendReply x "Restricted, pretend"

allow :: MessageData -> MusicBot ()
allow x = lift $ void $ sendReply x "Allowed, pretend"

substitute :: MessageData -> MusicBot ()
substitute x = lift $ void $ sendReply x "Substitute, pretend"

insert :: MessageData -> MusicBot ()
insert x = lift $ void $ sendReply x "Insert, pretend"

delete :: MessageData -> MusicBot ()
delete x = lift $ void $ sendReply x "Delete, pretend"

skip :: MessageData -> MusicBot ()
skip x = lift $ void $ sendReply x "Skipped, pretend"

list :: MessageData -> MusicBot ()
list x = lift $ void $ sendReply x "List, pretend"

kill :: MessageData -> MusicBot ()
kill x = lift $ void $ sendReply x "Kill, pretend"

swap :: MessageData -> MusicBot ()
swap x = lift $ void $ sendReply x "Swap, pretend"

neonLights :: MessageData -> MusicBot ()
neonLights x = lift $ void $ sendReply x "Imagine an amazing lightshow here"

matchPlay :: MessageData -> MusicBot ()
matchPlay x = do
    p <- parsePlay (contentMsg x)
    case p of
        Nothing -> return ()
        Just video -> handlePlay video

handlePlay :: YTResult -> MusicBot ()
handlePlay _ = return ()

parsePlay :: String -> MusicBot (Maybe YTResult)
parsePlay str = case parsePlayMay str of
                    Nothing -> return Nothing
                    Just req -> Just <$> retrieveYoutube (youtubeID req)

parsePlayMay :: String -> Maybe YoutubeRequest
parsePlayMay str = headMay $ mapMaybe parseRequest $ drop 1 $ dropWhile (not . isSuffixOf "!play") $ words str

test :: MessageData -> MusicBot ()
test x = do
         peoples <- asks peopleSet
         result <- liftIO $ atomically $ readTVar peoples
         lift $ void $ sendReply x $ show result

{-ytFunction :: YTState -> BotFunction

getFormattedTime :: Integer -> String
getFormattedTime x = let hours  =  div x 3600
                         minutes = div (x-hours*3600) 60
                         seconds = (x - hours*3600 - minutes*60)
                         in if x > 0 then
                             intercalate ":" $ map auxTime [hours,minutes,seconds]
                            else if x < 0 then
                             "past"
                            else
                             "now"
                      where auxTime :: Integer -> String
                            auxTime x = (if x < 10 then "0" else "") ++ show x



ytDescription :: YTQueueItem -> String
ytDescription yt = titleAuthorDuration yt ++ "\n!play youtube.com/watch?v=" ++ ytID (ytmeta yt) ++
                   aux "t" (startTime yt) ++ aux1 "te" (stopTime yt) (duration $ ytmeta yt) ++ "\n"
                   where aux c t = if t /= 0 then "&"++c++"=" ++ show t else ""
                         aux1 c t t' = if t /= 0 && t /= t'  then "&"++c++"=" ++ show t else ""
titleAuthor :: YTQueueItem -> String
titleAuthor x = title (ytmeta x) ++ " from [" ++ name (requester x) ++  "]"

titleAuthorDuration :: YTQueueItem -> String
titleAuthorDuration x = "[" ++ getFormattedTime (stopTime x - startTime x + restingTime ) ++  "] " ++ titleAuthor x

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
  putMVar (lastPlayed ytState) (lastPlay)
  curTime <- getPOSIXTime
  case SQ.null lastPlay of
    True -> return 0
    False -> let x = SQ.index lastPlay 0 in return $ stopTime x - (round curTime - timePlayed x) - (startTime x) + restingTime

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
  let back  = "back" `elem` opts
  ytSeq <- readMVar $ (if back then lastPlayed else queue) ytState
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
        "[ # ][ " ++ switchT back "past" "wait" ++ "  time ]\n" ++
        unlines (F.toList $
          SQ.zipWith (\y z ->
            numberPart (fst y)  ++
            "[  "++ z ++ "  ] \"" ++ titleAuthor (snd y)  ++
            verbosePart (snd y) verbose ids ++
            restrictPart (snd y) restr
            )
         (SQ.mapWithIndex (\x y-> (x+1,y)) ytSeq) $ fmap getFormattedTime $ getWaitTimes ytSeq timeRemaining))
       mesgID)
         where numberPart x = "[" ++ (if x < 10 then " " ++ show x ++ " " else show x)  ++ "]"
               spaces = "\n                     "
               youtubeIdLinks y ids  =  (if ids then "YTID: " else "Link: youtube.com/watch?v=") ++ ytID (ytmeta y)
               verbosePart y verbose ids =  concat [spaces ++ youtubeIdLinks y ids | verbose]
               restrictPart y restr = concat (let restrict = showRestrict $ ytmeta y in
                                           [spaces ++ shorten 56 restrict | restr && (not $ null restrict)])
               switchT x y z = if x then y else z

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
                if numR <= 0 || numR > length ytQ  then
                  sendPacket botState (Send "Number not in queue!" mesgID) >> putMVar (queue ytState) ytQ
                else
                  putMVar (queue ytState)  (SQ.update (numR-1) yt ytQ) >>
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
    ytLink <- catch (retrieveRequest ytState ("a1Y73sPHKxw",0,-1) thisBot 0) (\ (SomeException e) -> return $ Left $ show e)
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
      queueSongsInt (take 100 $ map (\x -> (x,0,-1)) $ filter (\x -> all (\y -> isAlphaNum y || '-' == y || '_' == y) x && length x > 9 ) $ reduceCommas text)
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
                  let posT =  if pos >= 1 && pos <= length ytQ + 1 then pos else length ytQ + 1
                  let updatedQ = SQ.take (posT-1) ytQ SQ.>< SQ.singleton yt SQ.>< SQ.drop (posT-1) ytQ
                  putMVar (queue ytState) updatedQ
                  timeRemaining <- getTimeRemaining ytState
                  let timeQueued =  let x = SQ.take posT $ getWaitTimes updatedQ timeRemaining in
                                        if SQ.null x then 0 else SQ.index x (SQ.length x - 1)
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
    putMVar (queue ytState) (seqSwap num1 num2 curQ) >>
    (sendPacket botState $ Send (swappedMsg num1 num2 curQ) mesgID)
  else
    putMVar (queue ytState) curQ >>
    (sendPacket botState $ Send "Error on parsing the command or index out of ranges. Usage : !switch (or !swap) <pos1> <pos2>" mesgID)
  where swappedMsg num1 num2 q = let sw n = title (ytmeta $ SQ.index q (n-1)) in "Swapped \"" ++ sw num1 ++ "\" with \"" ++ sw num2 ++ "\"."

swap :: Int -> Int -> [a] -> [a]
swap n1 n2 xs = zipWith selectElement [1..] xs
  where
    selectElement i x
         | i == n1     = xs !! (n2-1)
         | i == n2     = xs !! (n1-1)
         | otherwise = x

seqSwap :: Int -> Int -> SQ.Seq a -> SQ.Seq a
seqSwap n1' n2' s = let n1 = n1' - 1
                        n2 = n2' - 1 in
                        SQ.update n2 (SQ.index s n1) $ SQ.update n1 (SQ.index s n2) s

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

-}
