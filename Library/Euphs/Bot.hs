{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ViewPatterns #-}
-- | Provides a framework to build bots connecting to <https://github.com/euphoria-io/heim heim>.
-- This module is still a bit unstable, and between API changes, it will break a lot.
module Euphs.Bot (
  -- ** Structures and type synonyms
    Bot(..)
  , BotFunctions(..)
  , Net
  , BotAgent
  , PacketID
  -- ** Main Functions
  , bot
  , botWithOpts
  , closeBot
  , disconnect
  , emptyBot
  , getBotAgent
  , sendPacket
  , getBotConfig
) where

import qualified Network.WebSockets          as WS
import qualified Network.WebSockets.Stream   as WSS
import qualified Network.Socket              as S

import qualified OpenSSL                     as SSL
import qualified OpenSSL.Session             as SSL
import qualified System.IO.Streams.SSL       as Streams
import qualified System.IO.Streams.Network   as Streams
import qualified System.IO.Streams.Internal  as StreamsIO

import           System.IO                   (stdout, IOMode(..), Handle, openFile, hClose)
import           System.Posix.Signals        (keyboardSignal, installHandler, Handler(Catch))

import qualified Data.ByteString.Lazy        as B
--import qualified Data.ByteString.Lazy.Char8  as BC
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as T (decodeUtf8)
import qualified Data.Text.IO                as T

import           Data.Time.Clock             as Time (UTCTime,getCurrentTime, diffUTCTime)
import qualified Data.List                   as L
import qualified Data.Yaml                   as Y
import           Data.Maybe                  (fromMaybe)

import           Control.Monad.Trans         (liftIO, MonadIO)
import           Control.Monad.Reader        (ReaderT, asks, runReaderT, ask)
import           Control.Monad               (forever, when, void, guard, liftM)
import           Control.Concurrent.STM
import           Control.Concurrent          (ThreadId, forkIO, myThreadId)
import           Control.Exception           (SomeException, catch, onException)

import           Euphs.Events
import           Euphs.Commands
import           Euphs.Types
import           Euphs.Options

-- | The document path of the websocket
roomPath :: String -> String
roomPath room = "/room/" ++ room ++ "/ws"

-- | A mutable counter for packet ids
type PacketID = TVar Int
-- | The mutable bot agent
type BotAgent = TMVar UserData
-- | The monad transformer stack, for handling all the bot events
type Net = ReaderT Bot IO

-- | The main Bot data structure.
data Bot = Bot
    { botConnection :: WS.Connection -- ^ Websocket connection to heim.
    , packetCount   :: PacketID -- ^ The packet counter
    , botAgent      :: BotAgent -- ^ The Bot agent given from the server
    , botRoom       :: String -- ^ The room the bot currently is in.
    , botName       :: String -- ^ Initial bot nick
    , startTime     :: UTCTime -- ^ Time at which the bot was started
    , botFun        :: BotFunctions -- ^ Custom bot functions
    , sideThreads   :: TVar [ThreadId] -- ^ An experimental way to keep track of the threads spawned
    , logHandle     :: Handle -- ^ Logging handle
    , evtQueue      :: TQueue EuphEvent -- ^ Queue of reply events
    , roomPW        :: Maybe String -- ^ Room password
    , closeVar      :: TChan ()
    }

-- | Custom Bot functions
data BotFunctions = BotFunctions {
    eventsHook :: EuphEvent -> Net () -- ^ Main event loop. Every event not handled by the bot, calls this function.
  , dcHook :: Maybe (IO ()) -- ^ Special actions to run in a disconnect, for cleanup.
  , helpShortHook :: Maybe (Net String) -- ^ A short !help description
  , helpLongHook :: Maybe (Net String) -- ^ A long !help <botName> description
}

io :: MonadIO m => IO a -> m a
io = liftIO

-- | The main bot call function. When this action ends, the bot is closed.
bot   :: BotFunctions -> IO ()
bot hs = do
         getOpts defaults options >>= botWithOpts hs

-- | Function for closing off the bot.
disconnect :: BotFunctions -> IO ()
disconnect hs = fromMaybe (return ()) (dcHook hs)

-- | Function for calling the bot with custom options, using a getOpts call.
botWithOpts :: BotFunctions -> Opts -> IO ()
botWithOpts hs opts = do
         started <- getCurrentTime
         han <- if null $ logTarget opts then return stdout else openFile (logTarget opts) AppendMode
         tellLogWithHandle han started "Starting up the bot"
         let rooms = map (\x -> opts { roomList = x } ) $ words $ roomList opts
         _ <- sequence_ $ map (\o -> botInit o hs han started) rooms
         tellLogWithHandle han started "Ending the bot"
         disconnect hs
         hClose han

botInit :: Opts -> BotFunctions -> Handle -> UTCTime -> IO Bot
botInit opts hs h l = do
             client <- botConnect opts h l
             closing <- atomically newTChan
             client $ botMain opts hs h l closing

botConnect :: Opts -> Handle -> UTCTime -> IO (WS.ClientApp Bot -> IO Bot)
botConnect opts han started = do
        is <- S.getAddrInfo Nothing (Just $ heimHost opts) (Just $ show $ heimPort opts)
        let addr = S.addrAddress $ head is
            fam  = S.addrFamily $ head is
        s <-  S.socket fam S.Stream S.defaultProtocol
        S.connect s addr
        tellLogWithHandle han started "Connected to the socket"
        myStream <- if useSSL opts then
                        SSL.withOpenSSL $ do
                        ctx <- SSL.context
                        ssl <- SSL.connection ctx s
                        SSL.connect ssl
                        tellLogWithHandle han started "Connected with SSL"
                        (i,o) <- Streams.sslToStreams ssl
                        WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
                    else
                        do
                        (i,o) <- Streams.socketToStreams s
                        WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
        tellLogWithHandle han started "Websocket start"
        return $ WS.runClientWithStream myStream (heimHost opts) (roomPath $ takeWhile (/= '-') $ roomList opts) WS.defaultConnectionOptions []

botMain :: Opts -> BotFunctions -> Handle -> UTCTime -> TChan () -> WS.ClientApp Bot
botMain o h han started closing c =
                do
                counter <- atomically $ newTVar 1
                userVar <- atomically newEmptyTMVar
                threadVar <- atomically $ newTVar []
                evtQ <- atomically newTQueue
                let pw = drop 1 $ L.dropWhile (/= '-') $ roomList o
                let maybePw = if null pw then Nothing else Just pw
                let thisBot = Bot c counter userVar (roomList o) (Euphs.Options.nick o) started h threadVar han evtQ maybePw closing
                runReaderT botLoop thisBot
                return thisBot


botLoop :: Net ()
botLoop = do
          closing <- asks closeVar
          forkBot botQueue
          bname <- asks botName
          pw <- asks roomPW
          case pw of
            Nothing -> return()
            Just p -> do
                      a <- sendPacket (Auth AuthPasscode p)
                      guard $ success a
          _ <- sendPacket $ Nick bname
          _ <- io $ installHandler keyboardSignal (Catch $ atomically $ writeTChan closing ()) Nothing
          void $ io $ atomically $ readTChan closing

tellLog :: T.Text -> Net ()
tellLog text = do
               sT <- asks startTime
               han <- asks logHandle
               io $ tellLogWithHandle han sT text

tellLogWithHandle :: Handle -> UTCTime -> T.Text -> IO ()
tellLogWithHandle han sT text = do
    curTime <- getCurrentTime
    T.hPutStrLn han $ "[" `T.append` getTimeDiff curTime sT `T.append` "] " `T.append` text
    where getTimeDiff a b = T.pack $ show (diffUTCTime a b)

botQueue :: Net ()
botQueue = do
           conn <- asks botConnection
           close <- asks closeVar
           thisName <- asks botName
           replies <- asks evtQueue
           fun  <- asks botFun
           forever $ do
                  msg <- io $ onException (WS.receiveData conn :: IO B.ByteString) (atomically (writeTChan close ()))
                  case decodePacket msg of
                      Left stuff -> tellLog $ "Can't parse : " `T.append` TL.toStrict (T.decodeUtf8 msg) `T.append` T.pack ("\nReason: " ++ stuff)
                      Right event -> forkBot $
                                     case event of
                                       PingEvent x _ -> sendPing x
                                       p@(SendEvent m) -> case words $ contentMsg m of
                                                            "!ping" : x : _ -> when (x == "@" ++ thisName) (sendPong m)
                                                            ["!ping"] -> sendPong m
                                                            "!uptime" : x : _ -> when (x == "@" ++ thisName)
                                                                $ getUptimeReply >>= \reply -> void $ sendPacket $ Send reply $ msgID m
                                                            ["!help"] -> sendMaybeHelp m $ helpShortHook fun
                                                            "!help" : x : _  -> when (x == "@" ++ thisName) $ sendMaybeHelp m $ helpLongHook fun
                                                            _ -> eventsHook fun p
                                       HelloEvent bs _ _ -> do
                                                            ag <- asks botAgent
                                                            io $ atomically $ putTMVar ag bs
                                       x | isReply x -> io $ atomically $ writeTQueue replies x
                                         | otherwise -> eventsHook fun x
           where isReply WhoReply{} = True
                 isReply LogReply{} = True
                 isReply SendReply{} = True
                 isReply NickReply{} = True
                 isReply AuthReply{} = True
                 isReply _ = False
                 sendPong x = void $ sendPacket $ Send "Pong!" (msgID x)
                 sendMaybeHelp m = maybe (return ()) (\x -> x >>= (void . sendPacket . flip Send (msgID m)))

forkBot :: Net () -> Net ()
forkBot act = do
          thisBot <- ask
          thisThreads <- asks sideThreads
          void $ io $ forkIO $ do
              thrID <- myThreadId
              atomically $ modifyTVar thisThreads (thrID : )
              catch (runReaderT act thisBot) (\x -> runReaderT (tellLog $ T.pack $ show (x :: SomeException)) thisBot)
              atomically $ modifyTVar thisThreads (filter (/= thrID))

getNextPacket :: Net Int
getNextPacket = do
            counter <- asks packetCount
            io $ atomically $ do
                   a <- readTVar counter
                   modifyTVar counter (+1)
                   return a

sendPing :: Integer -> Net ()
sendPing x = do
      tellLog $ "Sending ping " `T.append` T.pack (show x)
      seqNum <- getNextPacket
      conn <- asks botConnection
      io $ WS.sendTextData conn $ encodePacket $ Command seqNum $ Ping x

-- | Sends an Heim command, and fetches the reply.
sendPacket :: EuphCommand -> Net EuphEvent
sendPacket euphPacket =
      do
      seqNum <- getNextPacket
      tellLog $ "Sending packet " `T.append` T.pack (show euphPacket) `T.append` " with seqnum : " `T.append` T.pack (show seqNum)
      conn <- asks botConnection
      evts <- asks evtQueue
      io $ WS.sendTextData conn $ encodePacket $ Command seqNum euphPacket
      ev <- io $ atomically $ do
          evt <- readTQueue evts
          guard $ matchIdReply seqNum evt
          return evt
      tellLog $ "Recieved packet " `T.append` T.pack (show ev)
      return ev

-- | Function for closing the bot from inside the Net monad
closeBot :: Net ()
closeBot =
  do
  closing <- asks closeVar
  io $ atomically (writeTChan closing ())

-- | An easier way to read the current bot agent
getBotAgent :: Net UserData
getBotAgent = asks botAgent >>= (io . atomically .  readTMVar)

getUptime :: Net String
getUptime = do
    sT <- asks startTime
    eT <- io Time.getCurrentTime
    let myTime = round $ Time.diffUTCTime eT sT :: Integer
    let startedString = show sT
    let (y, y') = L.mapAccumR quotRem myTime [24,60,60]
    let res = unwords $ map (\(x, z) -> show z ++ x) $ dropWhile ((== 0) . snd) $  zip ["d", "h", "m", "s"] $ y:y'
    return $ startedString ++ " (" ++ res ++ ")"

getUptimeReply :: Net String
getUptimeReply = liftM ((++) "/me has been up since " . flip (++) ".") getUptime

-- | Interface to the YAML parser, using the Bot commanline interface
getBotConfig :: (Y.FromJSON a) => Opts -> IO (Maybe a)
getBotConfig o =
    if null $ config o then
      return Nothing
    else
      Y.decodeFile $ config o

-- | Empty bot
emptyBot :: BotFunctions
emptyBot = BotFunctions (\_ -> return ()) Nothing Nothing Nothing
