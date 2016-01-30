{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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
  , bot
  , emptyBot
  -- ** Main Functions
--  , bot
--  , botWithOpts
--  , closeBot
--  , disconnect
--  , emptyBot
--  , getBotAgent
--  , sendPacket
--  , getBotConfig
--  , tellLog
--  , sendReply
) where

import qualified Network.WebSockets          as WS
import qualified Network.WebSockets.Stream   as WSS
import qualified Network.Socket              as S

import qualified OpenSSL                     as SSL
import qualified OpenSSL.Session             as SSL
import qualified System.IO.Streams.SSL       as Streams
import qualified System.IO.Streams.Network   as Streams
import qualified System.IO.Streams.Internal  as StreamsIO

import           System.IO                   (stdout, IOMode(..), Handle, openFile, hClose, hPutStrLn)
import           System.Posix.Signals

import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Internal    as BS
--import qualified Data.ByteString.Lazy.Char8  as BC
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as T (decodeUtf8)
import qualified Data.Text.IO                as T

import           Data.Time.Clock             as Time (UTCTime,getCurrentTime, diffUTCTime)
import qualified Data.List                   as L
import qualified Data.Yaml                   as Y
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as S

import           Control.Monad.Trans         (liftIO, MonadIO)
import           Control.Monad.Reader        (ReaderT, asks, runReaderT, ask,MonadReader)
import           Control.Monad.Writer.Lazy   (WriterT, runWriterT, tell)
import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent          (ThreadId, forkIO, myThreadId)
import           Control.Exception           (SomeException, catch, onException)

import           Euphs.Types
import           Euphs.Options

-- | The document path of the websocket
roomPath :: String -> String
roomPath room = "/room/" ++ room ++ "/ws"

-- | A mutable counter for packet ids
type PacketID = TVar Int
-- | The mutable bot agent
type BotAgent = TMVar SessionView
-- | The monad transformer stack, for handling all the bot events
type Net = ReaderT Bot IO

data Close = CorrectClose | Reconnect | RoomDoesntExist

-- TODO: Restructure the bot, the connection should be a side thread, with a proper channel to send the data (even raw packets).
--       The side thread should reconnect, or change room.

newtype RoomList = RoomList (TVar [Room])
type RoomName = String
type PacketsIncoming = TQueue EuphsPacket
type CommandsOutbound = TQueue Command
type ClosingChan = TChan Close
type LogFun = String -> IO ()

-- | The main Bot data structure.
data Bot = Bot
    { botStatic :: BotStatic
    , rooms  :: RoomList
    , logFun :: LogFun
    , botConnOpt :: ConnectionOptions
    }

data BotStatic = BotStatic {
      botFun :: !BotFunctions -- ^ Custom bot functions
    , sideThreads :: TVar (S.Set ThreadId) -- ^ An experimental way to keep track of the threads spawned
    , botName :: TVar String
}

data Room = Room {
      packetCount :: !PacketID -- ^ The packet counter
    , botAgent :: !BotAgent -- ^ The Bot agent given from the server
    , roomName :: !RoomName -- ^ The room the bot currently is in.
    , roomPW ::  Maybe String -- ^ Room password
    , users :: TVar (S.Set SessionView)
    , lastMessage ::  Maybe String -- ^ The last message id received
    , roomConn :: RoomConnection
}

data RoomConnection = RoomConnection {
      evtQueue ::  PacketsIncoming -- ^ Queue of events
    , cmdQueue ::  CommandsOutbound -- ^ Queue of commands to send to the room
    , closeChan :: ClosingChan -- ^ The way of handling the closing of a bot
    , roomStartTime :: !UTCTime -- ^ Time at which the bot was started
}

-- | Custom Bot functions
data BotFunctions = BotFunctions {
    eventsHook :: EuphEvent -> Net () -- ^ Main event loop. Every event not handled by the bot, calls this function.
  , dcHook :: Maybe (IO ()) -- ^ Special actions to run in a disconnect, for cleanup.
}

-- | The main bot call function. When this action ends, the bot is closed.
bot   :: BotFunctions -> IO ()
bot hs = getOpts defaults options >>= botWithOpts hs

botWithOpts :: BotFunctions -> Opts -> IO ()
botWithOpts hs opts = do
    write <- makeLogger opts
    rooms <- emptyRooms
    bs <- makeBotStatic hs opts
    let b = Bot bs rooms write (connOpt opts)
    return () -- runReaderT botStart b

makeLogger :: Opts -> IO (String -> IO ())
makeLogger opts = do
    han <- if' (null (logTarget opts)) (return stdout) (openFile (logTarget opts) AppendMode)
    started <- getCurrentTime
    writeQueue <- newTQueueIO
    forkIO $ forever (tlog han started writeQueue)
    return (atomically . writeTQueue writeQueue)

tlog :: Handle -> UTCTime -> TQueue String -> IO ()
tlog han sT q = do
    catch (forever $ do text <- atomically $ readTQueue q
                        curTime <- getCurrentTime
                        hPutStrLn han $ "[" ++ getTimeDiff curTime sT ++ "] " ++ text)
          (\x -> hPutStrLn han ("Logger ended : " ++ show (x :: SomeException)) >> hClose han)
    where getTimeDiff a b = show $ diffUTCTime a b

emptyRooms :: IO RoomList
emptyRooms = RoomList <$> (newTVarIO [])

makeBotStatic :: BotFunctions -> Opts -> IO BotStatic
makeBotStatic bf opts = do
    BotStatic bf <$> (newTVarIO $ S.empty) <*> (newTVarIO $ botNick opts)

-- | Empty bot
emptyBot :: BotFunctions
emptyBot = BotFunctions (\_ -> return ()) Nothing

mkRoom :: RoomName -> Maybe String -> Net Room
mkRoom rn pw = do
    room <- sideConnect
    io $ Room <$> (newTVarIO 0) <*> (newEmptyTMVarIO) <*>
         (return rn) <*> (return pw) <*> (io $ newTVarIO S.empty)
         <*> (return Nothing) <*> (return room)
    where sideConnect = do
                        rc <- io $ RoomConnection <$> newTQueueIO <*> newTQueueIO
                                   <*> newTChanIO <*> getCurrentTime
                        connectWith rn rc
                        return rc

parseRoomPw :: String -> (String, Maybe String)
parseRoomPw x  = (takeWhile (/='-') x,toMay $ drop 1 $ dropWhile (/='-') x)
    where toMay [] = Nothing
          toMay x = Just x

connectWith :: RoomName -> RoomConnection -> Net ()
connectWith rn co = do
    client <- botConnect rn
    return ()

botConnect ::  RoomName -> Net (WS.ClientApp a -> IO a)
botConnect rn = do
    co <- asks botConnOpt
    s <- io $ socketConnect co
    tellNet "Connected to the socket"
    (i,o)  <- io $ if' (useSSL co) sslConn Streams.socketToStreams s
    myStream <- io $ WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
    tellNet "Websocket start"
    return $ WS.runClientWithStream myStream (heimHost co) rn WS.defaultConnectionOptions []

socketConnect :: ConnectionOptions -> IO S.Socket
socketConnect opts = do
    is <- S.getAddrInfo Nothing (Just $ heimHost opts) (Just $ show $ heimPort opts)
    let addr = S.addrAddress $ head is
        fam  = S.addrFamily $ head is
    s <-  S.socket fam S.Stream S.defaultProtocol
    S.connect s addr
    return s

sslConn :: S.Socket -> IO (StreamsIO.InputStream BS.ByteString, StreamsIO.OutputStream BS.ByteString)
sslConn s = SSL.withOpenSSL $ do
    ctx <- SSL.context
    ssl <- SSL.connection ctx s
    SSL.connect ssl
    Streams.sslToStreams ssl

io :: MonadIO m => IO a -> m a
io = liftIO

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

-- | Function for closing off the bot.
disconnect :: MonadIO m => BotFunctions -> m ()
disconnect hs = fromMaybe (return ()) (fmap io $ dcHook hs)

tellNet :: String -> Net ()
tellNet str = do
    writer <- asks logFun
    io $ writer str

{-

-- | Function for calling the bot with custom options, using a getOpts call.


botStart :: BotFunctions -> Opts -> UTCTime ->  Log ()
botStart hs opts timeS = do
    tell "Starting up the bot"
    let rooms = parseRoomList $ roomList opts
    botS <- BotStatic timeS hs <$> (io $ newTVarIO S.empty) <*> (io $ newTVarIO [])
    roomList <- sequence $ map (\x -> uncurry mkRoom x $ connOpts opts) rooms
    runReaderT mainBot $ Bot botS roomList
    disconnect hs
    where parseRoomList = map parseRoomPw  . words


botMain :: Opts -> BotFunctions -> Handle -> UTCTime -> TChan () -> WS.ClientApp Bot
botMain o h han started closing c = do
                counter <- atomically $ newTVar 1
                userVar <- atomically newEmptyTMVar
                threadVar <- atomically $ newTVar []
                evtQ <- atomically newTQueue
                let pw = drop 1 $ L.dropWhile (/= '-') $ roomList o
                let maybePw = if null pw then Nothing else Just pw
                let thisBot = Bot c counter userVar (roomList o) (botNick o) started h threadVar han evtQ maybePw closing
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
          _ <- io $ installHandler keyboardSignal (CatchOnce $ atomically $ writeTChan closing ()) $
                    Just $ addSignal sigTERM emptySignalSet
          void $ io $ atomically $ readTChan closing


botQueue :: Net ()
botQueue = do
           conn <- asks botConnection
           close <- asks closeVar
           thisName <- asks botName
           replies <- asks evtQueue
           fun  <- asks botFun
           forever $ do
                  msg <- io $ onException (WS.receiveData conn :: IO B.ByteString) (atomically (writeTChan close ()))
                  --tellLog $ TL.toStrict $ T.decodeUtf8 msg
                  case decodePacket msg of
                      Left stuff -> tellLog $ "Can't parse : " `T.append` TL.toStrict (T.decodeUtf8 msg) `T.append` T.pack ("\nReason: " ++ stuff)
                      Right event -> forkBot $
                                     case event of
                                       PingEvent x _ -> sendPing x
                                       p@(SendEvent m) -> case words $ contentMsg m of
                                                            "!ping" : x : _ -> when (x == "@" ++ thisName) (sendPong m)
                                                            ["!ping"] -> sendPong m
                                                            "!uptime" : x : _ -> when (x == "@" ++ thisName)
                                                                $ getUptimeReply >>= \reply -> void $ sendReply m reply
                                                            ["!help"] -> sendMaybeHelp m [] $ helpShortHook fun
                                                            "!help" : x : y  -> when (x == "@" ++ thisName) $ sendMaybeHelp m y $ helpLongHook fun
                                                            _ -> eventsHook fun p
                                       HelloEvent bs _ _ -> do
                                                            ag <- asks botAgent
                                                            io $ atomically $ putTMVar ag bs
                                       n@(NickReply _ u _) -> do
                                                            ag <- asks botAgent
                                                            io $ atomically $ do swapTMVar ag u
                                                                                 writeTQueue replies n
                                       x | isReply x -> io $ atomically $ writeTQueue replies x
                                         | otherwise -> eventsHook fun x
           where isReply WhoReply{} = True
                 isReply LogReply{} = True
                 isReply SendReply{} = True
                 isReply NickReply{} = True
                 isReply AuthReply{} = True
                 isReply _ = False
                 sendPong x = void $ sendReply x "Pong!"
                 sendMaybeHelp m l = maybe (return ()) (\x -> x l >>= (void . sendReply m))

forkBot :: Net () -> Net ()
forkBot act = do
          thisBot <- ask
          thisThreads <- asks sideThreads
          void $ io $ forkIO $ do
              thrID <- myThreadId
              atomically $ modifyTVar thisThreads (S.add thrID)
              catch (runReaderT act thisBot) (\x -> runReaderT (tell $ show (x :: SomeException)) thisBot)
              atomically $ modifyTVar thisThreads (S.delete thrID)

getNextPacket :: Net Int
getNextPacket = do
            counter <- asks packetCount
            io $ atomically $ do
                   a <- readTVar counter
                   modifyTVar counter (+1)
                   return a

sendPing :: Integer -> Net ()
sendPing x = do
      --tellLog $ "Sending ping " `T.append` T.pack (show x)
      conn <- asks botConnection
      io $ WS.sendTextData conn $ encodePacket $ PingCommand x

-- | Sends an Heim command, and fetches the reply.
sendPacket :: EuphCommand -> Net EuphEvent
sendPacket euphPacket =
      do
      seqNum <- getNextPacket
      tell $ "Sending packet " ++ show euphPacket ++ " with seqnum : " ++ show seqNum
      conn <- asks botConnection
      evts <- asks evtQueue
      io $ WS.sendTextData conn $ encodePacket $ Command seqNum euphPacket
      ev <- io $ atomically $ do
          evt <- readTQueue evts
          guard $ matchIdReply seqNum evt
          return evt
      tell $ "Recieved packet " ++ show ev
      return ev

-- | Function for closing the bot from inside the Net monad
closeBot :: Net ()
closeBot =
  do
  closing <- asks closeVar
  io $ atomically (writeTChan closing ())

-- | An easier way to read the current bot agent
getBotAgent :: Net SessionView
getBotAgent = asks botAgent >>= (io . atomically .  readTMVar)

getUptime :: Net String
getUptime = do
    sT <- asks startTime
    eT <- io Time.getCurrentTime
    let myTime = round $ Time.diffUTCTime eT sT :: Integer
    let (y, y') = L.mapAccumR quotRem myTime [24,60,60]
    let res = unwords $ map (\(x, z) -> show z ++ x) $ dropWhile ((== 0) . snd) $  zip ["d", "h", "m", "s"] $ y:y'
    return $ show sT ++ " (" ++ res ++ ")"

getUptimeReply :: Net String
getUptimeReply = liftM ((++) "/me has been up since " . flip (++) ".") getUptime

-- | Interface to the YAML parser, using the Bot commanline interface
getBotConfig :: (Y.FromJSON a) => Opts -> IO (Maybe a)
getBotConfig o =
    if null $ config o then
      return Nothing
    else
      Y.decodeFile $ config o


-- | Convenience function for sendPacket $ Send str (msgID m)
sendReply :: Message -> String -> Net EuphEvent
sendReply m str = sendPacket $ Send str $ msgID m

-}
