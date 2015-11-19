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
  , closeConnection
  , disconnect
) where

import qualified Network.WebSockets          as WS
import qualified Network.WebSockets.Stream   as WSS
import qualified Network.Socket              as S
import qualified OpenSSL                     as SSL
import qualified OpenSSL.Session             as SSL
import qualified System.IO.Streams.SSL       as Streams
import qualified System.IO.Streams.Network   as Streams
import qualified System.IO.Streams.Internal  as StreamsIO
import           System.IO                   (stdout, IOMode(..), Handle, openFile)
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Lazy.Char8  as BC
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Aeson                  as J
import           Data.Char                   (isSpace)
import           Data.List
import           Control.Exception           --(finally, catch, SomeException
import           Control.Monad.Trans         (liftIO, MonadIO)
import           Control.Monad.Writer.Strict (runWriterT, execWriterT, tell, WriterT)
import           Control.Monad.Reader        (ReaderT, asks, runReaderT)
import           System.Environment          (getArgs)
import           Control.Monad
import           Data.Time.Clock.POSIX
import           Data.Time.Clock             (UTCTime,getCurrentTime, diffUTCTime)
import           Control.Concurrent.STM
import           Control.Concurrent          (ThreadId)

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

type Net = ReaderT Bot IO

-- | The main Bot data structure.
data Bot = Bot
    { botConnection :: WS.Connection -- ^ Websocket connection to heim.
    , packetCount   :: PacketID -- ^ The packet counter
    , botAgent      :: BotAgent -- ^ The Bot agent given from the server
    , botRoom       :: String -- | The room the bot currently is in
    , botName       :: String -- | Initial bot nick
    , startTime     :: UTCTime
    , botFun        :: BotFunctions
    , sideThreads   :: TVar [ThreadId]
    , logHandle     :: Handle
    }

data BotFunctions = BotFunctions {
    eventsHook :: EuphEvent -> Net (),
    dcHook :: Maybe (IO ())
}

io :: MonadIO m => IO a -> m a
io = liftIO

-- | The main bot call function. When this action ends, the bot is closed.
bot   :: BotFunctions -> IO ()
bot hs = do
         started <- getCurrentTime
         (opts, _) <- io $ getArgs >>= parseOpts
         io $ when (showHelp opts) showUsageAndExit
         han <- if null $ logTarget opts then return stdout else openFile (logTarget opts) AppendMode
         tellLogWithHandle han started "Starting up the bot"
         botStr <- botInit opts hs han started
         tellLogWithHandle han started "Ending the bot"
         disconnect botStr

botInit :: Opts -> BotFunctions -> Handle -> UTCTime -> IO Bot
botInit opts hs h l = do
             client <- botConnect opts hs h l
             botStr <- client $ botMain opts hs h l
             return botStr

botConnect :: Opts -> BotFunctions -> Handle -> UTCTime -> IO (WS.ClientApp Bot -> IO Bot)
botConnect opts h han started = do
        is <- S.getAddrInfo Nothing (Just $ heimHost opts) (Just $ show $ heimPort opts)
        let addr = S.addrAddress $ head is
            fam  = S.addrFamily $ head is
        s <-  S.socket fam S.Stream S.defaultProtocol
        S.connect s addr
        myStream <- if useSSL opts then
                        SSL.withOpenSSL $ do
                        ctx <- SSL.context
                        ssl <- SSL.connection ctx s
                        SSL.connect ssl
                        (i,o) <- Streams.sslToStreams ssl
                        WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
                    else
                        do
                        (i,o) <- Streams.socketToStreams s
                        WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
        return $ WS.runClientWithStream myStream (heimHost opts) (roomPath $ roomList opts) WS.defaultConnectionOptions []

botMain :: Opts -> BotFunctions -> Handle -> UTCTime -> WS.ClientApp Bot
botMain o h han started c =
                do
                counter <- atomically $ newTVar 1
                userVar <- atomically newEmptyTMVar
                threadVar <- atomically $ newTVar []
                let thisBot = Bot c counter userVar (roomList o) (Euphs.Options.nick o) started h threadVar han
                runReaderT botLoop thisBot
                return thisBot

-- | Function for closing off the bot.
disconnect :: Bot -> IO ()
disconnect hs = case dcHook $ botFun hs of
                  Nothing -> return ()
                  Just fun -> fun

botLoop :: Net ()
botLoop = do

          sendPacket $ Nick "Testing"
          a <- io $ getLine
          tellLog $ T.pack a
          closeConnection
          return ()

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


-- | Debug test bot
testBot :: IO ()
testBot = bot (BotFunctions (\x -> void $ tellLog (T.pack $ show x)) Nothing)

--botLoop :: BotName -> RoomName -> MVar Bool -> BotFunction -> WS.ClientApp ()
--botLoop botNick room closed botFunct conn = do
--        _ <- forkIO $ catch ( forever (
--          do
--          msg <- WS.receiveData conn :: IO B.ByteString
--          --putStrLn $ BC.unpack msg
--          --liftIO $ putStrLn $ maybe  ("Can't parse this : " ++ BC.unpack msg) (show) evt
--          case evt of
--            Just (PingEvent x _) -> sendPacket botState (PingReply x)
--            Just (NickReply _ user)   ->  putMVar myAgent user
--            Just (SendEvent (MessageData _ mesgID _ _ (stripPrefix ("!uptime @" ++ botNick)  -> Just r) _ _)) ->
--                 getPOSIXTime >>= (\x -> sendPacket botState (Send ("Been up  for " ++ getUptime botState (round x)) mesgID))
--            Just (SendEvent (MessageData _ mesgID _ _ (stripPrefix ("!ping @" ++ botNick) -> Just _) _ _)) -> sendPacket botState (Send "Pong!" mesgID)
--            Just (SendEvent (MessageData _ mesgID _ _ (stripPrefix "!ping" -> Just r) _ _)) -> when (null $ filter (not .isSpace) r) $ sendPacket botState (Send "Pong!" mesgID)
--            Just x                    ->  void $ forkIO $ botFunct botState x
--            Nothing                   ->  putStrLn $ "Can't parse this: " ++ BC.unpack msg
--          )) (\ (SomeException _) -> closeConnection botState True )
--
--        putStrLn $ "Connected to Euphoria! With nick: " ++ botNick ++ " and in the room: " ++ botRoom botState
--
--        let loop x = (unless x $ takeMVar closed >>= loop) in takeMVar closed >>= loop
--        void $ threadDelay 1000000
--        {-forkIO $ forever (-}
--            {-do-}
--            {-a <- timeout 1000000 $ readChan timeoutChan-}
--            {-t <- getPOSIXTime-}
--            {-case a of -}
--            {-Nothing -> closeConnection botState-}
--            {-Just timed -> do-}
--                          {-putStrLn $ "PONG! " ++ ( show ( fromInteger timed - round t))-}
--                          {-threadDelay (1000000*(fromInteger timed - round t))-}
--                          {-return ()-}
--                          {-)-}
--
--
--
--
getNextPacket :: Net Int
getNextPacket = do
            counter <- asks packetCount
            io $ atomically $ do
                   a <- readTVar counter
                   modifyTVar counter (+1)
                   return a

sendPacket :: EuphCommand -> Net ()
sendPacket euphPacket =
      do
      seqNum <- getNextPacket
      conn <- asks botConnection
      io $ WS.sendTextData conn $ J.encode (Command seqNum euphPacket)

-- | Function for closing the connection from inside the Net monad
closeConnection :: Net ()
closeConnection =
  do
  conn <- asks botConnection
  io $ WS.sendClose conn $ T.pack ""

getBotAgent :: Net UserData
getBotAgent = asks botAgent >>= (io . atomically .  readTMVar)

getUptime :: Net String
getUptime = do
    sT <- asks startTime
    eT <- io $ getCurrentTime
    let myTime = floor $ diffUTCTime eT sT :: Integer
        hours =  div myTime 3600
        minutes = div (myTime-hours*3600) 60
        seconds = (myTime - hours*3600 - minutes*60)
    return $ if myTime > 0 then
          show hours   ++ "h " ++
          show minutes ++ "m " ++
          show seconds ++ "s."
        else
          "UhOh, negative time?"
