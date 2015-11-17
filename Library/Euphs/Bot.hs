{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Euphs.Bot (
    bot
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
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Lazy.Char8  as BC
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Aeson                  as J
import           Data.Char                   (isSpace)
import           Data.List
import           Control.Exception           --(finally, catch, SomeException
import           Control.Monad.Trans         (liftIO, MonadIO)
import           Control.Monad.Writer.Lazy   (runWriterT, execWriterT, tell, WriterT)
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

roomPath :: String -> String
roomPath room = "/room/" ++ room ++ "/ws"

type PacketID = TVar Int
type BotAgent = TMVar UserData

type Log = WriterT T.Text IO
type Net = ReaderT Bot Log

data Bot = Bot
    { botConnection :: WS.Connection
    , packetCount   :: PacketID
    , botAgent      :: BotAgent
    , botRoom       :: String
    , botName       :: String
    , startTime     :: UTCTime
    , botFun        :: BotFunctions
    , sideThreads   :: TVar [ThreadId]
    }

data BotFunctions = BotFunctions {
    eventsHook :: EuphEvent -> Net (),
    dcHook :: Maybe (Log ())
}

io :: MonadIO m => IO a -> m a
io = liftIO

bot   :: BotFunctions -> IO ()
bot hs = runWriterT (do
         botInit hs
         disconnect hs
         ) >>= (T.putStrLn . snd)

botInit :: BotFunctions -> Log ()
botInit hs = do
            (opts, _) <- io $ getArgs >>= parseOpts
            io $ when (showHelp opts) showUsageAndExit
            client <- botConnect opts hs
            result <- io $ client $ botMain opts hs
            tell result

botConnect :: Opts -> BotFunctions -> Log (WS.ClientApp a -> IO a)
botConnect opts h = do
        is <- io $ S.getAddrInfo Nothing (Just $ heimHost opts) (Just $ show $ heimPort opts)
        let addr = S.addrAddress $ head is
            fam  = S.addrFamily $ head is
        s <- io $  S.socket fam S.Stream S.defaultProtocol
        io $ S.connect s addr
        myStream <- if useSSL opts then
                        io $ SSL.withOpenSSL $ do
                        ctx <- SSL.context
                        ssl <- SSL.connection ctx s
                        SSL.connect ssl
                        (i,o) <- Streams.sslToStreams ssl
                        WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
                    else
                        do
                        (i,o) <- io $ Streams.socketToStreams s
                        io $ WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
        return $ WS.runClientWithStream myStream (heimHost opts) (roomPath $ roomList opts) WS.defaultConnectionOptions []

botMain :: Opts -> BotFunctions -> WS.ClientApp T.Text
botMain o h c = execWriterT (do
                counter <- io $ atomically $ newTVar 1
                userVar <- io $ atomically $ newEmptyTMVar
                threadVar <- io $ atomically $ newTVar []
                startTime <- io $ getCurrentTime
                let thisBot = Bot c counter userVar (roomList o) (Euphs.Options.nick o) (startTime) h threadVar
                runReaderT botLoop thisBot
                )

disconnect :: BotFunctions -> Log ()
disconnect hs = case dcHook hs of
                  Nothing -> return ()
                  Just fun -> fun

botLoop :: Net ()
botLoop = return ()

tellLog :: T.Text -> Net ()
tellLog text = do
               curTime <- io $ getCurrentTime
               sT <- asks startTime
               tell $ "[" `T.append` getTimeDiff curTime sT `T.append` "s] " `T.append` text `T.snoc` '\n'
               where getTimeDiff a b = T.pack $ show (diffUTCTime a b)

testBot = bot (BotFunctions (\x -> return ()) Nothing)

--botLoop :: BotName -> RoomName -> MVar Bool -> BotFunction -> WS.ClientApp ()
--botLoop botNick room closed botFunct conn = do
--        myAgent <- newEmptyMVar
--        started <- getPOSIXTime
--        let botState = BotState conn count myAgent room botNick closed (round started)
--
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
--        sendPacket botState $ Nick botNick
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
--getNextPacket :: MVar Int -> IO Int
--getNextPacket count = do
--            var <- takeMVar count
--            putMVar count (var+1)
--            return var
--
--sendPacket :: BotState -> EuphCommand -> IO ()
--sendPacket botState euphPacket =
--      do
--      seqNum <- getNextPacket $ packetCount botState
--      WS.sendTextData (botConnection botState) $ J.encode (Command seqNum euphPacket)
--
--closeConnection :: BotState -> Bool -> IO ()
--closeConnection botState main =
--  do
--  _ <- tryPutMVar (closedBot botState) main
--  WS.sendClose (botConnection botState) $ T.pack ""
--
--getBotAgent :: BotState -> IO UserData
--getBotAgent botState =
--  do
--  a <- readMVar $ botAgent botState
--  putMVar (botAgent botState) a
--  return a
--
--
--getUptime :: BotState -> Integer -> String
--getUptime bs tt =
--  let myTime = tt - startTime bs
--      hours =  div myTime 3600
--      minutes = div (myTime-hours*3600) 60
--      seconds = (myTime - hours*3600 - minutes*60)
--  in if myTime > 0 then
--     show hours   ++ "h " ++
--     show minutes ++ "m " ++
--     show seconds ++ "s."
--    else
--     "UhOh, negative time?"
