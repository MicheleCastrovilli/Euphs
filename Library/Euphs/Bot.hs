{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Euphoria.Bot (
  euphoriaBot,
  sendPacket,
  closeConnection,
  closedBot,
  getBotAgent,
  botName,
  botRoom,
  BotName,
  RoomName,
  BotAgent,
  BotFunction,
  BotState
) where

import qualified Network.WebSockets          as WS
import qualified Network.WebSockets.Stream   as WSS
import qualified Network.Socket              as S
import qualified OpenSSL                     as SSL
import qualified OpenSSL.Session             as SSL
import qualified System.IO.Streams.SSL       as Streams
import qualified System.IO.Streams.Internal  as StreamsIO
import qualified Data.ByteString.Lazy        as B
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Aeson                  as J
import           Data.Char                  (isSpace)
import           Data.List
import           Control.Exception           --(finally, catch, SomeException
import           Control.Monad.Trans         (liftIO)
import           Control.Monad
import           Data.Time.Clock.POSIX
import           Control.Concurrent
import           Euphoria.Events
import           Euphoria.Commands
import           Euphoria.Types


myHost :: String
myHost = "euphoria.io"
--myHost = "localhost"
myPort :: Int
myPort = 443
--myPort = 8080
myPathBef :: String
myPathBef = "/room/"
myPathAft :: String
myPathAft = "/ws"

type PacketID = MVar Int
type BotName = String
type RoomName = String
type BotAgent = MVar UserData
type BotFunction = BotState -> EuphEvent -> IO ()


data BotState = BotState {
  botConnection :: WS.Connection,
  packetCount   :: PacketID,
  botAgent      :: BotAgent,
  botRoom       :: String,
  botName       :: String,
  closedBot     :: MVar Bool,
  startTime     :: Integer
}

euphoriaBot :: BotName -> RoomName -> BotFunction -> IO ()
euphoriaBot botNick room botFunction = SSL.withOpenSSL $ do
    ctx <- SSL.context
    is <- S.getAddrInfo Nothing (Just myHost) (Just $ show myPort)
    let addr = S.addrAddress $ head is
        fam = S.addrFamily $ head is
    s <- S.socket fam S.Stream S.defaultProtocol
    S.connect s addr
    ssl <- SSL.connection ctx s
    SSL.connect ssl
    (i,o) <- Streams.sslToStreams ssl
    myStream <- WSS.makeStream (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o)
    closed <- newEmptyMVar
    finally
      (WS.runClientWithStream myStream myHost (myPathBef ++ room ++ myPathAft) WS.defaultConnectionOptions [] $ botLoop botNick room closed botFunction)
      (void $ tryPutMVar closed True)

botLoop :: BotName -> RoomName -> MVar Bool -> BotFunction -> WS.ClientApp ()
botLoop botNick room closed botFunct conn = do
        count <- newMVar 1
        myAgent <- newEmptyMVar
        started <- getPOSIXTime
        let botState = BotState conn count myAgent room botNick closed (round started)

        _ <- forkIO $ catch ( forever (
          do
          msg <- WS.receiveData conn :: IO T.Text
          let evt = J.decode (WS.toLazyByteString msg) :: Maybe EuphEvent
          --liftIO $ T.putStrLn $ maybe  (T.append "Can't parse this : " msg) (T.pack . show) evt
          case evt of
            Just (PingEvent _ _) -> do
                                           {-putStrLn "PING!"-}
                                           time <- getPOSIXTime
                                           sendPacket botState (PingReply $ round time)
            Just (NickReply _ user)   ->  putMVar myAgent user
            Just (SendEvent (MessageData _ mesgID _ _ (stripPrefix ("!uptime @" ++ botNick)  -> Just r) _ _)) ->
                 getPOSIXTime >>= (\x -> sendPacket botState (Send ("Been up  for " ++ getUptime botState (round x)) mesgID))
            Just (SendEvent (MessageData _ mesgID _ _ (stripPrefix ("!ping @" ++ botNick) -> Just _) _ _)) -> sendPacket botState (Send "Pong!" mesgID)
            Just (SendEvent (MessageData _ mesgID _ _ (stripPrefix "!ping" -> Just r) _ _)) -> when (null $ filter (not .isSpace) r) $ sendPacket botState (Send "Pong!" mesgID)
            Just x                    ->  void $ forkIO $ botFunct botState x
            Nothing                   ->  return ()
          )) (\ (SomeException _) -> closeConnection botState True )

        sendPacket botState $ Nick botNick
        putStrLn $ "Connected to Euphoria! With nick: " ++ botNick ++ " and in the room: " ++ botRoom botState

        let loop x = if x then return () else loop x in readMVar closed >>= loop
        void $ threadDelay 1000000
        {-forkIO $ forever (-}
            {-do-}
            {-a <- timeout 1000000 $ readChan timeoutChan-}
            {-t <- getPOSIXTime-}
            {-case a of -}
            {-Nothing -> closeConnection botState-}
            {-Just timed -> do-}
                          {-putStrLn $ "PONG! " ++ ( show ( fromInteger timed - round t))-}
                          {-threadDelay (1000000*(fromInteger timed - round t))-}
                          {-return ()-}
                          {-)-}




getNextPacket :: MVar Int -> IO Int
getNextPacket count = do
            var <- takeMVar count
            putMVar count (var+1)
            return var

sendPacket :: BotState -> EuphCommand -> IO ()
sendPacket botState euphPacket =
      do
      seqNum <- getNextPacket $ packetCount botState
      WS.sendTextData (botConnection botState) $ J.encode (Command seqNum euphPacket)

closeConnection :: BotState -> Bool -> IO ()
closeConnection botState main =
  do
  _ <- tryPutMVar (closedBot botState) main
  WS.sendClose (botConnection botState) $ T.pack ""

getBotAgent :: BotState -> IO UserData
getBotAgent botState =
  do
  a <- readMVar $ botAgent botState
  putMVar (botAgent botState) a
  return a


getUptime :: BotState -> Integer -> String
getUptime bs tt =
  let myTime = tt - startTime bs
      hours =  div myTime 3600
      minutes = div (myTime-hours*3600) 60
      seconds = (myTime - hours*3600 - minutes*60)
  in if myTime > 0 then
     show hours   ++ "h " ++
     show minutes ++ "m " ++
     show seconds ++ "s."
    else
     "UhOh, negative time?"
