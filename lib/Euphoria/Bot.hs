{-# LANGUAGE OverloadedStrings #-}

module Euphoria.Bot (
  euphoriaBot,
  sendPacket,
  closeConnection,
  BotName,
  RoomName,
  BotAgent,
  BotFunction,
  BotState,
  PacketId,
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
import           Control.Monad.Trans         (liftIO)
import           Control.Monad
import           Data.Time.Clock.POSIX
import           Control.Concurrent
import           Euphoria.Events
import           Euphoria.Commands
import           Euphoria.Types

myHost :: String
myHost = "euphoria.io"
myPort :: Int
myPort = 443
myPathBef :: String
myPathBef = "/room/"
myPathAft :: String
myPathAft = "/ws"

data BotState = BotState WS.Connection PacketId BotAgent 

type PacketId = MVar Int
type BotName = String
type RoomName = String
type BotAgent = MVar UserData
type BotFunction = BotState -> EuphEvent -> IO ()

euphoriaBot :: BotName -> RoomName -> BotFunction -> IO ()
euphoriaBot botName room botFunction = SSL.withOpenSSL $ do 
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
    WS.runClientWithStream myStream myHost (myPathBef ++ room ++ myPathAft)
               WS.defaultConnectionOptions [] $ botLoop botName botFunction

botLoop :: BotName -> BotFunction -> WS.ClientApp ()
botLoop botName botFunct conn = do
        count <- newMVar 1
        myAgent <- newEmptyMVar
        putStrLn "Connected!"
        let botState = BotState conn count myAgent
        forkIO $ forever $ do
          msg <- WS.receiveData conn :: IO T.Text
          let evt = J.decode (WS.toLazyByteString msg) :: Maybe EuphEvent
          liftIO $ T.putStrLn $ maybe  (T.append "Can't parse this : " msg) (T.pack . show) evt
          case evt of
            Just (PingEvent _ _)      ->  do
                                          time <- getPOSIXTime 
                                          sendPacket botState (PingReply $ round time)
            Just (SendEvent (MessageData _ msgID _ _ "!ping" _ _)) -> sendPacket botState (Send "Pong!" msgID)
            Just (NickReply _ user)  ->  putMVar myAgent user
            Just x                    ->  void $ forkIO $ botFunct botState x
            Nothing                   ->  return ()
        
        sendPacket botState $ Nick botName

        let loop = do 
                   line <- getLine
                   unless (null line) $ sendPacket botState (Send line "") >> loop

        loop
        WS.sendClose conn (T.pack "Bye!" )

getNextPacket :: MVar Int -> IO Int
getNextPacket count = do 
            var <- takeMVar count
            putMVar count (var+1)
            return var

sendPacket :: BotState -> EuphCommand -> IO ()
sendPacket (BotState conn packetCounter _) euphPacket =
      do
      seqNum <- getNextPacket packetCounter
      WS.sendTextData conn $ J.encode (Command seqNum euphPacket)

closeConnection :: BotState -> IO ()
closeConnection (BotState conn _ _) =
  WS.sendClose conn $ T.pack ""
