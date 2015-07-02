{-# LANGUAGE OverloadedStrings #-}

module Euphoria.Bot (
  euphoriaBot,
  sendPacket,
  closeConnection,
  getBotAgent,
  botName,
  BotName,
  RoomName,
  BotAgent,
  BotFunction,
  BotState,
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
  botName       :: String
}

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
               WS.defaultConnectionOptions [] $ botLoop botName room botFunction

botLoop :: BotName -> RoomName -> BotFunction -> WS.ClientApp ()
botLoop botName botRoom botFunct conn = do
        count <- newMVar 1
        myAgent <- newEmptyMVar
        putStrLn "Connected!"
        let botState = BotState conn count myAgent botRoom botName
        forkIO $ forever $ do
          msg <- WS.receiveData conn :: IO T.Text
          let evt = J.decode (WS.toLazyByteString msg) :: Maybe EuphEvent
          liftIO $ T.putStrLn $ maybe  (T.append "Can't parse this : " msg) (T.pack . show) evt
          case evt of
            Just (PingEvent _ _)      ->  do
                                          time <- getPOSIXTime 
                                          sendPacket botState (PingReply $ round time)
            Just (SendEvent (MessageData _ msgID _ _ "!ping" _ _)) -> sendPacket botState (Send "Pong!" msgID)
            Just (NickReply _ user)   ->  putMVar myAgent user
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
sendPacket botState euphPacket =
      do
      seqNum <- getNextPacket $ packetCount botState
      WS.sendTextData (botConnection botState) $ J.encode (Command seqNum euphPacket)

closeConnection :: BotState -> IO ()
closeConnection botState =
  WS.sendClose (botConnection botState) $ T.pack ""

getBotAgent :: BotState -> IO UserData
getBotAgent botState =
  do
  a <- readMVar $ botAgent botState
  return a
