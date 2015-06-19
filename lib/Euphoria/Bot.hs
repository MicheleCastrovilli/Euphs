module Euphoria.Bot where

import qualified  Network.WebSockets          as WS
import qualified  Network.WebSockets.Stream   as WSS
import qualified  Network.Socket              as S
import qualified  OpenSSL                     as SSL
import qualified  OpenSSL.Session             as SSL
import qualified  System.IO.Streams.SSL       as Streams
import qualified  System.IO.Streams.Internal  as StreamsIO
import qualified  Data.ByteString.Lazy        as B

myHost = "euphoria.io"
myPort = 443
myPathBef = "/room/"
myPathAft = "/ws"

data BotState = BotState

type BotName = String
type RoomName = String
type BotFunction = BotState -> IO ()

euphBot :: BotName -> RoomName -> BotFunction -> IO ()
euphBot room botName function = SSL.withOpenSSL $ do 
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
    WS.runClientWithStream myStream myHost (myPathBef ++ room ++ myPathAft) WS.defaultConnectionOptions [] app

botLoop :: BotName -> BotFunction -> WS.ClientApp ()
bot botName botFunct conn = do
        count <- newEmptyMVar
        putMVar count 1
        myAgent <- newEmptyMVar
        putStrLn "Connected!"
        forkIO $ forever $ do
          msg <- WS.receiveData conn :: IO T.Text
          let evt = J.decode (WS.toLazyByteString msg) :: Maybe EuphEvent
          liftIO $ T.putStrLn $ maybe  (T.append "Can't parse this : " msg) (T.pack . show) evt
          case evt of
            Just (PingEvent _ _) ->  do
                              seq <- getNext count
                              reply <- makePingReply seq
                              WS.sendTextData conn reply
            Just (NickEvent id user) -> putMVar myAgent user
            Just x  -> (forkIO $ myFunct conn count myAgent x) >> return ()
            Nothing -> return ()
        
        seq <- getNext count
        WS.sendTextData conn (nickRequest nick seq)

        let loop = do 
                   line <- getLine
                   packetId <- getNext count
                   unless (null line) $ WS.sendTextData conn (J.encode (Send packetId line "")) >> loop

        loop
        WS.sendClose conn (T.pack "Bye!" )

getNext count = do 
            var <- takeMVar count
            putMVar count (var+1)
            return var

