{-# LANGUAGE OverloadedStrings #-}
-- | Module Declaring all the possible events the bot can receive during a connection, while putting them into a structure.
module Euphs.Events where

import qualified Data.Aeson              as J
import qualified Data.Text               as T
import qualified Data.ByteString.Lazy    as B
import           Control.Monad
import           Euphs.Types
import           Euphs.Commands

-- | The main Event structure, every JSON reply gets parsed into this structure, and then passed to the custom bot function.
data EuphEvent =
        PingEvent     { pingTime  :: Integer
                      , nextTime  :: Integer
                      }
      | WhoReply      { idReply   :: Int
                      , users     :: [UserData]
                      }
      | LogReply      { idReply   :: Int
                      , messages  :: [MessageData] }
      | SnapshotEvent { identity  :: String
                      , sessionID :: String
                      , version   :: String
                      , users     :: [UserData]
                      , messages  :: [MessageData]
                      }
      | SendEvent     { msgData   :: MessageData }
      | SendReply     { idReply   :: Int
                      , msgData   :: MessageData }
      | NickEvent     { userData  :: UserData
                      , fromNick  :: String
                      }
      | NickReply     { idReply   :: Int
                      , userData  :: UserData
                      , fromNick  :: String
                      }
      | JoinEvent     { userData  :: UserData }
      | PartEvent     { userData  :: UserData }
      | HelloEvent    { userData  :: UserData
                      , privateRoom :: Bool
                      , version   :: String
                      }
      deriving (Show)


instance J.FromJSON EuphEvent where
  parseJSON (J.Object v) = do
    msgType <- v J..: "type"
    case msgType :: T.Text of
     "ping-event" ->
          PingEvent     <$> ( v J..: "data" >>= (J..: "time"))
                        <*> ( v J..: "data" >>= (J..: "next"))
     "send-reply" ->
          SendReply     <$> (fmap read $ v J..: "id")
                        <*> v J..: "data"
     "send-event" ->
          SendEvent     <$> ( v J..: "data" )
     "nick-reply" ->
          NickReply     <$> (fmap read $ v J..: "id")
                        <*> v J..: "data"
                        <*> (v J..:? "from" J..!="")
                          -- (UserData <$> ( v J..: i"data" >>= (J..: "id"))
                          --            <*> ( v J..: "data" >>= (J..: "to"))
                          --            <*> return ""
                          --            <*> return ""
                          --            <*> ( v J..: "data" >>= (J..: "session_id"))
                          --  )
     "nick-event" ->
          NickEvent     <$> v J..: "data"
                           -- (UserData <$> ( v J..: "data" >>= (J..: "id"))
                           --           <*> ( v J..: "data" >>= (J..: "to"))
                           --           <*> return ""
                           --           <*> return ""
                           --           <*> ( v J..: "data" >>= (J..: "session_id"))
                           -- )
                        <*> v J..: "from"
     "who-reply" ->
          WhoReply      <$> (fmap read $ v J..: "id")
                        <*> (v J..: "data" >>= (J..: "listing"))
     "join-event" ->
          JoinEvent     <$> v J..: "data"
     "part-event" ->
          PartEvent     <$> v J..: "data"
     "log-reply" ->
          LogReply      <$> (fmap read $ v J..: "id")
                        <*> v J..: "data"
     "hello-event" ->
          HelloEvent <$> (v J..: "data" >>= (J..: "session"))
                     <*> (v J..: "data" >>= (J..: "room_is_private"))
                     <*> (v J..: "data" >>= (J..: "version"))
     "snapshot-event" ->
          SnapshotEvent <$> (v J..: "data" >>= (J..: "identity"))
                        <*> (v J..: "data" >>= (J..: "session_id"))
                        <*> (v J..: "data" >>= (J..: "version"))
                        <*> (v J..: "data" >>= (J..: "listing"))
                        <*> (v J..: "data" >>= (J..: "log"))
     _ -> mzero
  parseJSON _ = mzero


decodePacket :: B.ByteString -> Either String EuphEvent
decodePacket = J.eitherDecode'

encodePacket :: SentCommand -> B.ByteString
encodePacket = J.encode

matchIdReply :: Int -> EuphEvent -> Bool
matchIdReply i (SendReply s _) = s == i
matchIdReply i (NickReply s _ _) = s == i
matchIdReply i (WhoReply s _) = s == i
matchIdReply i (LogReply s _) = s == i
matchIdReply _ _ = False
