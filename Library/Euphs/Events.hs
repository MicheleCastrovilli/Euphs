{-# LANGUAGE OverloadedStrings #-}

module Euphoria.Events where

import qualified Data.Aeson              as J
import qualified Data.Text               as T
import           Control.Monad
import           Euphoria.Types

data EuphEvent =
        PingEvent     { pingTime  :: Integer,
                        nextTime  :: Integer}
      | WhoReply      { idResp    :: String,
                        users     :: [UserData]
                      }
      | LogReply      { messages  :: [MessageData] }
      | SnapshotEvent { identity  :: String,
                        sessionID :: String,
                        version   :: String,
                        users     :: [UserData],
                        messages  :: [MessageData]
                      }
      | SendEvent     { msgData   :: MessageData }
      | SendReply     { msgData   :: MessageData }
      | NickEvent     { userData  :: UserData,
                        fromNick  :: String
                      }
      | NickReply     { idResp    :: String,
                        userData  :: UserData }
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
          SendReply     <$> ( v J..: "data" )
     "send-event" ->
          SendEvent     <$> ( v J..: "data" )
     "nick-reply" ->
          NickReply     <$>  v J..: "id"
                        <*> (UserData <$> ( v J..: "data" >>= (J..: "id"))
                                      <*> ( v J..: "data" >>= (J..: "to"))
                                      <*> return ""
                                      <*> return ""
                                      <*> ( v J..: "data" >>= (J..: "session_id"))
                            )
     "nick-event" ->
          NickEvent     <$> (UserData <$> ( v J..: "data" >>= (J..: "id"))
                                      <*> ( v J..: "data" >>= (J..: "to"))
                                      <*> return ""
                                      <*> return ""
                                      <*> ( v J..: "data" >>= (J..: "session_id"))
                            )
                        <*> ( v J..: "data" >>= (J..: "from"))
     "who-reply" ->
          WhoReply      <$> v J..: "id"
                        <*> (v J..: "data" >>= (J..: "listing"))
     "join-event" ->
          JoinEvent     <$> v J..: "data"
     "part-event" ->
          PartEvent     <$> v J..: "data"
     "log-reply" ->
          LogReply      <$> v J..: "data"
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
