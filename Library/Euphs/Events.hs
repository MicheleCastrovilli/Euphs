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
        PingEvent     { pingTime  :: !Integer -- ^ The sent time
                      , nextTime  :: !Integer -- ^ Next time a ping will be sent
                      }
      | WhoReply      { idReply   :: !Int -- ^ ID based on the command sent
                      , users     :: ![UserData] -- ^ List of sessions connected
                      }
      | LogReply      { idReply   :: !Int
                      , messages  :: ![MessageData] -- ^ List of messages requested
                      }
      | SnapshotEvent { identity  :: !String -- ^  Id of the agent logged
                      , sessionID :: !String -- ^ Global ID of this session
                      , version   :: !String -- ^ Server's version identifier
                      , users     :: ![UserData] -- ^ List of all other sessions , excluding self
                      , messages  :: ![MessageData] -- ^ Up to previous 100 messages in the room
                      }
      | SendEvent     { msgData   :: !MessageData  -- ^ Messsage sent from an user
                      }
      | SendReply     { idReply   :: !Int
                      , msgData   :: !MessageData -- ^ Message sent from the bot
                      }
      | NickEvent     { userData  :: !UserData -- ^ New user agent
                      , fromNick  :: !String -- ^ Previous nick
                      }
      | NickReply     { idReply   :: !Int
                      , userData  :: !UserData -- ^ New bot agent
                      , fromNick  :: !String -- ^ Previous nick
                      }
      | JoinEvent     { userData  :: !UserData  -- ^ Session that joined the room
                      }
      | PartEvent     { userData  :: !UserData  -- ^ Session that left the room
                      }
      | HelloEvent    { userData  :: !UserData  -- ^ Self session
                      , privateRoom :: !Bool -- ^ Indicating whether a room is private or not.
                      , version   :: !String -- ^ Version id of the server
                      }
      | AuthReply     { idReply   :: !Int
                      , success   :: !Bool -- ^ Whether the authentication was successful
                      , reason    :: !String -- ^ Reason of denied auth
                      }
      | BounceEvent   { reason    :: !String -- ^ Reason of bounce event
                      }
      deriving (Show)

-- | JSON parsing instance for Heim events
instance J.FromJSON EuphEvent where
  parseJSON (J.Object v) = do
    msgType <- v J..: "type"
    case msgType :: T.Text of
     "ping-event" ->
          PingEvent     <$> ( v J..: "data" >>= (J..: "time"))
                        <*> ( v J..: "data" >>= (J..: "next"))
     "send-reply" ->
          SendReply     <$> fmap read (v J..: "id")
                        <*> v J..: "data"
     "send-event" ->
          SendEvent     <$> ( v J..: "data" )
     "nick-reply" ->
          NickReply     <$> fmap read (v J..: "id")
                        <*> v J..: "data"
                        <*> (v J..: "data" >>= (\x -> x J..:? "from" J..!=""))
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
                        <*> (v J..: "data" >>= (J..: "from"))
     "who-reply" ->
          WhoReply      <$> fmap read (v J..: "id")
                        <*> (v J..: "data" >>= (J..: "listing"))
     "join-event" ->
          JoinEvent     <$> v J..: "data"
     "part-event" ->
          PartEvent     <$> v J..: "data"
     "log-reply" ->
          LogReply      <$> fmap read (v J..: "id")
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
     "auth-reply" ->
          AuthReply <$> fmap read (v J..: "id")
                    <*> (v J..: "data" >>= (J..: "success"))
                    <*> (v J..: "data" >>= (\x -> x J..:? "reason" J..!= ""))
     "bounce-event" ->
          BounceEvent <$> (v J..: "data" >>= (J..: "reason"))
     _ -> mzero
  parseJSON _ = mzero

-- | Function to decode a network packet to an Euphorian event
decodePacket :: B.ByteString -> Either String EuphEvent
decodePacket = J.eitherDecode'

-- | Function to encode an Euphorian command to a network packet
encodePacket :: SentCommand -> B.ByteString
encodePacket = J.encode

-- | Function to match a Reply with the command
matchIdReply :: Int -> EuphEvent -> Bool
matchIdReply i (SendReply s _) = s == i
matchIdReply i (NickReply s _ _) = s == i
matchIdReply i (WhoReply s _) = s == i
matchIdReply i (LogReply s _) = s == i
matchIdReply i (AuthReply s _ _) = s == i
matchIdReply _ _ = False
