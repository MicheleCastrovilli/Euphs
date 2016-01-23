{-# LANGUAGE OverloadedStrings #-}
-- | This module defines the Types and structure of the packets incoming.
-- | Also look at the official <https://github.com/euphoria-io/heim/blob/master/doc/api.md api>.
module Euphs.Types where

import qualified Data.Aeson                 as J
import qualified Data.Text               as T
import qualified Data.ByteString.Lazy    as B
import           Control.Monad
import           Control.Applicative ((<|>))

-- | A type synonym for Snowflake.
type MessageID = String
type UserID = String
-- | Structure representing an agent, user or bot. Also called SessionView, in the api.
data UserData = UserData { userID     :: String -- ^ ID of the user
    , name       :: String -- ^ Current nick of the user
    , serverID   :: String -- ^ The id of the server
    , serverEra  :: String -- ^ The era of the server
    , sessionID  :: String -- ^ ID of the session, unique across all sessions, globally
    , isStaff    :: Bool   -- ^ Whether the session, is a member of the staff
    , isManager  :: Bool   -- ^ Whether the session, is a manager of the room
    } deriving (Show, Read)

-- | Instance for Eq, based on the agentID
instance Eq UserData where
    x == y = (userID x) == (userID y)

-- | Instance for Eq, based on the agentID
instance Ord UserData where
    compare x y = compare (userID x) (userID y)

-- | Structure representing a Message incoming from the server.
data MessageData = MessageData { timeRecieved :: Integer --
    , msgID        :: MessageID -- ID of the message
    , parentMsg    :: String -- ID of the message's parent, or null
    , prevEditID   :: String -- ID of the message's previous edit, or null
    , sender       :: UserData -- View of the sender's session
    , contentMsg   :: String -- Content of the message.
    , encryptKey   :: String -- ID of the key that encrypts the message
    , edited       :: Maybe Integer -- Unix Timestamp of when the message was edited
    , deleted      :: Maybe Integer -- Unix Timestamp of when the message was deleted
    , truncated    :: Bool -- Full content of the msg not included
    } deriving (Show)

-- | Aeson instance for parsing Message
instance J.FromJSON MessageData where
  parseJSON (J.Object v) =
      MessageData <$> (v J..: "time")
                  <*> (v J..: "id")
                  <*> (v J..:? "parent" J..!="")
                  <*> (v J..:? "previous_edit_id" J..!="")
                  <*> (v J..: "sender")
                  <*> (v J..: "content")
                  <*> (v J..:? "encryption_key_id" J..!="")
                  <*> (v J..:? "edited" J..!=Nothing)
                  <*> (v J..:? "deleted" J..!=Nothing)
                  <*> (v J..:? "truncated" J..!=False)
  parseJSON _ = fail "Not a valid Message JSON"

-- | Aeson instance for parsing a SessionView
instance J.FromJSON UserData where
  parseJSON (J.Object v) =
      UserData <$> (v J..: "id")
               <*> (v J..: "name" <|> v J..: "to")
               <*> (v J..:? "server_id" J..!= "")
               <*> (v J..:? "server_era" J..!= "")
               <*> (v J..: "session_id")
               <*> (v J..:? "is_staff" J..!= False)
               <*> (v J..:? "is_manager" J..!= False)
  parseJSON _ = fail "Not a valid SessionView JSON"

---------------------------------------- EVENTS ----------------------------------------

-- | The main Event structure
data EuphsPacket = EuphsEvt {
    packetEvt :: EuphEvent -- ^ The Event the packet encapsulates
  }
  | EuphsReply {
    idPacket :: String -- ^ The ID of the reply
  , packetRpl :: EuphReply -- ^ The reply encapsulated
  , throttled :: Maybe String -- ^ A throttled warning
  }
  | EuphsError {
    idPacketMay :: Maybe String -- ^ If the error is referring to a reply
  , errorReason :: String -- ^ The reason for the error
  }
  deriving Show

-- | Asynchronous events recieved from Heim
data EuphEvent =
        PingEvent     { pingTime :: !Integer -- ^ The sent time
                      , nextTime :: !Integer -- ^ Next time a ping will be sent
                      }
      | SnapshotEvent { identity :: !String -- ^  Id of the agent logged
                      , sessionID :: !String -- ^ Global ID of this session
                      , version :: !String -- ^ Server's version identifier
                      , users :: ![UserData] -- ^ List of all other sessions , excluding self
                      , messages :: ![MessageData] -- ^ Up to previous 100 messages in the room
                      }
      | SendEvent     { msgData :: !MessageData  -- ^ Messsage sent from an user
                      }
      | NickEvent     { userData :: !UserData -- ^ New user agent
                      , fromNick :: !String -- ^ Previous nick
                      }
      | JoinEvent     { userData :: !UserData  -- ^ Session that joined the room
                      }
      | PartEvent     { userData :: !UserData  -- ^ Session that left the room
                      }
      | HelloEvent    { userData :: !UserData  -- ^ Self session
                      , privateRoom :: !Bool -- ^ Indicating whether a room is private or not.
                      , serverVersion :: !String -- ^ Version id of the server
                      }
      | BounceEvent   { bounceReason :: !String -- ^ Reason of bounce event
      }
      | NetworkEvent  { networkType :: !String -- ^ Reason for the network event
                      }
      | PMInitEvent   { userData :: !UserData
                      , fromNick :: !String
                      , fromRoom :: !String
                      , pmID :: !MessageID
                      }
      deriving (Show)

-- | Replies to command sent by the client
data EuphReply =
        GetMessageReply { getMsg :: !Message -- ^ Retrieved message of the requested ID
                        }
      | WhoReply        { whoUsers :: ![SessionView] -- ^ List of sessions connected
                        }
      | LogReply        { logMessages :: ![Message] -- ^ List of messages requested
                        , logBefore :: Snowflake -- ^  Messages prior to this snowflake returned
                        }
      | SendReply       { sendReply :: !Message -- ^ Message sent from the bot
                        }
      | NickReply       { nickSessionId :: !String
                        , nickUserData :: !UserData -- ^ New bot agent
                        , nickFrom :: !String -- ^ Previous nick
                        , nickTo :: !String
                        }
      | AuthReply       { authSuccess :: !Bool -- ^ Whether the authentication was successful
                        , authReason :: !String -- ^ Reason for the denied authentication
                        }
      | PMInitReply     { pmrID :: !Snowflake
                        , pmrToNick :: !String
                        }

    deriving Show

instance J.FromJSON EuphsPacket where
    parseJSON p@(J.Object v) = do
                               msgType <- v J..: "type" :: Parsed T.Text
                               let pktType = reverse . takeWhile (/='-') . reverse msgType
                               let parsed =  case pktType of
                                                "reply" -> parseReply p
                                                _ -> parseEvt p
                               parseError p <|> parsed
    parseJSON x = J.typeMismatch "EuphsPacket" x

parseReply :: J.Value -> Parser EuphsPacket
parseReply (J.Object v) = do
    msgType <- v J..: "type"
    packet <- case msgType :: T.Text of
        "send-reply" -> SendReply <$> fmap read (v J..: "id")
                        <*> (v J..: "data")
        "nick-reply" -> NickReply <$> fmap read (v J..: "id")
                        <*> (v J..: "data")
                        <*> (v J..: "data" >>= (\x -> x J..:? "from" J..!=""))
        "who-reply" ->  WhoReply <$> fmap read (v J..: "id")
                        <*> (v J..: "data" >>= (J..: "listing"))
        "log-reply" ->  LogReply <$> fmap read (v J..: "id")
                        <*> v J..: "data"
        "auth-reply" -> AuthReply <$> fmap read (v J..: "id")
                        <*> (v J..: "data" >>= (J..: "success"))
                        <*> (v J..: "data" >>= (\x -> x J..:? "reason" J..!= ""))
        "pm-initiate-reply" -> PMInitReply  <$> (v J..: "data" >>= (J..: "pm_id"))
                               <*> (v J..: "data" >>= (J..: "to_nick"))
        "get-message-reply" -> GetMessageReply <$> (v J..: "data")
        _ -> fail "No suitable reply found"
    idPkt <- v J..: "id"
    thrReas <- v J..:? "throttled_reason"
    return (EuphsReply idPkt packet thrReas)
parseReply x = J.typeMismatch "EuphsPacket" x

parseEvt :: J.Value -> Parser EuphsPacket
parseEvt (J.Object v) = do
    msgType <- v J..: "type"
    pkt <- case msgType :: T.Text of
        "ping-event" -> PingEvent <$> ( v J..: "data" >>= (J..: "time"))
                        <*> ( v J..: "data" >>= (J..: "next"))
        "send-event" -> SendEvent <$> ( v J..: "data" )
        "nick-event" -> NickEvent <$> v J..: "data"
                        <*> (v J..: "data" >>= (J..: "from"))
        "join-event" -> JoinEvent <$> v J..: "data"
        "part-event" -> PartEvent <$> v J..: "data"
        "hello-event" -> HelloEvent <$> (v J..: "data" >>= (J..: "session"))
                         <*> (v J..: "data" >>= (J..: "room_is_private"))
                         <*> (v J..: "data" >>= (J..: "version"))
        "snapshot-event" -> SnapshotEvent <$> (v J..: "data" >>= (J..: "identity"))
                            <*> (v J..: "data" >>= (J..: "session_id"))
                            <*> (v J..: "data" >>= (J..: "version"))
                            <*> (v J..: "data" >>= (J..: "listing"))
                            <*> (v J..: "data" >>= (J..: "log"))
        "bounce-event" -> BounceEvent <$> (v J..: "data" >>= (J..: "reason"))
        "pm-initiate-event" -> PMInitEvent <$> (v J..: "data" >>= (J..: "from"))
                               <*> (v J..: "data" >>= (J..: "from_nick"))
                               <*> (v J..: "data" >>= (J..: "from_room"))
                               <*> (v J..: "data" >>= (J..: "pm_id"))
        "network-event" -> NetworkEvent <$> (v J..: "data" >>= (J..: "type"))
        _ -> fail "No suitable event found"
    return $ EuphsEvt pkt
parseEvt x = J.typeMismatch "EuphsPacket" x

parseError :: J.Value -> Parser EuphsPacket
parseError (J.Object v) = do
    msgId <- v J..:? "id"
    errorMsg <- v J..: "error"
    return $ EuphsError msgId errorMsg
parseError x = J.typeMismatch "EuphsPacket" x

-- | Function to decode a network packet to an Euphorian event
decodePacket :: B.ByteString -> Either String EuphPacket
decodePacket = J.eitherDecode'

-- | Function to encode an Euphorian command to a network packet
encodePacket :: SentCommand -> B.ByteString
encodePacket = J.encode

-- | Function to match a Reply with the command
matchIdReply :: String -> EuphPacket -> Bool
matchIdReply i (EuphsError x _ ) = maybe False (==i) x
matchIdReply i (EuphsReply s _ _) = s == i

---------------------------------------- COMMANDS ----------------------------------------

-- | Generic structure for sending a command.
data SentCommand =
    Command
      { commandID   :: Int -- ^ ID of the packet to send the server
      , commandData :: EuphCommand -- ^ Proper packet
      }
  | PingCommand { timeReply :: Integer }
    deriving (Show)
-- | Option type for determining the Authentication Type
data AuthOption = AuthPasscode deriving (Eq, Show)

instance J.ToJSON AuthOption where
  toJSON AuthPasscode = "passcode"

-- | Types of commands
data EuphCommand =
    Who -- | Requests a list of sessions connected to the room
  | Log           { nMsg         :: Int -- ^ Number of messages to request
                  , beforeMsg    :: String  -- ^ ID from when to request
                  } -- ^ Requests a log of the most recent messages
  | Send          { contentSend  :: String -- ^ Message to send
                  , parentMsg    :: String -- ^ MessageID of the reply, empty for root
                  } -- ^ Sends a message to the Room
  | Nick          { nick         :: String -- ^ Nick to change to.
                  } -- ^ Requests a new nick.
  | GetMessage    { idGet        :: MessageID -- ^ The Snowflake of the message to get
                  } --  ^ Fully gets a message
  | Auth          { authType     :: AuthOption -- ^ Method of Authentication
                  , passcode     :: String -- ^ Passcode authentication
                  }
  | PMInit        { userID      :: UserID
                  }
  deriving (Eq,Show)

instance J.ToJSON SentCommand where
  toJSON (Command idCommand dataCommand) = J.object ( "id" J..= show idCommand  : inPair dataCommand)
  toJSON (PingCommand time) = J.object (["type" J..= ("ping-reply" :: J.Value) ,"data" J..= J.object [ "time" J..= time ]])

-- | An internal function, to work with the JSON data
inPair :: EuphCommand -> [(T.Text, J.Value)]
inPair (Nick nickname) =
  [
    ("type" , "nick" ),
    ("data" , J.object [ "name"  J..= nickname])
  ]
inPair (Send content parent) =
  [
    ("type" , "send" ),
    ("data" , J.object [
        "content" J..= content,
        "parent"  J..= parent
      ]
    )
  ]
inPair (Who) =
  [
    ("type", "who")
  ]
inPair (Log num beforeID) =
  [
    ("type", "log"),
    ("data", J.object [
        "n" J..= num,
        "before" J..= beforeID
      ]
    )
  ]
inPair (GetMessage idGot) =
    [
      ("type", "get-message"),
      ("data", J.object [
          "id" J..= idGot
        ]
      )
    ]
inPair (Auth t p) =
    [
      ("type", "auth"),
      ("data", J.object [
          "type" J..= t
        , "passcode" J..= p
        ]
      )
    ]
inPair (PMInit u) =
    [ ("type", "pm-initiate")
    , ("data", J.object [
        "user_id" J..= u
        ]
      )
    ]
