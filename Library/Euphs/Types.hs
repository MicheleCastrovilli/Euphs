{-# LANGUAGE OverloadedStrings #-}
-- | This module defines the Types and structure of the packets incoming.
-- | Also look at the official <https://github.com/euphoria-io/heim/blob/master/doc/api.md api>.
module Euphs.Types where

import qualified Data.Aeson                  as J
import qualified Data.Aeson.Types            as JT
import qualified Data.Text                   as T
import qualified Data.ByteString.Lazy        as B
import           Control.Applicative ((<|>))

-- | A type synonym for Snowflake.
type Snowflake = String
type UserID = String
-- | Structure representing an agent, user or bot. Also called SessionView, in the api.
data SessionView = SessionView {
      userID :: UserID -- ^ ID of the user
    , userName :: String -- ^ Current nick of the user
    , userServerID :: String -- ^ The id of the server
    , userServerEra :: String -- ^ The era of the server
    , userSessionID :: Snowflake -- ^ ID of the session, unique across all sessions, globally
    , userIsStaff :: Bool   -- ^ Whether the session, is a member of the staff
    , userIsManager :: Bool   -- ^ Whether the session, is a manager of the room
    } deriving (Show, Read)

-- | Instance for Eq, based on the agentID
instance Eq SessionView where
    x == y = (userID x) == (userID y)

-- | Instance for Eq, based on the agentID
instance Ord SessionView where
    compare x y = compare (userID x) (userID y)

-- | Structure representing a Message incoming from the server.
data Message = Message { timeRecieved :: Integer --
    , msgID :: Snowflake -- ID of the message
    , msgParentMsg :: Snowflake -- ID of the message's parent, or null
    , msgPrevEditID :: Snowflake -- ID of the message's previous edit, or null
    , msgSender :: SessionView -- View of the sender's session
    , msgContentMsg :: String -- Content of the message.
    , msgEncryptKey :: String -- ID of the key that encrypts the message
    , msgEdited :: Maybe Integer -- Unix Timestamp of when the message was edited
    , msgDeleted :: Maybe Integer -- Unix Timestamp of when the message was deleted
    , msgTruncated :: Bool -- Full content of the msg not included
    } deriving (Show)

-- | Aeson instance for parsing Message
instance J.FromJSON Message where
  parseJSON (J.Object v) =
      Message <$> (v J..: "time")
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
instance J.FromJSON SessionView where
  parseJSON (J.Object v) =
      SessionView  <$> (v J..: "id")
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
    pktEvt :: EuphEvent -- ^ The Event the packet encapsulates
  }
  | EuphsReply {
    pktID :: String -- ^ The ID of the reply
  , pktReply :: EuphReply -- ^ The reply encapsulated
  , pktThrottled :: Maybe String -- ^ A throttled warning
  }
  | EuphsError {
    pktIDMay :: Maybe String -- ^ If the error is referring to a reply
  , pktErrorReason :: String -- ^ The reason for the error
  }
  deriving Show

-- | Asynchronous events recieved from Heim
data EuphEvent =
        PingEvent     { pingTime :: !Integer -- ^ The sent time
                      , nextPingTime :: !Integer -- ^ Next time a ping will be sent
                      }
      | SnapshotEvent { snapIdentity :: !String -- ^  Id of the agent logged
                      , snapSessionID :: !String -- ^ Global ID of this session
                      , snapVersion :: !String -- ^ Server's version identifier
                      , snapUsers :: ![SessionView] -- ^ List of all other sessions , excluding self
                      , snapMessages :: ![Message] -- ^ Up to previous 100 messages in the room
                      }
      | SendEvent     { sentMsg :: !Message -- ^ Messsage sent from an user
                      }
      | NickEvent     { userData :: !SessionView -- ^ New user agent
                      , neFromNick :: !String -- ^ Previous nick
                      }
      | JoinEvent     { userData :: !SessionView  -- ^ Session that joined the room
                      }
      | PartEvent     { userData :: !SessionView  -- ^ Session that left the room
                      }
      | HelloEvent    { userData :: !SessionView  -- ^ Self session
                      , privateRoom :: !Bool -- ^ Indicating whether a room is private or not.
                      , serverVersion :: !String -- ^ Version id of the server
                      }
      | BounceEvent   { bounceReason :: !String -- ^ Reason of bounce event
      }
      | NetworkEvent  { networkType :: !String -- ^ Reason for the network event
                      }
      | PMInitEvent   { userData :: !SessionView
                      , fromNick :: !String
                      , fromRoom :: !String
                      , pmID :: !Snowflake
                      }
      deriving (Show)

-- | Replies to command sent by the client
data EuphReply =
        AuthReply       { authSuccess :: !Bool -- ^ Whether the authentication was successful
                        , authReason :: !String -- ^ Reason for the denied authentication
                        }
      | GetMessageReply { getMsg :: !Message -- ^ Retrieved message of the requested ID
                        }
      | LogReply        { logMessages :: ![Message] -- ^ List of messages requested
                        , logBefore :: Maybe Snowflake -- ^  Messages prior to this snowflake returned
                        }
      | NickReply       { nickSessionId :: !String
                        , nickUserData :: !SessionView -- ^ New bot agent
                        , nickFrom :: !String -- ^ Previous nick
                        , nickTo :: !String
                        }
      | PMInitReply     { pmrID :: !Snowflake
                        , pmrToNick :: !String
                        }
      | SendReply       { sentReply :: !Message -- ^ Message sent from the bot
                        }
      | WhoReply        { whoUsers :: ![SessionView] -- ^ List of sessions connected
                        }

    deriving Show

instance J.FromJSON EuphsPacket where
    parseJSON p@(J.Object v) = do
                               msgType <- v J..: "type" :: JT.Parser T.Text
                               let pktType = reverse $ takeWhile (/='-') $ reverse $ T.unpack msgType
                               let parsed =  case pktType of
                                                "reply" -> parseReply p
                                                _ -> parseEvt p
                               parseError p <|> parsed
    parseJSON x = JT.typeMismatch "EuphsPacket" x

parseReply :: J.Value -> JT.Parser EuphsPacket
parseReply (J.Object v) = do
    msgType <- v J..: "type"
    packet <- case msgType :: T.Text of
        "auth-reply" -> AuthReply <$> (v J..: "data" >>= (J..: "success"))
                        <*> (v J..: "data" >>= (\x -> x J..:? "reason" J..!= ""))
        "get-message-reply" -> GetMessageReply <$> (v J..: "data")
        "log-reply" ->  LogReply <$> (v J..: "data" >>= (J..: "log"))
                        <*> (v J..: "data" >>= (J..:? "before"))
        "nick-reply" -> NickReply
                        <$> (v J..: "data" >>= (J..: "session_id"))
                        <*> (v J..: "data" >>= (J..: "id"))
                        <*> (v J..: "data" >>= (J..: "from"))
                        <*> (v J..: "data" >>= (J..: "to"))
        "pm-initiate-reply" -> PMInitReply <$> (v J..: "data" >>= (J..: "pm_id"))
                               <*> (v J..: "data" >>= (J..: "to_nick"))
        "send-reply" -> SendReply <$> (v J..: "data")
        "who-reply" ->  WhoReply <$> (v J..: "data" >>= (J..: "listing"))
        _ -> fail "No suitable reply found"
    idPkt <- v J..: "id"
    thrReas <- v J..:? "throttled_reason"
    return (EuphsReply idPkt packet thrReas)
parseReply x = JT.typeMismatch "EuphsPacket" x

parseEvt :: J.Value -> JT.Parser EuphsPacket
parseEvt (J.Object v) = do
    msgType <- v J..: "type"
    pkt <- case msgType :: T.Text of
        "bounce-event" -> BounceEvent <$> (v J..: "data" >>= (J..: "reason"))
        "hello-event" -> HelloEvent <$> (v J..: "data" >>= (J..: "session"))
                         <*> (v J..: "data" >>= (J..: "room_is_private"))
                         <*> (v J..: "data" >>= (J..: "version"))
        "join-event" -> JoinEvent <$> v J..: "data"
        "network-event" -> NetworkEvent <$> (v J..: "data" >>= (J..: "type"))
        "nick-event" -> NickEvent <$> v J..: "data"
                        <*> (v J..: "data" >>= (J..: "from"))
        "part-event" -> PartEvent <$> v J..: "data"
        "ping-event" -> PingEvent <$> ( v J..: "data" >>= (J..: "time"))
                        <*> ( v J..: "data" >>= (J..: "next"))
        "pm-initiate-event" -> PMInitEvent <$> (v J..: "data" >>= (J..: "from"))
                               <*> (v J..: "data" >>= (J..: "from_nick"))
                               <*> (v J..: "data" >>= (J..: "from_room"))
                               <*> (v J..: "data" >>= (J..: "pm_id"))
        "send-event" -> SendEvent <$> ( v J..: "data" )
        "snapshot-event" -> SnapshotEvent <$> (v J..: "data" >>= (J..: "identity"))
                            <*> (v J..: "data" >>= (J..: "session_id"))
                            <*> (v J..: "data" >>= (J..: "version"))
                            <*> (v J..: "data" >>= (J..: "listing"))
                            <*> (v J..: "data" >>= (J..: "log"))
        _ -> fail "No suitable event found"
    return $ EuphsEvt pkt
parseEvt x = JT.typeMismatch "EuphsPacket" x

parseError :: J.Value -> JT.Parser EuphsPacket
parseError (J.Object v) = do
    msgId <- v J..:? "id"
    errorMsg <- v J..: "error"
    return $ EuphsError msgId errorMsg
parseError x = JT.typeMismatch "EuphsPacket" x

-- | Function to decode a network packet to an Euphorian event
decodePacket :: B.ByteString -> Either String EuphsPacket
decodePacket = J.eitherDecode'

-- | Function to encode an Euphorian command to a network packet
encodePacket :: SentCommand -> B.ByteString
encodePacket = J.encode

-- | Function to match a Reply with the command
matchIdReply :: String -> EuphsPacket -> Bool
matchIdReply i (EuphsError x _ ) = maybe False (==i) x
matchIdReply i (EuphsReply s _ _) = s == i
matchIdReply _ _ = False

---------------------------------------- COMMANDS ----------------------------------------

data Command = Command SentCommand | CloseCommand | ChangeRoom String

-- | Generic structure for sending a command.
data SentCommand =
    SentCommand
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
  | Log           { nMsg :: Int -- ^ Number of messages to request
                  , beforeMsg :: Snowflake -- ^ ID from when to request
                  } -- ^ Requests a log of the most recent messages
  | Send          { contentSend :: String -- ^ Message to send
                  , parentMsg :: String -- ^ MessageID of the reply, empty for root
                  } -- ^ Sends a message to the Room
  | Nick          { nick :: String -- ^ Nick to change to.
                  } -- ^ Requests a new nick.
  | GetMessage    { idGet :: Snowflake -- ^ The Snowflake of the message to get
                  } --  ^ Fully gets a message
  | Auth          { authType :: AuthOption -- ^ Method of Authentication
                  , passcode :: String -- ^ Passcode authentication
                  }
  | PMInit        { pmcID :: UserID
                  }
  deriving (Eq,Show)

instance J.ToJSON SentCommand where
  toJSON (SentCommand idCommand dataCommand) = J.object ( "id" J..= show idCommand  : inPair dataCommand)
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
