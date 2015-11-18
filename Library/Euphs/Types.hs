{-# LANGUAGE OverloadedStrings #-}
-- | This module defines the Types and structure of the packets incoming.
-- | Also look at the official <https://github.com/euphoria-io/heim/blob/master/doc/api.md api>.
module Euphs.Types where

import qualified Data.Aeson                 as J
import           Control.Monad
import           Control.Applicative ((<|>))

-- | A type synonym for Snowflake.
type MessageID = String

-- | Structure representing an agent, user or bot. Also called SessionView, in the api.
data UserData = UserData { userID     :: String -- ^ ID of the user
                         , name       :: String -- ^ Current nick of the user
                         , serverID   :: String -- ^ The id of the server
                         , serverEra  :: String -- ^ The era of the server
                         , sessionID  :: String -- ^ ID of the session, unique across all sessions, globally
                         , isStaff    :: Bool   -- ^ Whether the session, is a member of the staff
                         , isManager  :: Bool   -- ^ Whether the session, is a manager of the room
                         }
      deriving (Eq,Show, Read)

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
                               }
      deriving (Show)

{-instance Eq MessageData where-}
  {-m1@(MessageData {})i == m2@(MessageData {}) = timeRecieved m1 == timeRecieved m2-}

{-instance Ord MessageData where-}
  {-m1@(MessageData {})i `compare` m2@(MessageData {}) = timeRecieved m1 `compare` timeRecieved m2-}

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

  parseJSON _ = mzero

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
  parseJSON _ = mzero
