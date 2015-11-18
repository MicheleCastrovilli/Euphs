{-# LANGUAGE OverloadedStrings #-}
-- | This module defines which commands can be executed from the bot.
module Euphs.Commands (
    SentCommand(..)
 ,  EuphCommand(..)
)where

import qualified Data.Aeson                 as J
import qualified Data.Text                  as T

import Euphs.Types

-- | Generic structure for sending a command.
data SentCommand =
  Command
    { commandID   :: Int -- ^ ID of the packet to send the server
    , commandData :: EuphCommand -- ^ Proper packet
    } deriving (Show)

-- | Types of commands
data EuphCommand =
    PingReply     { timeReply    :: Integer } -- ^ Reply to ping event
  | Who -- | Requests a list of sessions connected to the room
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
  deriving (Eq,Show)

instance J.ToJSON SentCommand where
  toJSON (Command idCommand dataCommand) = J.object ( "id" J..= show idCommand  : inPair dataCommand)

-- | An internal function, to work with the JSON data
inPair :: EuphCommand -> [(T.Text, J.Value)]
inPair (PingReply time) =
  [
    ("type" , "ping-reply" ),
    ("data", J.object  [ "time" J..= time ])
  ]
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

