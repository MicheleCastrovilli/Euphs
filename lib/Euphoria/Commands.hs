{-# LANGUAGE OverloadedStrings #-}

module Euphoria.Commands where 

import qualified Data.Aeson                 as J
import qualified Data.Text                  as T

data SentCommand = 
  Command {
    commandID   :: Int,
    commandData :: EuphCommand 
  }
  deriving (Show)

data EuphCommand = 
    PingReply     { timeReply    :: Integer }
  | Who 
  | Log           { nMsg         :: Int,
                    beforeMsg    :: String  }
  | Send          { contentMsg   :: String,
                    parentMsg    :: String  }
  | Nick          { nick         :: String  }
  deriving (Eq,Show)

instance J.ToJSON SentCommand where
  toJSON (Command idCommand dataCommand) = J.object ( "id" J..= show idCommand  : inPair dataCommand)


inPair :: EuphCommand -> [(T.Text, J.Value)]
inPair (PingReply time)      = [
                                 ("type" , "ping-reply" ),
                                 ("data", J.object  [ "time" J..= time ])
                               ]
inPair (Nick nickname)           = [ 
                                 ("type" , "nick" ),
                                 ("data" , J.object [ "name"  J..= nickname])
                               ]
inPair (Send content parent) = [ 
                                 ("type" , "send" ),
                                 ("data" , J.object [ 
                                                      "content" J..= content,
                                                      "parent"  J..= parent
                                                    ]
                                 )
                               ]
inPair _ = []

