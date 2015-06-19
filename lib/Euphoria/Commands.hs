module Euphoria.Commands where 

import Euphoria.Types

data EuphCommands = 
    PingReply { id :: Int,
                time :: Integer}
  | Who { id :: Int } 
  | Log { id :: Int,
          nMsg :: Int,
          before ::  String}
  | Send { id :: Int,
           content :: String,
           parent :: String}
  | Nick { id :: Int, 
           nick :: String}
  deriving (Eq,Show)


instance J.ToJSON EuphCommands where
  toJSON (PingReply id time) = J.object [ "id" J..= show id,
                                                "type" J..= ("ping-reply" :: T.Text),
                                                "data" J..= J.object [ "time"  J..= time]
                                              ]
  toJSON (Nick id nick) =  J.object [ "id" J..= show id,
                                            "type" J..= ("nick" :: T.Text),
                                            "data" J..= J.object [ "name"  J..= nick]
                                          ]
  toJSON (Send id content parent) = J.object [ "id"   J..= show id,
                                               "type" J..= ("send" :: T.Text) ,
                                               "data" J..= J.object [ "content" J..= content,
                                                                      "parent"  J..= parent
                                                                    ]
                                             ]
  toJSON _ = J.object []

