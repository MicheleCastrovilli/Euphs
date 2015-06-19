{-# LANGUAGE OverloadedStrings #-}
module Euphoria.Events where

import qualified Data.Aeson              as J
import qualified Data.Text               as T
import           Control.Monad
import           Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Maybe              as M
import           Euphoria.Types

data EuphEvent = 
        PingEvent { pingTime :: Integer,
                    nextTime :: Integer}
      | WhoReply
      | LogReply
      | SnapshotEvent
      | SendEvent { msgData :: MessageData }
      | SendReply { msgData :: MessageData }
      | NickEvent { 
                   idResp :: String,
                   userD :: UserData
                  }
      | JoinEvent
      | PartEvent
      deriving (Show)


instance J.FromJSON EuphEvent where 
  parseJSON (J.Object v) = do 
                           msgType <- (v J..: "type")
                           case msgType :: T.Text of
                            "ping-event" ->
                                 PingEvent <$> ( v J..: "data" >>= (J..: "time"))
                                           <*> ( v J..: "data" >>= (J..: "next"))
                            "snapshot-event" -> return SnapshotEvent
                            "send-reply" -> 
                                 SendReply <$> ( v J..: "data" )
                            "send-event" ->
                                 SendEvent <$> ( v J..: "data" )
                            "nick-reply" ->
                                 NickEvent <$>  v J..: "id"
                                           <*> (UserData <$> ( v J..: "data" >>= (J..: "id"))
                                                         <*> ( v J..: "data" >>= (J..: "to"))
                                                         <*> return ""
                                                         <*> return ""
                                                         <*> ( v J..: "data" >>= (J..: "session_id"))
                                               )

                            "nick-event" -> 
                                 NickEvent <$> v J..: "id" 
                                           <*> (UserData <$> ( v J..: "data" >>= (J..: "id"))
                                                         <*> ( v J..: "data" >>= (J..: "to"))
                                                         <*> return ""
                                                         <*> return ""
                                                         <*> ( v J..: "data" >>= (J..: "session_id"))

                                               )

                            _ -> mzero
  parseJSON _ = mzero



makePingReply :: Int -> IO BL.ByteString 
makePingReply count = do 
                      time <- getPOSIXTime
                      return $ J.encode (PingReply count $ round time)

nickRequest name count= J.encode (Nick count name)
