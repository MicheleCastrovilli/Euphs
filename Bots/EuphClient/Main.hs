module Main where

import Euphs.Bot
import Euphs.Events
import Euphs.Commands
import Euphs.Options
import Euphs.Types

main :: IO ()
main = clientBot >>= bot

clientBot = do
    config =
    BotFunctions {
       eventsHook = clientFun
    }

clientFun m (SendEvent m) = do

