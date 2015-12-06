module Euphs.Easy where

import Data.List (intercalate)
import Control.Monad (void)
import Safe

import Euphs.Bot
import Euphs.Events
import Euphs.Commands
import Euphs.Types

type Command = String
type ShortHelp = String
type LongHelp = String
type EasyFun = String -> Net String

type EasyBotFun = (Command,(EasyFun, ShortHelp, LongHelp))

easyBot :: [EasyBotFun] -> IO ()
easyBot f = bot emptyBot {
                 eventsHook = easyFun f
            ,    helpLongHook = Just $ easyHelp f
            }

easyFun :: [EasyBotFun] -> EuphEvent -> Net ()
easyFun f (SendEvent m) =
    let fun = do
            word <- headMay $ words $ contentMsg m
            (fun', _, _) <- lookup word f
            return fun'
    in case fun of
         Nothing -> return ()
         Just f' -> (f' $ concat $ drop 1 $ words $ contentMsg m) >>=
                    (\res -> void $ sendPacket $ Send res $ msgID m)

easyFun _ _ = return ()

easyHelp :: [EasyBotFun] -> [String] -> Net String
easyHelp f h = return $ if null h then
                    "This bot replies to the following commands:\n\n"
                    ++ intercalate "\n" (map showHelp f)
               else
                    maybe "Function not found."
                        (\(_,_,l) -> if null l then "No descriptive help" else l) $ lookup (head h) f

showHelp :: EasyBotFun -> String
showHelp (c, ( _, s, _))  = "\"" ++ c ++ "\"" ++ if null s then "" else (" - " ++ s)

