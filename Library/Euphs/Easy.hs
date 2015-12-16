-- | An Easier way of making a bot, that has basic needs, like an exact word match for the command,
-- | and providing an easy way to also have an auto generated help.
module Euphs.Easy (easyBot, EasyBotFun, Command, ShortHelp, LongHelp, EasyFun) where

import Data.List (intercalate)
import Control.Monad (void)
import Safe

import Euphs.Bot
import Euphs.Events
import Euphs.Commands
import Euphs.Types

-- | A '!command' string, to define the trigger of the action.
type Command = String
-- | Short help about the command implemented
type ShortHelp = String
-- | Long help about the command implemented
type LongHelp = String
-- | A function with the rest of the command as input, and the output as a direct reply.
type EasyFun = String -> Net String
-- | A structure for defining a command.
type EasyBotFun = (Command,(EasyFun, ShortHelp, LongHelp))

-- | The main function call.
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

