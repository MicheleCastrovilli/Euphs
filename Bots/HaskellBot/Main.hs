{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (
    main
) where

import           Euphs.Bot
import           Euphs.Events
import           Euphs.Commands
import           Euphs.Types

import           System.Environment
import           Control.Concurrent
import           Control.Monad        (when, void, forever)
import           Data.Char            (isAlphaNum)
import           System.Process
import           System.IO
import           System.Exit          ( ExitCode(..) )
import qualified Control.Exception as C
import           Data.List
import           Control.Monad.Trans  (liftIO, MonadIO)
import           Control.Monad.Reader (asks)
--import           YTBot                (getYtFun, ytFunction, reduceCommas,noPlay,tagFunction, getTagFunction)

io :: (MonadIO m) => IO a -> m a
io = liftIO

main :: IO ()
main = bot muevalBot

myFun :: EuphEvent -> Net ()
myFun (SendEvent m) = case contentMsg m of
                        "!testing" -> void $ sendPacket $ Send "Boop" $ msgID m
                        _ -> return ()
myFun _ = return ()
--main :: IO ()
--main = do
--       args <- getArgs
--       if length args < 2 then
--        putStrLn $ "Usage: ./EuPhBot <function> <function param>\n\
--          \Current functions include : \n\
--          \E - <room argument> Starts HeliumDJBot in the room specified\n\
--          \C - <room argument> Starts  CounterBot in the room specified\
--          \F - <room argument> Starts  FortuneBot in the room specified\n\
--          \M - <room argument> Starts  HaskellBot in the room specified\n\
--          \T - <room argument> Starts  TestTagBot in the room specified\n"
--       else if head args == "E"  then
--            do
--            ytFun <- getYtFun "AIzaSyA0x4DFVPaFr8glEQvd5nylwThPrDUD4Yc" (if length args >= 3 then (args !! 2) else "False") (args !! 1)
--            _ <- if length args >= 4 then void $ forkIO (euphoriaBot "♪|HeliumDJBot" (args !! 3) $ ytFunction (ytFun {noPlay = True})) else return ()
--            euphoriaBot "♪|HeliumDJBot" (args !! 1) $ ytFunction ytFun
--        else if head args == "C" then
--            do
--            a <- newMVar True
--            b <- newMVar 0
--            euphoriaBot "CounterBot" (args !! 1) $ countFunction $ CountState a b
--        else if head args == "F" then
--          euphoriaBot "FortuneBot" (args !! 1) fortuneFunction
--        else if head args == "M" then
--          euphoriaBot "HaskellBot" (args !! 1) muevalFunction
--        else if head args == "T" then
--          getTagFunction >>= (euphoriaBot "TestTagBot" (args !! 1) . tagFunction)
--        else if head args == "Talk" then
--          newChan >>= ( euphoriaBot "ViviBot" (args !! 1) . talkBasicFun)
--        else
--          putStrLn "Use help"

--fortuneFunction :: BotFunction
--fortuneFunction botState (SendEvent message)
--  = when (contentMsg message == "!fortune") $
--      do
--      a <- readProcess "fortune" ["-s"] []
--      putStrLn a
--      sendPacket botState $ Send a $ msgID message
--      return ()
--
--fortuneFunction _ _ = return ()
--
--data CountState = CountState (MVar Bool) (MVar Int)
--
--countFunction :: CountState -> BotFunction
--countFunction cs@(CountState up num) botState (SendEvent message)
--   =  case words (contentMsg message) of
--      "!upCount" : _ ->
--        do
--        prevUp <- takeMVar up
--        putMVar up True
--        sendPacket botState (Send (if prevUp then "It was already up!" else "Set to up") $ msgID message)
--      "!downCount" : _ ->
--        do
--        prevUp <- takeMVar up
--        putMVar up False
--        sendPacket botState (Send (if prevUp then "Set to down" else "It was already down!") $ msgID message)
--      "!count" : _ ->
--        do
--        prevNum <- takeMVar num
--        prevUp  <- takeMVar up
--        threadDelay 500000
--        putMVar up prevUp
--        let nextNum = if prevUp then prevNum + 1 else prevNum - 1
--        putMVar num nextNum
--        sendPacket botState $ Send (show nextNum) $ msgID message
--      "!gotoRoom" : x ->
--        closeConnection botState False >>
--         (euphoriaBot "CounterBot"  (head x) $ countFunction cs)
--      "!replicateTo" : x ->
--        euphoriaBot "CounterBot"  (head x) $ countFunction cs
--
--      _ -> return ()
--
--countFunction _ _ _ =
--      return ()
--

muevalBot = emptyBot {
    eventsHook = muevalFunction
  , helpShortHook = Just shortHelp
  , helpLongHook = Just longHelp
}

shortHelp :: Net String
shortHelp = do
            bn <- asks botName
            return $ unlines ["This bot helps with the Haskell programming language"
                             , "Use !help @" ++ bn ++ " for more info."]
longHelp :: Net String
longHelp = return "Use !haskell <expr> for evaluating an Haskell expression.\n\
           \Use !hoogle to return the first three results on a particular search.\n\
           \Use !hoogleinfo to return the first result, in a more informative way\n\
           \Searches can be done through function name, or definition"

muevalFunction :: EuphEvent -> Net ()
muevalFunction (SendEvent message) =
    case words (contentMsg message) of
      "!haskell" : _ -> case stripPrefix "!haskell" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> io (readProcess' "mueval" ["-m","Numeric","-l", "/home/viviff9/floobits/viviff9/MuevalDef/MuevalDef.hs",  "-t","15","-e", x ] []) >>=
                              (\y -> void $ sendPacket $ Send (concatMap format y) $ msgID message)
      "!hoogleinfo"  : _ -> case stripPrefix "!hoogleinfo" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> io (readProcess' "hoogle"
                            ["search" ,"-d" ,
                            "/home/viviff9/.cabal/share/x86_64-linux-ghc-7.10.2/hoogle-4.2.42/databases",
                            "-n", "3", "-i", x] []) >>=
                                (\y -> void $ sendPacket $ Send y $ msgID message)
      "!hoogle"  : _ -> case stripPrefix "!hoogle" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> io (readProcess' "hoogle"
                            ["search" ,"-d" ,
                            "/home/viviff9/.cabal/share/x86_64-linux-ghc-7.10.2/hoogle-4.2.42/databases",
                            "-n", "3", x] []) >>=
                                (\y -> void $ sendPacket $ Send y $ msgID message)
      _ -> return  ()

muevalFunction _ = return ()


readProcess'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout + stderr
readProcess' cmd args input = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure _ -> return output

format :: Char -> String
format ',' = " , "
format c = [c]
--
--
--talkBasicFun :: Chan MessageID -> BotFunction
--talkBasicFun chan botState (SnapshotEvent _ _ _ _ _)
--  = forever (getLine >>= \x -> sendPacket botState (Send x ""))
--
--talkBasicFun chan botState (SendEvent message)
--    = case words $ contentMsg message of
--      (stripPrefix "!countbots" ->  Just r):x -> sendPacket botState Who >> writeChan chan (msgID message)
--      (stripPrefix "!help" -> Just _):r:_ -> when (filter isAlphaNum r == filter isAlphaNum (botName botState))
--                                             $ sendPacket botState $ Send "Help: !countbots for counting the current bots in the channel." $ msgID message
--      _ -> return ()
--talkBasicFun chan botState (WhoReply x y)
-- =  readChan chan >>= sendPacket botState . Send (show $ length $ filter (isPrefixOf "bot:" . userID) y)
--
--
--
--talkBasicFun _ _ _ = return ()