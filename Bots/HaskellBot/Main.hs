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
import           Data.Char            (isAlphaNum, isSpace)
import           Data.Maybe           (listToMaybe, fromMaybe)
import qualified Data.List            as L
import qualified Data.Text            as T
import           System.Process
import           System.IO
import           System.Exit          ( ExitCode(..) )
import qualified Control.Exception as C
import           Data.List
import           Control.Monad.Trans  (liftIO, MonadIO)
import           Control.Monad.Reader (asks)

import Text.Read.Lex
import Text.ParserCombinators.ReadP

io :: (MonadIO m) => IO a -> m a
io = liftIO

main :: IO ()
main = bot muevalBot

myFun :: EuphEvent -> Net ()
myFun (SendEvent m) = case contentMsg m of
                        "!who" -> do
                                  w <- sendPacket Who
                                  let equ = \x y -> name x == name y
                                  let grsort = L.groupBy equ . L.sortBy (\x y -> compare (name x) (name y))
                                  let fil = filter (isPrefixOf "bot:" . userID)
                                  let stuff = unlines $ map show $ concat $ filter (\x -> length x > 1) $ grsort  $ fil $ users w
                                  void $ sendPacket $ Send stuff $ msgID m
                                  tellLog $ T.pack $ show w
                        _ -> tellLog $ T.pack $ show m
myFun a@(SnapshotEvent _ _ _ _ _) = do
                                  tellLog $ T.pack $ show a
                                  w <- sendPacket Who
                                  let equ = \x y -> name x == name y
                                  let grsort = L.groupBy equ . L.sortBy (\x y -> compare (name x) (name y))
                                  let fil = filter (isPrefixOf "bot:" . userID)
                                  let stuff = unlines $ map show $ concat $ filter (\x -> length x > 1) $ grsort  $ fil $ users w
                                  tellLog $ T.pack $ stuff
myFun x = tellLog $ T.pack $ show x

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
  , helpShortHook = Just $ const shortHelp
  , helpLongHook = Just $ const longHelp
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
                          Just x -> do
                              y <- io $ readProcess' "mueval"
                                ["-m","Numeric","-l", "/home/viviff9/floobits/viviff9/MuevalDef/MuevalDef.hs",  "-t","15","-e", x ] []
                              let y' = parse y
                              void $ sendPacket $ Send (concatMap format y') $ msgID message
      "!hastype" : _ -> case stripPrefix "!hastype" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> do
                              y <- io $ readProcess' "mueval"
                                ["-m","Numeric","-l", "/home/viviff9/floobits/viviff9/MuevalDef/MuevalDef.hs","-T","-i","-e", x ] []
                              let y' = parse y
                              void $ sendPacket $ Send (concatMap format y') $ msgID message
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

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

parse :: String -> String
parse str = let y = escape str in fromMaybe y $ maybeRead y

escape xs
    | []      <- r = []
    | [(a,_)] <- r = a
    where r = readP_to_S (manyTill lexChar eof) xs
