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

defFile :: String
defFile = "/home/viviff9/floobits/viviff9/MuevalDef/MuevalDef.hs"

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
myFun a@(SnapshotEvent _ _ _ u _) = do
                                  tellLog $ T.pack $ show u
                                  w <- sendPacket Who
                                  let equ = \x y -> name x == name y
                                  let grsort = L.groupBy equ . L.sortBy (\x y -> compare (name x) (name y))
                                  let fil = filter (isPrefixOf "bot:" . userID)
                                  let stuff = unlines $ map show $ concat $ filter (\x -> length x > 1) $ grsort  $ fil $ users w
                                  tellLog $ T.pack $ unlines $ map show $ fil $ users w
myFun x = tellLog $ T.pack $ show x

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
                                ["-m","Numeric","-l", defFile , "-t","10","-e", x, "+RTS", "-N2"]
                              let y' = parse y
                              void $ sendPacket $ Send (concatMap format y') $ msgID message
      "!hastype" : _ -> case stripPrefix "!hastype" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> do
                              y <- io $ readProcess' "mueval"
                                ["-m","Numeric","-l", defFile , "-T","-i","-e", x ]
                              let y' = parse y
                              void $ sendPacket $ Send (concatMap format y') $ msgID message
      "!hoogleinfo"  : _ -> case stripPrefix "!hoogleinfo" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> io (readProcess' "hoogle"
                            ["search" ,
                            --"-d" , "/home/viviff9/.cabal/share/x86_64-linux-ghc-7.10.2/hoogle-4.2.42/databases",
                            "-n", "3", "-i", x]) >>=
                                (\y -> void $ sendPacket $ Send y $ msgID message)
      "!hoogle"  : _ -> case stripPrefix "!hoogle" $ contentMsg message of
                          Nothing -> return ()
                          Just x -> io (readProcess' "hoogle"
                            ["search" ,
                            --"-d" , "/home/viviff9/.cabal/share/x86_64-linux-ghc-7.10.2/hoogle-4.2.42/databases",
                            "-n", "3", x]) >>=
                                (\y -> void $ sendPacket $ Send y $ msgID message)
      "!nick" : _ -> case stripPrefix "!nick" $ contentMsg message of
                      Nothing -> void $ sendPacket $ Nick ""
                      Just x -> void $ sendPacket $ Nick $ dropWhile (==' ') x
      _ -> return  ()

muevalFunction _ = return ()

readProcess' :: FilePath -> [String] -> IO String
readProcess' mu arg = do (code,so,se) <-readProcessWithExitCode mu arg ""
                         case code of
                           ExitSuccess -> return so
                           ExitFailure _ -> return $ if null so then se else so


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
