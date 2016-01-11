-- | This module defines the type 'Opts' and gives a function to parse commandline arguments.
{-# LANGUAGE BangPatterns #-}
module Euphs.Options
    ( Opts(..)
    , parseOpts
    , OptsList
    , defaults
    , options
    , getOpts
    ) where

import System.Exit
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad (when)

-- | The main options structure.
data Opts = Opts { heimHost   :: !String    -- ^ Heim instance hoster
                 , heimPort   :: !Int       -- ^ The port to connect to
                 , useSSL     :: !Bool      -- ^ Whether to use wss or ws
                 , roomList   :: !String    -- ^ A list of initial rooms separated by whitespace in the format of <room name>[-<pw>]
                 , showHelp   :: !Bool      -- ^ Print help on startup
                 , logTarget  :: !FilePath  -- ^ The log file to write to.
                                           -- Fall back to 'stdout' when
                                           -- empty
                 , botAccount :: !FilePath  -- ^ Email and password for logging into the account
                 , botNick    :: !String    -- ^ The nick the Bot will be using.
                 , config     :: !FilePath  -- ^ Config file for specific bot options
                 }
                 deriving (Show)

-- | Type synonym for the list of options to the bot
type OptsList = [OptDescr (Opts -> Opts)]

-- | Default options
defaults :: Opts
defaults = Opts { heimHost   = "euphoria.io"
                , heimPort   = 443
                , useSSL     = True
                , roomList   = "test"
                , showHelp   = False
                , logTarget  = ""
                , botAccount = ""
                , botNick    = "EmptyBot"
                , config     = ""
                }

-- | List of options available
options :: OptsList
options =
    [ Option "e" ["host"]
        (ReqArg (\arg opt -> opt {heimHost = arg}) "HOST") "Heim instance to connect to"
    , Option "p" ["port"]
        (ReqArg (\arg opt -> opt {heimPort = read arg :: Int}) "PORT") "Port to use"
    , Option "r" ["rooms", "room"]
        (ReqArg (\arg opt -> opt {roomList = arg}) "ROOMS") "Rooms to join"
    , Option "s" ["nossl"]
        (NoArg (\opt -> opt {useSSL = False})) "Disable SSL"
    , Option "l" ["log"]
        (ReqArg (\arg opt -> opt {logTarget = arg}) "LOG") "Logging FilePath"
    , Option "i" ["identity"]
        (ReqArg (\arg opt -> opt {botAccount = arg}) "ID") "Bot Identity FilePath"
    , Option "c" ["config"]
        (ReqArg (\arg opt -> opt {config = arg}) "CONFIG") "Path for the bot config file"
    , Option "n" ["nick"]
        (ReqArg (\arg opt -> opt {botNick = arg}) "NICK") "Nickname to use"
    , Option "h" ["help"]
        (NoArg (\opt -> opt {showHelp = True})) "Shows this help."
    ]

header :: String
header = "Usage: Euphs [options]"

-- | Function to parse commandline Options
parseOpts :: Opts -> OptsList -> [String] -> IO (Opts, [String])
parseOpts defOpts opts argv =
    case getOpt Permute opts argv of
        (o,n,[]  ) -> return (foldl (flip id) defOpts o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ showUsage))

-- | Shows the usage message
showUsage :: String
showUsage = usageInfo header options

-- | Prints the usage message to stdout and exits.
showUsageAndExit :: IO ()
showUsageAndExit = do
        putStrLn showUsage
        exitSuccess

-- | Parses options from the commandline, using the Opts given in as a default to be overridden
getOpts :: Opts -> OptsList -> IO Opts
getOpts opt optL = do
          (opts, _) <- getArgs >>= parseOpts opt optL
          when (showHelp opts) showUsageAndExit
          return opts
