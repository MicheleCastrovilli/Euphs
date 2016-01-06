module Help where

import Euphs.Bot (Net, botName)

import Control.Monad.Reader (asks)
--import Options.Applicative
--import Safe
import Data.List

helpIntro :: String  ->  String
helpIntro bn =
    "I am @" ++ bn ++ ", a bot created by viviff for use with rooms with video players.\n\
   \This bot continues the work of NeonDJBot, the original &music bot by Drex."

helpCommands :: String
helpCommands =
   "COMMANDS:\n\
   \‣ Commands are case insensitive and ignore suffixes.\n\
   \‣ Youtube.com links or ytLink's are of the form:\n  youtube.com/watch?v=FTQbiNvZqaY\n or simply the ytid, FTQbiNvZqaY, separated by a space or a comma.\n\
   \‣ Some link shorteners are accepted, like:\n  youtu.be/FTQbiNvZqaY\n\
   \‣ Not accepted in links: playlists or start-times."

helpHelp :: String -> String
helpHelp botName' = "Help:\n• !help @" ++ botName' ++" : This very help."

helpQ :: String
helpQ = "Queue Operation:\n\
   \• !q <ytLink> <ytLink>  [-id or -ytid] (!queue):\n  Queues single or multiple ytLinks at the queue's end.\n\
   \• !qf <ytLink> <ytLink>  [-id or -ytid] (!queuefirst):\n  Same as !q but queues at the start of the queue.\n\
   \• !list [-v or -verbose][-r or -restricted][-id or -ytid][-links][-comma][-space]:\n  Shows a list of the songs currently in the queue,\n\
   \  -verbose adds ytLinks while keeping the titles.\n\
   \  -links and -id show only the links or ids without other info, separated by -comma and/or -space [default]."

helpQAdv :: String
helpQAdv = "Advanced Queue Operation:\n\
   \• !ins <pos> <ytLink> <ytLink>  [-id or -ytid] (!insert):\n  Inserts the song(s) at position <pos>,\n  moving the existing songs down.\n\
   \• !sub <pos> <ytLink>  (!substitute):\n  Substitutes the song at position <pos>\n  with the new ytLink.\n\
   \• !del <pos> <num>  (!delete or !rem, !rm, !remove):\n  Deletes <num> songs from the queue\n  starting from the <pos> position.\n\
   \• !switch <pos1> <pos2> (!swap):\n  Swaps the position of the two songs in the queue."

helpPlay :: String
helpPlay = "Playback Operation:\n\
   \• !skip:\n  Skips the currently playing song,\n  if there is a next song in the queue.\n\
   \• !dskip  (!dramaticskip):\n  Skips in any case, humorously, like the old times :D\n\
   \• !dumpq  (!dumpqueue):\n  Dumps the queue.\n\
   \• !play <ytLink>:\n  If no bots are present, this plays a single song.\n  It interrupts any current song,\n  no link shorteners allowed."

helpCountry :: String
helpCountry = "Country Restrictions:\n\
   \Shows information for the current song, or optionally for one at position <pos>.\n\
   \• !restrict [<pos> or <ytLink>](!restrictions or !restricted):\n  Shows the countries in which the song is not playable.\n\
   \• !allowed [<pos>]:\n  Shows the countries in which the song is playable."

helpExtra :: String
helpExtra = "Extras:\n\
   \• !nls  (!neonlightshow): Light Show!"

helpBot :: String
helpBot = "Bot Operation:\n\
   \• !pause: Pauses the bot, temporarily.\n\
   \• !restore: Restores the bot, from a pause.\n\
   \• !kill: Kills the bot, forever.\n\
   \• !ping: Pong!"

helpIss :: String
helpIss = "Feel free to report a problem here -> https://gitreports.com/issue/MicheleCastrovilli/Euphs\n\
   \See the current status and issues here -> https://github.com/MicheleCastrovilli/Euphs/issues"

helpFun :: Net String
helpFun = do
    botName' <- asks botName
    return $ intercalate "\n\n" [helpIntro botName', helpCommands, helpHelp botName',
                                       helpQ, helpQAdv, helpPlay, helpCountry, helpExtra, helpBot, helpIss]

helpFunShort :: Net String
helpFunShort = do
    botName' <- asks botName
    return $  "◉ :arrow_forward: To play a song: !q <youtube.com link> (now accepts youtu.be !)\n\
    \◉ Use !help @" ++ botName' ++ " for more options ('tab' will auto-complete)"

