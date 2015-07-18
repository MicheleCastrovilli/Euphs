EuPhBot
=========

[![Build Status](https://travis-ci.org/MicheleCastrovilli/Euphs.svg?branch=master)] (https://travis-ci.org/MicheleCastrovilli/EuPhBot)

A simple bot framework for the euphoria.io platform.
Implemented in Haskell.

Not working yet. 

Current TODO:
  * [x] Refactoring
  * [x] Implement some bot functions
  * [ ] Find a way to implement a script of some kind
  * [ ] Implement whole euphoria.io protocol



DJBot TODO:
  * [x] Make commands react to both uppercase and lower case
  * [x] Make it recognize also longer versions of the commands (e.g. !vqueuefirst)
  * [x] Reject the video entirely if not embeddable
  * [x] Add !vhelp also, instead of only !help @<botName>
  * [x] Make commands react also to only the command prefix
  * [x] !vinsert or !vi , for inserting songs in the queue in any position
  * [ ] Take queue requests also when offline, so in the reconnecting phase
  * [ ] Announce if countries are banned from playing that song
  * [ ] Add some inter-room function
    * [ ] Split the queue control, and the queue play bot functions
    * [ ] Also re-unite the bots
  * [ ] !vdelete or !vd , for deleting songs from the queue 
  * [ ] Expanding into multiple modes, like party or event mode
    * [ ] Also add protection from misuse, maybe a !vundo 
    * [ ] Blind titles for show/event mode
    * [ ] Possibility for locking/unlocking the queue to the event host
    * [ ] Various verbosity levels
  * [ ] Allow ytid only !vq or !vqf
  * [ ] Add optional flag to !vlist to show only video titles, without the playing
    * [ ] Add also flag to list titles with full video links (with no playing)
  * [ ] Integrate it better with SongMaster
  * [ ] Make it accept the NeonDJBot commands (without the v), if the other DJ is not present
  * [ ] Reword help to make it clearer that multiqueue is possible, also saying second generation of bots

