module Utils where

import Data.Maybe (listToMaybe)
import Data.Char (isSpace)

import qualified Data.Sequence as SQ

safeHead :: [a] -> Maybe a
safeHead x = if null x then
              Nothing
             else
              Just $ head x

safeHeadSeq :: SQ.Seq a -> Maybe a
safeHeadSeq x = if SQ.null x then
                  Nothing
                else
                  Just $ SQ.index x 0

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads
