module Utils where

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
