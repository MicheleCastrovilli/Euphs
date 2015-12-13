module Utils where

import Data.Maybe (listToMaybe)
import Data.Char (isSpace)

import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.IO.Class (liftIO, MonadIO)

import qualified Data.Sequence as SQ

safeHeadSeq :: SQ.Seq a -> Maybe a
safeHeadSeq x = if SQ.null x then
                  Nothing
                else
                  Just $ SQ.index x 0

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

liftNet :: (MonadTrans t, Monad m) => m a -> t m a
liftNet = lift

io :: (MonadIO m) => IO a -> m a
io = liftIO
