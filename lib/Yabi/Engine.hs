module Yabi.Engine
    ( run
    , initWorld
    ) where

import Control.Monad.Trans
import Data.ByteString
import Control.Lens
import System.IO       (stdin, stdout)
import Prelude         hiding (null, head)

import qualified Data.IntMap as IM

import Yabi.Types

initWorld :: World
initWorld = World IM.empty 0

run :: [Inst] -> VM ()
run [] = return ()
run (x:xs) = inst x >> run xs

inst :: Inst -> VM ()
inst Next = pos += 1
inst Prev = pos -= 1
inst Incr = use pos >>= \p -> array . at p %= Just . maybe 1 (+1)
inst Decr = use pos >>= \p -> array . at p %= Just . maybe (-1) (+ (-1))
inst GetC = do
    bs <- liftIO $ hGet stdin 1
    p <- use pos
    array . at p ?= if null bs then -1 else head bs
inst PutC = do
    p <- use pos
    c <- uses (array . at p) (maybe 0 id)
    liftIO $ hPut stdout (pack [c])
inst (Loop blk) = do
    p <- use pos
    c <- uses (array . at p) (maybe 0 id)
    if c == 0 then return () else run blk >> inst (Loop blk)
