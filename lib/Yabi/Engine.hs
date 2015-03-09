{-# LANGUAGE RankNTypes #-}

module Yabi.Engine
    ( run
    , initWorld
    ) where

import Control.Monad.Trans
import Data.ByteString
import Control.Lens
import System.IO       (stdin, stdout)
import Data.Word
import Prelude         hiding (null, head)

import qualified Data.Stream as S

import Yabi.Types

initWorld :: World
initWorld = World (S.repeat 0) 0 (S.repeat 0)

run :: [Inst] -> VM ()
run [] = return ()
run (x:xs) = inst x >> run xs

shift :: Lens' World (S.Stream Word8) -> VM Word8
shift g = g %%= \(S.Cons x xs) -> (x, xs)

unshift :: Word8 -> Lens' World (S.Stream Word8) -> VM ()
unshift x g = g %= S.Cons x

inst :: Inst -> VM ()
inst Next = do
    c <- use here
    r <- shift right
    unshift c left
    here .= r
inst Prev = do
    c <- use here
    l <- shift left
    unshift c right
    here .= l
inst Incr = here += 1
inst Decr = here -= 1
inst GetC = do
    bs <- liftIO $ hGet stdin 1
    here .= if null bs then -1 else head bs
inst PutC = do
    c <- use here
    liftIO $ hPut stdout (pack [c])
inst (Loop blk) = do
    c <- use here
    if c == 0 then return () else run blk >> inst (Loop blk)
