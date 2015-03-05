{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Yabi.Types
    ( VM(..)
    , Inst(..)
    , World(..)
    , array
    , pos
    ) where

import Control.Monad.State
import Control.Applicative
import Control.Lens
import Data.Word

import qualified Data.IntMap as IM

data Inst
    = Next
    | Prev
    | Incr
    | Decr
    | PutC
    | GetC
    | Loop [Inst]

newtype VM a = VM { runVM :: StateT World IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState World
             , MonadIO
             )

data World = World
    { _array :: IM.IntMap Word8
    , _pos   :: Int
    }

makeLenses ''World
