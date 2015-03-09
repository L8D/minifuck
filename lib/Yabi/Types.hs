{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Yabi.Types
    ( VM(..)
    , Inst(..)
    , World(..)
    , left
    , here
    , right
    ) where

import Control.Monad.State
import Control.Applicative
import Control.Lens
import Data.Word

import qualified Data.Stream as S

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
    { _left  :: S.Stream Word8
    , _here  :: Word8
    , _right :: S.Stream Word8
    }

makeLenses ''World
