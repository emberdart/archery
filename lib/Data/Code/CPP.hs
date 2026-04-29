{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.CPP (CPP(..)) where

import Control.Lens hiding (Choice)
import Data.Code.Generic

newtype CPP a b = CPP {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (CPP a b) k1 a k2 b where
    code = coerced