{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.C (C(..)) where

import Control.Lens hiding (Choice)
import Data.Code.Generic

newtype C a b = C {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (C a b) k1 a k2 b where
    code = coerced