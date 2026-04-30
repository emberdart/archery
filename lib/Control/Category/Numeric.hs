{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Control.Category.Numeric (Numeric(..)) where

import Control.Arrow (Kleisli (..))
import Data.Aeson

class Numeric cat where
    -- sorry for the ToJSON here but what wouldn't be really
    num :: (Integral n, Show n, ToJSON n) ⇒ n → cat a n
    negate' :: Num n ⇒ cat n n
    add :: Num n ⇒ cat (n, n) n
    mult :: Num n ⇒ cat (n, n) n
    div' :: Integral n ⇒ cat (n, n) n
    mod' :: Integral n ⇒ cat (n, n) n

instance Numeric (->) where
    num = const
    negate' = negate
    add = uncurry (+)
    mult = uncurry (*)
    div' = uncurry div
    mod' = uncurry mod

instance Applicative m ⇒ Numeric (Kleisli m) where
    negate' = Kleisli $ pure . negate
    add = Kleisli $ pure . uncurry (+)
    mult = Kleisli $ pure . uncurry (*)
    div' = Kleisli $ pure . uncurry div
    mod' = Kleisli $ pure . uncurry mod
    num = Kleisli . const . pure
