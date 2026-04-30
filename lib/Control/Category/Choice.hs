{-# LANGUAGE Safe #-}

module Control.Category.Choice (Choice(..)) where

import Control.Arrow (Kleisli (..))

class Choice cat where
    left' :: cat a b → cat (Either a x) (Either b x)
    right' :: cat a b → cat (Either x a) (Either x b)

instance Choice (->) where
    left' f (Left a)  = Left (f a)
    left' _ (Right a) = Right a
    right' :: (a → b) → Either x a → Either x b
    right' _ (Left a)  = Left a
    right' f (Right a) = Right (f a)

instance Monad m ⇒ Choice (Kleisli m) where
    left' (Kleisli f) = Kleisli $ \case
        Left a  -> f a >>= \b -> pure (Left b)
        Right a -> pure (Right a)
    right' (Kleisli f) = Kleisli $ \case
        Left a  -> pure (Left a)
        Right a -> f a >>= \b -> pure (Right b)

-- instance Monad m => Choice (Kleisli m) where
