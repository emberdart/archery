{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE Unsafe                #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Function.Free.Abstract (FreeFunc(..)) where

import Control.Category
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
-- import Control.Category.Cochoice
-- import Control.Category.Costrong
import Control.Category.Interpret
import Control.Category.Numeric
import Control.Category.Strong
import Control.Category.Symmetric
import Data.Aeson
import Data.Aeson.Types
import Prelude                      hiding (id, (.))
import Control.Arrow

data FreeFunc p a b where
    Id :: FreeFunc p x x
    Compose :: FreeFunc p y z -> FreeFunc p x y -> FreeFunc p x z
    Copy :: FreeFunc p x (x, x)
    Consume :: FreeFunc p x ()
    First :: FreeFunc p a b -> FreeFunc p (a, x) (b, x)
    Second :: FreeFunc p a b -> FreeFunc p (x, a) (x, b)
    {- Unfirst :: FreeFunc p (a, x) (b, x) -> FreeFunc p a b -}
    Fst :: FreeFunc p (a, b) a
    Snd :: FreeFunc p (a, b) b
    InjectL :: FreeFunc p a (Either a b)
    InjectR :: FreeFunc p a (Either b a)
    Left' :: FreeFunc p a b -> FreeFunc p (Either a x) (Either b x)
    Right' :: FreeFunc p a b -> FreeFunc p (Either x a) (Either x b)
    {- Unleft :: FreeFunc p (Either a x) (Either b x) -> FreeFunc p a b -}
    Unify :: FreeFunc p (Either a a) a
    Tag :: FreeFunc p (Bool, a) (Either a a)
    Num :: (Num n, Show n, ToJSON n) => n -> FreeFunc p a n
    Negate :: Num n => FreeFunc p n n
    Add :: Num n => FreeFunc p (n, n) n
    Mult :: Num n => FreeFunc p (n, n) n
    Div :: Integral n => FreeFunc p (n, n) n
    Mod :: Integral n => FreeFunc p (n, n) n
    Swap :: FreeFunc p (a, b) (b, a)
    SwapEither :: FreeFunc p (Either a b) (Either b a)
    Reassoc :: FreeFunc p (a, (b, c)) ((a, b), c)
    ReassocEither :: FreeFunc p (Either a (Either b c)) (Either (Either a b) c)
    Lift :: p a b -> FreeFunc p a b

deriving instance (forall a b. Show (p a b)) ⇒ Show (FreeFunc p x y)

-- deriving instance (forall a b. Read (p a b)) => Read (FreeFunc p x y)



instance (Numeric cat, Cocartesian cat, {- Cochoice cat,-} Choice cat, Cartesian cat, {- Costrong cat, -} Strong cat, Category cat, Symmetric cat, Interpret p cat) ⇒ Interpret (FreeFunc p) cat where
    {-# INLINABLE interpret #-}
    interpret Id            = id
    interpret (Compose a b) = interpret a . interpret b
    interpret Copy          = copy
    interpret Consume       = consume
    interpret (First a)     = first' (interpret a)
    interpret (Second a)    = second' (interpret a)
    {- interpret (Unfirst a)   = unfirst (interpret a) -}
    interpret Fst           = fst'
    interpret Snd           = snd'
    interpret InjectL       = injectL
    interpret InjectR       = injectR
    interpret (Left' a)     = left' (interpret a)
    interpret (Right' a)    = right' (interpret a)
    {- interpret (Unleft a)    = unleft (interpret a) -}
    interpret Unify         = unify
    interpret Tag           = tag
    interpret (Num n)       = num n
    interpret Negate        = negate'
    interpret Add           = add
    interpret Mult          = mult
    interpret Div           = div'
    interpret Mod           = mod'
    interpret Swap          = swap
    interpret SwapEither    = swapEither
    interpret Reassoc       = reassoc
    interpret ReassocEither = reassocEither
    interpret (Lift a)      = interpret a

instance (forall a b. ToJSON (k a b)) ⇒ ToJSON (FreeFunc k x y) where
    toJSON Id = String "Id"
    toJSON (Compose f g) = Array [ String "Compose", Array [ toJSON f, toJSON g ] ]
    toJSON Copy = String "Copy"
    toJSON Consume = String "Consume"
    toJSON (First f) = Array [ String "First", Array [ toJSON f ] ]
    toJSON (Second f) = Array [ String "Second", Array [ toJSON f ] ]
    {- toJSON (Unfirst f) = Array [ String "Unfirst", Array [ toJSON f ] ] -}
    toJSON Fst = String "Fst"
    toJSON Snd = String "Snd"
    toJSON InjectL = String "InjectL"
    toJSON InjectR = String "InjectR"
    toJSON (Left' f) = Array [ String "Left'", Array [ toJSON f ] ]
    toJSON (Right' f) = Array [ String "Right'", Array [ toJSON f ] ]
    {- toJSON (Unleft f) = Array [ String "Unleft'", Array [ toJSON f ] ] -}
    toJSON Unify = String "Unify"
    toJSON Tag = String "Tag"
    toJSON (Num n) = Array [ String "Num", Array [ toJSON n ] ]
    toJSON Negate = String "Negate"
    toJSON Add = String "Add"
    toJSON Mult = String "Add"
    toJSON Div = String "Div"
    toJSON Mod = String "Mod"
    toJSON Swap = String "Swap"
    toJSON SwapEither = String "SwapEither"
    toJSON Reassoc = String "Reassoc"
    toJSON ReassocEither = String "ReassocEither"
    toJSON (Lift f) = Array [ "Lift", Array [ toJSON f ] ]

-- be specific here for now
instance FromJSON (FreeFunc k (Int, Int) Int) where
    parseJSON (String "Add") = pure Add
    parseJSON (String "Mult") = pure Mult
    parseJSON (String "Div") = pure Div
    parseJSON (String "Mod") = pure Mod
    parseJSON a = typeMismatch "(Int, Int) -> Int" a

instance FromJSON (FreeFunc k Int Int) where
    parseJSON (String "Negate") = pure Negate
    parseJSON a = typeMismatch "Int -> Int" a

instance FromJSON (FreeFunc k (Integer, Integer) Integer) where
    parseJSON (String "Add") = pure Add
    parseJSON (String "Mult") = pure Mult
    parseJSON (String "Div") = pure Div
    parseJSON (String "Mod") = pure Mod
    parseJSON a = typeMismatch "(Integer, Integer) -> Integer" a

instance FromJSON (FreeFunc k Integer Integer) where
    parseJSON (String "Negate") = pure Negate
    parseJSON a = typeMismatch "Integer -> Integer" a

instance FromJSON (FreeFunc k (Float, Float) Float) where
    parseJSON (String "Add") = pure Add
    parseJSON (String "Mult") = pure Mult
    parseJSON a = typeMismatch "(Float, Float) -> Float" a

instance FromJSON (FreeFunc k Float Float) where
    parseJSON (String "Negate") = pure Negate
    parseJSON a = typeMismatch "Integer -> Integer" a

instance FromJSON (FreeFunc k (Double, Double) Double) where
    parseJSON (String "Add") = pure Add
    parseJSON (String "Mult") = pure Mult
    parseJSON a = typeMismatch "(Double, Double) -> Double" a

instance FromJSON (FreeFunc k Double Double) where
    parseJSON (String "Negate") = pure Negate
    parseJSON a = typeMismatch "Integer -> Integer" a

    {-
    FreeFunc p x x
    Compose :: FreeFunc p y z -> FreeFunc p x y -> FreeFunc p x z
    Copy :: FreeFunc p x (x, x)
    Consume :: FreeFunc p x ()
    First :: FreeFunc p a b -> FreeFunc p (a, x) (b, x)
    Second :: FreeFunc p a b -> FreeFunc p (x, a) (x, b)
    {- Unfirst :: FreeFunc p (a, x) (b, x) -> FreeFunc p a b -}
    Fst :: FreeFunc p (a, b) a
    Snd :: FreeFunc p (a, b) b
    InjectL :: FreeFunc p a (Either a b)
    InjectR :: FreeFunc p a (Either b a)
    Left' :: FreeFunc p a b -> FreeFunc p (Either a x) (Either b x)
    Right' :: FreeFunc p a b -> FreeFunc p (Either x a) (Either x b)
    {- Unleft :: FreeFunc p (Either a x) (Either b x) -> FreeFunc p a b -}
    Unify :: FreeFunc p (Either a a) a
    Tag :: FreeFunc p (Bool, a) (Either a a)
    Num :: (Num n, Show n, ToJSON n) => n -> FreeFunc p a n
    Swap :: FreeFunc p (a, b) (b, a)
    SwapEither :: FreeFunc p (Either a b) (Either b a)
    Reassoc :: FreeFunc p (a, (b, c)) ((a, b), c)
    ReassocEither :: FreeFunc p (Either a (Either b c)) (Either (Either a b) c)
    Lift :: p a b -> FreeFunc p a b
    -}

-- instance {-# OVERLAPPING #-} (forall a. ToJSON (k a a), x ~ y) ⇒ FromJSON (FreeFunc k x x) where
--     parseJSON (String "Id") = pure Id
--     parseJSON a             = fail $ "TypeError: got " <> show a <> ", expecting a -> a"
-- 
-- -- (forall a b. FromJSON (FreeFunc k a b), forall b c. FromJSON (FreeFunc k b c)) => 
-- instance {-# OVERLAPPING #-} FromJSON (FreeFunc k x z) where
--     parseJSON (Array [String "Compose", Array [a, b]]) = Compose <$> parseJSON a <*> parseJSON b
--     parseJSON a             = fail $  "TypeError: got " <> show a <> ", expecting (b -> c) -> (a -> b) -> a -> c"
-- 
-- -- (forall a. FromJSON (k a (a, a))) ⇒ 
-- instance {-# OVERLAPPING #-} FromJSON (FreeFunc k x (x, x)) where
--     parseJSON (String "Copy") = pure Copy
--     parseJSON a = fail $ "TypeError: got " <> show a <> ", expecting a -> (a, a)"

-- instance (FromJSON (p a b)) => FromJSON (FreeFunc p a b) where
--     parseJSON (Array [ String "Lift", x ] ) = pure $ Lift (parseJSON x)

instance Category (FreeFunc p) where
    id = Id
    (.) = Compose

instance Arrow (FreeFunc p) where
    arr = error "Arbitrary functions cannot be injected into FreeFunc. Use Archery functions instead."
    first = First
    second = Second

instance Cartesian (FreeFunc p) where
    copy = Copy
    consume = Consume
    fst' = Fst
    snd' = Snd

instance Strong (FreeFunc p) where
    first' = First
    second' = Second

{-}
instance Costrong (FreeFunc p) where
    unfirst = Unfirst
-}

instance Cocartesian (FreeFunc p) where
    injectL = InjectL
    injectR = InjectR
    unify = Unify
    tag = Tag

instance Choice (FreeFunc p) where
    left' = Left'
    right' = Right'

instance ArrowChoice (FreeFunc p) where
    left = Left'

{-}
instance Cochoice (FreeFunc p) where
    unleft = Unleft
-}

instance Symmetric (FreeFunc p) where
    swap = Swap
    swapEither = SwapEither
    reassoc = Reassoc
    reassocEither = ReassocEither

instance Numeric (FreeFunc p) where
    num = Num
    negate' = Negate
    add = Add
    mult = Mult
    div' = Div
    mod' = Mod
