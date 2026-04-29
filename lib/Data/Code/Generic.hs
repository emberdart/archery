{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-safe -Wno-unsafe #-}
{-# OPTIONS_GHC -Wno-orphans -ddump-splices #-}

module Data.Code.Generic where
 
import Control.Lens
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.MapSet

type Module = BSL.ByteString

type FunctionName = BSL.ByteString
 
type FunctionTypeFrom = BSL.ByteString

type FunctionTypeTo = BSL.ByteString

-- | Includes the function name to define the full (composed) function.
-- e.g. a(b(c))
type Shorthand = BSL.ByteString


-- | Includes the view longhand to define the full (composed) function.
-- e.g. (\x y z -> x (y z))(+1)(+ 1)(2)
type Longhand = BSL.ByteString

-- | A single function to be imported.
data Function = Function {
    _name      :: FunctionName,
    _typeFrom  :: FunctionTypeFrom,
    _typeTo    :: FunctionTypeTo,
    _fnShorthand :: Shorthand,
    _fnLonghand  :: Longhand
} deriving (Eq, Show, Ord)

makeClassy ''Function

type ExternalImports = MapSet Module FunctionName

type InternalImports = MapSet Module Function

-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> [("a", ["b"])] <> [("a", ["c"])] <> [("b", ["a", "b"]), ("c", ["a", "b"])] <> [("c", ["a", "c"]), ("a", ["a"])] :: Map Module (Set FunctionName)
-- fromList [("a",fromList ["b"]),("b",fromList ["a","b"]),("c",fromList ["a","b"])]

-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> [("a", ["b"])] <> [("a", ["c"])] <> [("b", ["a", "b"]), ("c", ["a", "b"])] <> [("c", ["a", "c"]), ("a", ["a"])] :: MapSet Module FunctionName
-- MapSet {getMapSet = fromList [("a",fromList ["a","b","c"]),("b",fromList ["a","b"]),("c",fromList ["a","b","c"])]}

-- TODO makeClassy
-- TODO capabilities?
-- class Has i a where

-- | Internal implementation of any programming language's code.
data Code a b = Code {
    -- | Anything outside of the project this function requires.
    _externalImports :: ExternalImports,
    -- | Anything inside of the project this function requires, including whether and how to export itself.
    _internalImports :: InternalImports,
    -- | The name of the current module. Probably "Main" unless making a library.
    -- @TODO Figure out whether we should be using Module here or in function
    -- it depends because we need to know whether to map Module to Set of Functions, or just have Set of Functions,
    -- when defining internal imports.
    -- TODO
    _shorthand       :: Shorthand,
    _longhand        :: Longhand
} deriving (Eq, Show)

makeClassy ''Code

-- instance IsString (Code a b) where
--     fromString s = Code [] Nothing (BSL.pack s) (BSL.pack s)

-- instance HasCode f a b ⇒ HasExport (f a b) where
--     export = export . code

{-
toImports ∷ (HasDefinition c) ⇒ c → Imports
toImports c = Imports [
        (
            module',
            [
                (
                    name',
                    Just (longhand c)
                )
                ]
        )
        ]
    ) (export c)
-}


{-}
class IsoNT unwrapped wrapped where
    wrap :: unwrapped -> wrapped
    unwrap :: wrapped -> unwrapped

instance HasCode f a b => IsoNT (f a b) (Code f a b) where
    unwrap = code
-}

{-}
instance (HasCode f a b, IsoNT (f a b) ) => IsString (f a b) where
    fromString =
-}
