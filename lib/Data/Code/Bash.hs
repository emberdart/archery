-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.Bash (Bash(..)) where

-- import Control.Category
import Control.Lens hiding (Choice)
import Data.Code.Generic

newtype Bash a b = Bash {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (Bash a b) k1 a k2 b where
    code = coerced

-- -- I'm not convinced bash even has view longhand unless we constantly redefine stuff
-- instance Category Bash where
--     id = Bash $ Code {
--         _externalImports = [],
--         _internalImports = [("category", [
--             Function {
--                 _name = "id",
--                 _typeFrom = "",
--                 _typeTo = "",
--                 _fnShorthand = "id",
--                 _fnLonghand = "function a() { echo $1; };"
--             }
--             ]
--         )],
--         _fnShorthand = "function a() { echo $1; };",
--         _longhand = "function a() { echo $1; };"
--     }
--     a . b = Bash $ Code {
--         _externalImports = view externalImports a <> view externalImports b,
--         _internalImports = view internalImports a <> view internalImports b <> [("category", [
--             Function {
--                 _name = "compose",
--                 _typeFrom = "",
--                 _typeTo = "",
--                 _fnShorthand = "compose",
--                 _fnLonghand = ""
--             }
--         ])],
--         _shorthand = "(" <> view shorthand a <> " . " <> view shorthand b <> ")",
--         _longhand = "" <> view longhand a <> ")(" <> view longhand b <> ")"
--     }
