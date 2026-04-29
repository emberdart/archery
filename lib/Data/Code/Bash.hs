-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.Bash (Bash(..)) where

-- import Control.Category
import Data.Code.Generic

newtype Bash a b = Bash {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode Bash a b where
  code = _code

-- -- I'm not convinced bash even has view longhand unless we constantly redefine stuff
-- instance Category Bash where
--     id = Bash $ Code {
--         _externalImports = [],
--         _internalImports = [("category", [
--             Function {
--                 _functionName = "id",
--                 _functionTypeFrom = "",
--                 _functionTypeTo = "",
--                 _functionShorthand = "id",
--                 _functionLonghand = "function a() { echo $1; };"
--             }
--             ]
--         )],
--         _shorthand = "function a() { echo $1; };",
--         _longhand = "function a() { echo $1; };"
--     }
--     a . b = Bash $ Code {
--         _externalImports = view externalImports a <> view externalImports b,
--         _internalImports = view internalImports a <> view internalImports b <> [("category", [
--             Function {
--                 _functionName = "compose",
--                 _functionTypeFrom = "",
--                 _functionTypeTo = "",
--                 _functionShorthand = "compose",
--                 _functionLonghand = ""
--             }
--         ])],
--         _shorthand = "(" <> view shorthand a <> " . " <> view shorthand b <> ")",
--         _longhand = "" <> view longhand a <> ")(" <> view longhand b <> ")"
--     }
