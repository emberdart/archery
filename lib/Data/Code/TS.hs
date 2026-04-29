{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.TS (TS(..)) where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
-- import Control.Category.Execute.TS.Imports
-- import Control.Category.Execute.TS.Longhand
-- import Control.Category.Execute.TS.Shorthand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Shorthand
import Control.Category.Execute.Stdio.Imports
import Control.Category.Execute.Stdio.Longhand
import Control.Category.Execute.Stdio.Shorthand
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                        hiding (bracket)
import Control.Lens hiding (Choice)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8               qualified as BSL
import Data.Code.Generic
import Data.Foldable
-- import Data.Map                                         (Map)
import Data.Map                                 qualified as M
import Data.MapSet
-- import Data.Maybe
import Data.Render.Library.External.Imports
import Data.Render.Library.External.Longhand
import Data.Render.Library.External.Shorthand
import Data.Render.Library.Internal.Imports
import Data.Render.Library.Internal.Longhand
import Data.Render.Library.Internal.Shorthand
import Data.Render.Program.Imports
import Data.Render.Program.Longhand
import Data.Render.Program.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Set                                         (Set)
import Data.Set                                 qualified as S
-- import Data.String
import Data.Text.Encoding qualified as TE
-- import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                                  hiding (id, (.))
import System.Process
import Text.Read
import Control.Arrow

newtype TS a b = TS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (TS a b) k1 a k2 b where
    code = coerced

toExternalCLIImports ∷ TS a b → [String]
toExternalCLIImports js = GHC.IsList.toList (view externalImports js) >>=
    \(moduleName, functions) -> ["import { " <> BSL.unpack (BSL.intercalate ", " (S.toList functions)) <> " } from \"" <> BSL.unpack moduleName <> "\";"]

toInternalCLIImports ∷ TS a b → [String]
toInternalCLIImports js = GHC.IsList.toList (view internalImports js) >>=
    \(moduleName, functions) -> ["import { " <> BSL.unpack (BSL.intercalate ", " (view functionName <$> S.toList functions)) <> " } from \"" <> BSL.unpack moduleName <> "\";"]

toShorthandCLIDefinitions ∷ TS a b → [String]
toShorthandCLIDefinitions js = GHC.IsList.toList (view internalImports js) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        BSL.unpack $
            "const " <> view functionName function' <> " = " <> view functionLonghand function' <> ";"
        ]

toInternalFileImports ∷ TS a b → [BSL.ByteString]
toInternalFileImports js = (
    \(moduleName, functions) ->
        "import { " <> BSL.intercalate ", " (view functionName <$> S.toList functions) <> " } from \"" <> moduleName <> "\";"
    ) <$> M.toList (getMapSet (view internalImports js))

toShorthandFileDefinitions ∷ TS a b → [BSL.ByteString]
toShorthandFileDefinitions js = foldMap' (\(_, functions) ->
    foldMap' (\fn ->
        [
            "export const (" <> view functionName fn <> ": " <> view functionTypeFrom  fn <> " => " <> view functionTypeTo  fn <> ") = " <> view functionLonghand fn <> "\n"
        ]
    )
    functions
    ) $ M.toList (getMapSet (view internalImports js))

toExternalFileImports ∷ TS a b → [BSL.ByteString]
toExternalFileImports js = (
    \(moduleName, functions) ->
        "import " <> BSL.intercalate ", " (S.toList functions) <> " from \"" <> moduleName <> "\";"
    ) <$> M.toList (getMapSet (view externalImports js))

instance RenderStatementLonghand (TS a b) where
    renderStatementLonghand = view longhand

instance RenderStatementShorthand (TS a b) where
    renderStatementShorthand = view shorthand

instance RenderLibraryInternalShorthand (TS a b) where
    renderLibraryInternalShorthand _ = []

instance RenderLibraryInternalLonghand (TS a b) where
    renderLibraryInternalLonghand _ = []

instance RenderLibraryInternalImports (TS a b) where
    renderLibraryInternalImports _ = []

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderLibraryExternalShorthand (TS a b) where
    renderLibraryExternalShorthand _newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
       --"module " <> "module " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        "/**\n * @param {" <> newFunctionTypeFrom <> "} param\n * @returns {" <> newFunctionTypeTo <> "}\n */\n" <>
        "export const " <> newFunctionName <> " = " <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderLibraryExternalLonghand (TS a b) where
    renderLibraryExternalLonghand _newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view functionName cat <> " = " <> renderStatementLonghand cat
        "/**\n * @param {" <> newFunctionTypeFrom <> "} param\n * @returns {" <> newFunctionTypeTo <> "}\n */\n" <>
        "export const " <> newFunctionName <> " = " <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderLibraryExternalImports (TS a b) where
    renderLibraryExternalImports _newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
       BSL.unlines (toExternalFileImports cat) <>
       BSL.unlines (toInternalFileImports cat) <>
       "/**\n * @param {" <> newFunctionTypeFrom <> "} param\n * @returns {" <> newFunctionTypeTo <> "}\n */\n" <>
        "export const " <> newFunctionName <> " = " <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (TS a b) where
    renderProgramShorthand cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view functionName cat <> " = " <> renderStatementShorthand cat
        "\n" <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (TS a b) where
    renderProgramLonghand cat =
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view functionName cat <> " = " <> renderStatementLonghand cat
        "\n" <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderProgramImports (TS a b) where
    renderProgramImports cat =
       BSL.unlines (toExternalFileImports cat) <>
       BSL.unlines (toInternalFileImports cat) <>
       "\n" <> renderStatementShorthand cat

instance Bracket TS where
    bracket f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "(" <> renderStatementShorthand f <> ")",
        _longhand = "(" <> renderStatementLonghand f <> ")"
    }
instance Category TS where
    id = TS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "control/category", [
                    Function {
                        _functionName = "id",
                        _functionTypeFrom = "a",
                        _functionTypeTo = "a",
                        _functionShorthand = "a => a",
                        _functionLonghand = "a => a"
                    }
                ]
            )
        ],
        _shorthand = "id",
        _longhand = "a => a"
    }
    a . b = TS $ Code {
        _externalImports = view externalImports a <> view externalImports b,
        _internalImports = view internalImports a <> view internalImports b <> [
            ("control/category", [
                Function {
                    _functionName = "compose",
                    _functionTypeFrom = "TODO",
                    _functionTypeTo = "TODO",
                    _functionShorthand = "f => g => x => f(g(x))",
                    _functionLonghand = "f => g => x => f(g(x))"
                }
            ])
        ],
        _shorthand = "compose(" <> view shorthand a <> ")(" <> view shorthand b <> ")",
        _longhand = "(f => g => x => f(g(x)))(" <> view longhand a <> ")(" <> view longhand b <> ")"
    }

instance Cartesian TS where
    copy = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "copy",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "[a]",
                    _functionShorthand = "x => ([x, x])",
                    _functionLonghand = "x => ([x, x])"
                }
                ]
            )
        ],
        _shorthand = "copy",
        _longhand = "x => ([x, x])"
    }
    consume = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "consume",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "null",
                    _functionShorthand = "x => null",
                    _functionLonghand = "x => null"
                }
                ]
            )
        ],
        _shorthand = "consume",
        _longhand = "x => null"
    }
    fst' = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "fst",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "a",
                    _functionShorthand = "([a, b]) => a",
                    _functionLonghand = "([a, b]) => a"
                }
                ]
            )
        ],
        _shorthand = "fst",
        _longhand = "([a, b]) => a"
    }
    snd' = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "snd",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "b",
                    _functionShorthand = "([a, b]) => b",
                    _functionLonghand = "([a, b]) => b"
                }
                ]
            )
        ],
        _shorthand = "snd",
        _longhand = "([a, b]) => b"
    }

instance Cocartesian TS where
    injectL = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "injectL",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "{ Left: a } | { Right: a }",
                    _functionShorthand = "a => ({ Left: a })",
                    _functionLonghand = "a => ({ Left: a })"
                }
                ]
            )
        ],
        _shorthand = "injectL",
        _longhand = "a => ({ Left: a })"
    }
    injectR = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "injectR",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "{ Left: a } | { Right: a }",
                    _functionShorthand = "a => ({ Right: a })",
                    _functionLonghand = "a => ({ Right: a })"
                }
                ]
            )
        ],
        _shorthand = "injectR",
        _longhand = "a => ({ Right: a })"
    }
    unify = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "unify",
                    _functionTypeFrom = "{ Left: a } | { Right: a }",
                    _functionTypeTo = "a",
                    _functionShorthand = "x => x.Right || x.Left",
                    _functionLonghand = "x => x.Right || x.Left"
                }
                ]
            )
        ],
        _shorthand = "unify",
        _longhand = "x => x.Right ?? x.Left"
    }
    tag = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "tag",
                    _functionTypeFrom = "[boolean, a]",
                    _functionTypeTo = "{ Left: a } | { Right: a }",
                    _functionShorthand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})",
                    _functionLonghand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})"
                }
                ]
            )
        ],
        _shorthand = "tag",
        _longhand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: TS String String
-- TS {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "a", _functionTypeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_functionName = "fst", _functionTypeFrom = "(a, b)", _functionTypeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_functionName = "(.)", _functionTypeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _functionTypeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: TS String String)

instance Strong TS where
    first' f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([(" <> view shorthand f <> ")(a), b])",
        _longhand = "([a, b]) => ([(" <> view longhand f <> ")(a), b])"
    }
    second' f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([a, (" <> view shorthand f <> ")(b)])",
        _longhand = "([a, b]) => ([a, (" <> view longhand f <> ")(b)])"
    }

instance Arrow TS where
    arr = error "Arbitrary functions cannot be injected into TS. Use Archery functions instead."
    first f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([(" <> view shorthand f <> ")(a), b])",
        _longhand = "([a, b]) => ([(" <> view longhand f <> ")(a), b])"
    }
    second f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([a, (" <> view shorthand f <> ")(b)])",
        _longhand = "([a, b]) => ([a, (" <> view longhand f <> ")(b)])"
    }

instance Choice TS where
    left' f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Left && ({ Left: (" <> view shorthand f <> ")(x.Left) }) || x",
        _longhand = "x => x.Left && ({ Left: (" <> view longhand f <> ")(x.Left) }) || x"
    }
    right' f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Right && ({ Right: (" <> view shorthand f <> ")(x.Right) }) || x",
        _longhand = "x => x.Right && ({ Right: (" <> view longhand f <> ")(x.Right) }) || x"
    }

instance ArrowChoice TS where
    left f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Left && ({ Left: (" <> view shorthand f <> ")(x.Left) }) || x",
        _longhand = "x => x.Left && ({ Left: (" <> view longhand f <> ")(x.Left) }) || x"
    }
    right f = TS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Right && ({ Right: (" <> view shorthand f <> ")(x.Right) }) || x",
        _longhand = "x => x.Right && ({ Right: (" <> view longhand f <> ")(x.Right) }) || x"
    }

instance Symmetric TS where
    swap = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _functionName = "swap",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "[b, a]",
                    _functionShorthand = "([a, b]) => ([b, a])",
                    _functionLonghand = "([a, b]) => ([b, a])"
                }
                ]
            )
            ],
        _shorthand = "swap",
        _longhand = "([a, b]) => ([b, a])"
    }
    swapEither = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _functionName = "swapEither",
                    _functionTypeFrom = "{ Left : x }  | { Right : x }",
                    _functionTypeTo = "{ Left : x }  | { Right : x }",
                    _functionShorthand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })",
                    _functionLonghand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })"
                }
                ]
            )
        ],
        _shorthand = "swapEither",
        _longhand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })"
    }
    reassoc = TS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "control/category/symmetric",
                [
                    Function {
                        _functionName = "reassoc",
                        _functionTypeFrom = "[a, [b, c]]",
                        _functionTypeTo = "[[a, b], c]",
                        _functionShorthand = "([a, [b, c]]) => ([[a, b], c])",
                        _functionLonghand = "([a, [b, c]]) => ([[a, b], c])"
                    }
                    ]
            )
            ],
        _shorthand = "reassoc",
        _longhand = "([a, [b, c]]) => ([[a, b], c])"
    }
    reassocEither = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _functionName = "reassocEither",
                    _functionTypeFrom = "TODO",
                    _functionTypeTo = "TODO",
                    -- \\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }
                    _functionShorthand = "x => { throw new Error(\"TODO: reassocEither\"); }",
                    _functionLonghand = "x => { throw new Error(\"TODO: reassocEither\"); }"
                }
                ]
            )
            ],
        _shorthand = "reassocEither",
        _longhand = "TODO"
    }

-- instance Cochoice TS where

-- instance Costrong TS where

-- instance Apply TS where

instance PrimitiveBool TS where
    eq = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/bool", [
                Function {
                    _functionName = "eq",
                    _functionTypeFrom = "[a, a]",
                    _functionTypeTo = "boolean",
                    _functionShorthand = "([x, y]) => x === y",
                    _functionLonghand = "([x, y]) => x === y"
                }
            ])
        ],
        _shorthand = "eq",
        _longhand = "([x, y]) => x === y"
    }

instance PrimitiveConsole TS where
    outputString = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/console", [
                Function {
                    _functionName = "outputString",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "void",
                    _functionShorthand = "console.log",
                    _functionLonghand = "console.log"
                }
            ])
        ],
        _shorthand = "outputString",
        _longhand = "console.log"
    }
    inputString = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/console", [
                Function {
                    _functionName = "inputString",
                    _functionTypeFrom = "void",
                    _functionTypeTo = "string",
                    _functionShorthand = "x => { throw new Error(\"TODO Node or browser?\"); }",
                    _functionLonghand = "x => { throw new Error(\"TODO Node or browser?\"); }"
                }
            ])
        ],
        _shorthand = "inputString",
        _longhand = "TODO Node or browser?"
    }

instance PrimitiveExtra TS where
    intToString = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/extra", [
                Function {
                    _functionName = "intToString",
                    _functionTypeFrom = "number",
                    _functionTypeTo = "string",
                    _functionShorthand = "String",
                    _functionLonghand = "String"
                }
                ]
            )
            ],
        _shorthand = "intToString",
        _longhand = "String"
    }
    concatString = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/extra", [
                Function {
                    _functionName = "concatString",
                    _functionTypeFrom = "[string, string]",
                    _functionTypeTo = "string",
                    _functionShorthand = "([a, b]) => a + b",
                    _functionLonghand = "([a, b]) => a + b"
                }
                ]
            )
            ],
        _shorthand = "concatString",
        _longhand = "([a, b]) => a + b"
    }
    constString s = TS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => \"" <> BSL.fromStrict (TE.encodeUtf8 s) <> "\"",
        _longhand = "_x => \"" <> BSL.fromStrict (TE.encodeUtf8 s) <> "\""
    }
instance PrimitiveFile TS where
    readFile' = TS $ Code {
        _externalImports = [
            ("fs", ["readFileSync"])
        ],
        _internalImports = [
            ("control/category/primitive/file", [
                Function {
                    _functionName = "readFile'",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "string",
                    _functionShorthand = "readFileSync",
                    _functionLonghand = "x => readFileSync(x)"
                }
                ]
            )
            ],
        _shorthand = "readFile'",
        _longhand = "x => require('fs').readFileSync(x)"
    }
    writeFile' = TS $ Code {
        _externalImports = [
            ("fs", ["writeFileSync"])
        ],
        _internalImports = [
            ("control/category/primitive/file", [
                Function {
                    _functionName = "writeFile'",
                    _functionTypeFrom = "[string, string]",
                    _functionTypeTo = "void",
                    _functionShorthand = "x => writeFileSync(x[0], x[1])",
                    _functionLonghand = "x => writeFileSync(x[0], x[1])"
                }
                ]
            )
            ],
        _shorthand = "writeFile'",
        _longhand = "x => writeFileSync(x[0], x[1])"
    }

instance PrimitiveString TS where
    reverseString = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/string", [
                Function {
                    _functionName = "reverseString",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "string",
                    _functionShorthand = "x => x.split('').reverse().join('')",
                    _functionLonghand = "x => x.split('').reverse().join('')"
                }
                ]
            )
            ],
        _shorthand = "reverseString",
        _longhand = "x => x.split('').reverse().join('')"
    }

instance Numeric TS where
    num n = TS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => " <> BSL.pack (show n),
        _longhand = "_x => " <> BSL.pack (show n)
    }
    negate' = TS $ Code {
        _externalImports = [],
        _internalImports = [(
            "control/category/numeric", [
                Function {
                    _functionName = "negate",
                    _functionTypeFrom = "number",
                    _functionTypeTo = "number",
                    _functionShorthand = "x => -x",
                    _functionLonghand = "x => -x"
                }
            ]
        )],
        _shorthand = "negate",
        _longhand = "x => -x"
    }
    add = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "add",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => x + y",
                    _functionLonghand = "([x, y]) => x + y"
                }
                ]
            )
            ],
        _shorthand = "add",
        _longhand = "([x, y]) => x + y"
    }
    mult = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "mult",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => x * y",
                    _functionLonghand = "([x, y]) => x * y"
                }
                ]
            )
            ],
        _shorthand = "mult",
        _longhand = "([x, y]) => x * y"
    }
    div' = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "div",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => Math.floor(x / y)",
                    _functionLonghand = "([x, y]) => Math.floor(x / y)"
                }
                ]
            )
            ],
        _shorthand = "div",
        _longhand = "([x, y]) => Math.floor(x / y)"
    }
    mod' = TS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "mod",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => x % y",
                    _functionLonghand = "([x, y]) => x % y"
                }
                ]
            )
            ],
        _shorthand = "mod",
        _longhand = "([x, y]) => x % y"
    }

{-}
-- I don't quite know how to call node or cabal new-repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
-}

-- instance ExecuteTSLonghand TS where
--     executeTSLonghand cat param = do
--         let params ∷ [String]
--             params = [
--                 "-e",
--                 unwords (toExternalCLIImports cat) <>
--                     "(" <> BSL.unpack (renderStatementLonghand cat) <> ") (" <> param <> ")"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)
--
-- instance ExecuteTSShorthand TS where
--     executeTSShorthand cat param = do
--         let params ∷ [String]
--             params = [
--                 "-e",
--                 unwords (toExternalCLIImports cat) <>
--                     unwords (toShorthandCLIDefinitions cat) <>
--                     "'(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)
--
-- instance ExecuteTSImports TS where
--     executeTSImports cat param = do
--         let params ∷ [String]
--             params = [
--                 "-e",
--                 unwords (toExternalCLIImports cat) <>
--                     unwords (toInternalCLIImports cat) <>
--                     "'(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within TS sessions. Let's try to use Pure/Monadic... or maybe TSPure / TSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand TS where
    executeStdioLonghand cat stdin = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    "(" <> BSL.unpack (renderStatementLonghand cat) <> ")(null)"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioImports TS where
    executeStdioImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e",
                -- toCLIImports cat <>
                -- [
                    "(" <> BSL.unpack (renderStatementShorthand cat) <> ")(null)"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioShorthand TS where
    executeStdioShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    "(" <> BSL.unpack (renderStatementShorthand cat) <> ")(null)"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteJSONLonghand TS where
    executeJSONLonghand cat param = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    "console.log(JSON.stringify((" <> BSL.unpack (renderStatementLonghand cat) <> ")(JSON.parse(" <> show (BSL.unpack (encode param)) <> "))))"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONShorthand TS where
    executeJSONShorthand :: (ToJSON input, FromJSON output, MonadIO m) ⇒ TS input output → input → m output
    executeJSONShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    unwords (toShorthandCLIDefinitions cat) <>
                    "console.log(JSON.stringify((" <> BSL.unpack (renderStatementShorthand cat) <> ")(JSON.parse(" <> show (BSL.unpack (encode param)) <> "))))"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONImports TS where
    executeJSONImports cat param = do
        let params ∷ [String]
            params = [
                    "-e",
                    unwords (toExternalCLIImports cat) <>
                        unwords (toInternalCLIImports cat) <>
                        "console.log(JSON.stringify((" <> BSL.unpack (renderStatementShorthand cat) <> ")(JSON.parse(" <> show (BSL.unpack (encode param)) <> "))))"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))
