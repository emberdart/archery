{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.JS (JS(..)) where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
-- import Control.Category.Execute.JS.Imports
-- import Control.Category.Execute.JS.Longhand
-- import Control.Category.Execute.JS.Shorthand
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
import GHC.IsList
import Prelude                                  hiding (id, (.))
import System.Process
import Text.Read
import Control.Arrow
import System.Exit

newtype JS a b = JS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (JS a b) k1 a k2 b where
    code = coerced

-- moduleNameToFilename ∷ BSL.ByteString → FilePath
-- moduleNameToFilename = BSL.unpack . (<> ".js")

toExternalCLIImports ∷ JS a b → [String]
toExternalCLIImports js = GHC.IsList.toList (view externalImports js) >>=
    \(moduleName, functions) -> ["import { " <> BSL.unpack (BSL.intercalate ", " (S.toList functions)) <> " } from \"" <> BSL.unpack moduleName <> "\";"]

toInternalCLIImports ∷ JS a b → [String]
toInternalCLIImports js = GHC.IsList.toList (view internalImports js) >>=
    \(moduleName, functions) -> ["import { " <> BSL.unpack (BSL.intercalate ", " (view name <$> S.toList functions)) <> " } from \"" <> BSL.unpack moduleName <> "\";"]

toShorthandCLIDefinitions ∷ JS a b → [String]
toShorthandCLIDefinitions js = GHC.IsList.toList (view internalImports js) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        BSL.unpack $
            "const " <> view name function' <> " = " <> view fnLonghand function' <> ";"
        ]

toInternalFileImports ∷ JS a b → [BSL.ByteString]
toInternalFileImports js = (
    \(moduleName, functions) ->
        "import { " <> BSL.intercalate ", " (view name <$> S.toList functions) <> " } from \"" <> moduleName <> "\";"
    ) <$> M.toList (getMapSet (view internalImports js))

toShorthandFileDefinitions ∷ JS a b → [BSL.ByteString]
toShorthandFileDefinitions js = foldMap' (\(_, functions) ->
    foldMap' (\fn ->
        [
            "/**\n * @param {" <> view typeFrom  fn <> "} param\n * @returns {" <> view typeTo  fn <> "}\n */\n" <>
            "export const " <> view name fn <> " = " <> view fnLonghand fn <> "\n"
        ]
    )
    functions
    ) $ M.toList (getMapSet (view internalImports js))

toExternalFileImports ∷ JS a b → [BSL.ByteString]
toExternalFileImports js = (
    \(moduleName, functions) ->
        "import " <> BSL.intercalate ", " (S.toList functions) <> " from \"" <> moduleName <> "\";"
    ) <$> M.toList (getMapSet (view externalImports js))

instance RenderStatementLonghand (JS a b) where
    renderStatementLonghand = view longhand

instance RenderStatementShorthand (JS a b) where
    renderStatementShorthand = view shorthand

instance RenderLibraryInternalShorthand (JS a b) where
    renderLibraryInternalShorthand _ = []

instance RenderLibraryInternalLonghand (JS a b) where
    renderLibraryInternalLonghand _ = []

instance RenderLibraryInternalImports (JS a b) where
    renderLibraryInternalImports _ = []

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderLibraryExternalShorthand (JS a b) where
    renderLibraryExternalShorthand _newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
       --"module " <> "module " <> module' cat <> " (" <> view name cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        "/**\n * @param {" <> newFunctionTypeFrom <> "} param\n * @returns {" <> newFunctionTypeTo <> "}\n */\n" <>
        "export const " <> newFunctionName <> " = " <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderLibraryExternalLonghand (JS a b) where
    renderLibraryExternalLonghand _newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view name cat <> " :: " <> view typeFrom  cat <> " -> " <> view typeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view name cat <> " = " <> renderStatementLonghand cat
        "/**\n * @param {" <> newFunctionTypeFrom <> "} param\n * @returns {" <> newFunctionTypeTo <> "}\n */\n" <>
        "export const " <> newFunctionName <> " = " <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderLibraryExternalImports (JS a b) where
    renderLibraryExternalImports _newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
       BSL.unlines (toExternalFileImports cat) <>
       BSL.unlines (toInternalFileImports cat) <>
       "/**\n * @param {" <> newFunctionTypeFrom <> "} param\n * @returns {" <> newFunctionTypeTo <> "}\n */\n" <>
        "export const " <> newFunctionName <> " = " <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (JS a b) where
    renderProgramShorthand cat =
        -- "\nmodule " <> module' cat <> " (" <> view name cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> view name cat <> " :: " <> view typeFrom  cat <> " -> " <> view typeTo  cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view name cat <> " = " <> renderStatementShorthand cat
        "\n" <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (JS a b) where
    renderProgramLonghand cat =
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view name cat <> " :: " <> view typeFrom  cat <> " -> " <> view typeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view name cat <> " = " <> renderStatementLonghand cat
        "\n" <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderProgramImports (JS a b) where
    renderProgramImports cat =
       BSL.unlines (toExternalFileImports cat) <>
       BSL.unlines (toInternalFileImports cat) <>
       "\n" <> renderStatementShorthand cat

instance Bracket JS where
    bracket f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "(" <> renderStatementShorthand f <> ")",
        _longhand = "(" <> renderStatementLonghand f <> ")"
    }

instance Category JS where
    id = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "control/category", [
                    Function {
                        _name = "id",
                        _typeFrom = "a",
                        _typeTo = "a",
                        _fnShorthand = "a => a",
                        _fnLonghand = "a => a"
                    }
                ]
            )
        ],
        _shorthand = "id",
        _longhand = "a => a"
    }
    a . b = JS $ Code {
        _externalImports = view externalImports a <> view externalImports b,
        _internalImports = view internalImports a <> view internalImports b <> [
            ("control/category", [
                Function {
                    _name = "compose",
                    _typeFrom = "TODO",
                    _typeTo = "TODO",
                    _fnShorthand = "f => g => x => f(g(x))",
                    _fnLonghand = "f => g => x => f(g(x))"
                }
            ])
        ],
        _shorthand = "compose(" <> view shorthand a <> ")(" <> view shorthand b <> ")",
        _longhand = "(f => g => x => f(g(x)))(" <> view longhand a <> ")(" <> view longhand b <> ")"
    }

instance Cartesian JS where
    copy = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _name = "copy",
                    _typeFrom = "a",
                    _typeTo = "[a]",
                    _fnShorthand = "x => ([x, x])",
                    _fnLonghand = "x => ([x, x])"
                }
                ]
            )
        ],
        _shorthand = "copy",
        _longhand = "x => ([x, x])"
    }
    consume = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _name = "consume",
                    _typeFrom = "a",
                    _typeTo = "null",
                    _fnShorthand = "x => null",
                    _fnLonghand = "x => null"
                }
                ]
            )
        ],
        _shorthand = "consume",
        _longhand = "x => null"
    }
    fst' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _name = "fst",
                    _typeFrom = "[a, b]",
                    _typeTo = "a",
                    _fnShorthand = "([a, b]) => a",
                    _fnLonghand = "([a, b]) => a"
                }
                ]
            )
        ],
        _shorthand = "fst",
        _longhand = "([a, b]) => a"
    }
    snd' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _name = "snd",
                    _typeFrom = "[a, b]",
                    _typeTo = "b",
                    _fnShorthand = "([a, b]) => b",
                    _fnLonghand = "([a, b]) => b"
                }
                ]
            )
        ],
        _shorthand = "snd",
        _longhand = "([a, b]) => b"
    }

instance Cocartesian JS where
    injectL = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _name = "injectL",
                    _typeFrom = "a",
                    _typeTo = "{ Left: a } | { Right: a }",
                    _fnShorthand = "a => ({ Left: a })",
                    _fnLonghand = "a => ({ Left: a })"
                }
                ]
            )
        ],
        _shorthand = "injectL",
        _longhand = "a => ({ Left: a })"
    }
    injectR = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _name = "injectR",
                    _typeFrom = "a",
                    _typeTo = "{ Left: a } | { Right: a }",
                    _fnShorthand = "a => ({ Right: a })",
                    _fnLonghand = "a => ({ Right: a })"
                }
                ]
            )
        ],
        _shorthand = "injectR",
        _longhand = "a => ({ Right: a })"
    }
    unify = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _name = "unify",
                    _typeFrom = "{ Left: a } | { Right: a }",
                    _typeTo = "a",
                    _fnShorthand = "x => x.Right || x.Left",
                    _fnLonghand = "x => x.Right || x.Left"
                }
                ]
            )
        ],
        _shorthand = "unify",
        _longhand = "x => x.Right ?? x.Left"
    }
    tag = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _name = "tag",
                    _typeFrom = "[boolean, a]",
                    _typeTo = "{ Left: a } | { Right: a }",
                    _fnShorthand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})",
                    _fnLonghand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})"
                }
                ]
            )
        ],
        _shorthand = "tag",
        _longhand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: JS String String
-- JS {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_name = "copy", _typeFrom = "a", _typeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_name = "fst", _typeFrom = "(a, b)", _typeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_name = "(.)", _typeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _typeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: JS String String)

instance Arrow JS where
    arr = error "Arbitrary functions cannot be injected into JS. Use Archery functions instead."
    first f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([(" <> view shorthand f <> ")(a), b])",
        _longhand = "([a, b]) => ([(" <> view longhand f <> ")(a), b])"
    }
    second f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([a, (" <> view shorthand f <> ")(b)])",
        _longhand = "([a, b]) => ([a, (" <> view longhand f <> ")(b)])"
    }
    
instance Strong JS where
    first' f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([(" <> view shorthand f <> ")(a), b])",
        _longhand = "([a, b]) => ([(" <> view longhand f <> ")(a), b])"
    }
    second' f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "([a, b]) => ([a, (" <> view shorthand f <> ")(b)])",
        _longhand = "([a, b]) => ([a, (" <> view longhand f <> ")(b)])"
    }

instance Choice JS where
    left' f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Left && ({ Left: (" <> view shorthand f <> ")(x.Left) }) || x",
        _longhand = "x => x.Left && ({ Left: (" <> view longhand f <> ")(x.Left) }) || x"
    }
    right' f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Right && ({ Right: (" <> view shorthand f <> ")(x.Right) }) || x",
        _longhand = "x => x.Right && ({ Right: (" <> view longhand f <> ")(x.Right) }) || x"
    }

instance ArrowChoice JS where
    left f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Left && ({ Left: (" <> view shorthand f <> ")(x.Left) }) || x",
        _longhand = "x => x.Left && ({ Left: (" <> view longhand f <> ")(x.Left) }) || x"
    }
    right f = JS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "x => x.Right && ({ Right: (" <> view shorthand f <> ")(x.Right) }) || x",
        _longhand = "x => x.Right && ({ Right: (" <> view longhand f <> ")(x.Right) }) || x"
    }

instance Symmetric JS where
    swap = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _name = "swap",
                    _typeFrom = "[a, b]",
                    _typeTo = "[b, a]",
                    _fnShorthand = "([a, b]) => ([b, a])",
                    _fnLonghand = "([a, b]) => ([b, a])"
                }
                ]
            )
            ],
        _shorthand = "swap",
        _longhand = "([a, b]) => ([b, a])"
    }
    swapEither = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _name = "swapEither",
                    _typeFrom = "{ Left : x }  | { Right : x }",
                    _typeTo = "{ Left : x }  | { Right : x }",
                    _fnShorthand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })",
                    _fnLonghand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })"
                }
                ]
            )
        ],
        _shorthand = "swapEither",
        _longhand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })"
    }
    reassoc = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "control/category/symmetric",
                [
                    Function {
                        _name = "reassoc",
                        _typeFrom = "[a, [b, c]]",
                        _typeTo = "[[a, b], c]",
                        _fnShorthand = "([a, [b, c]]) => ([[a, b], c])",
                        _fnLonghand = "([a, [b, c]]) => ([[a, b], c])"
                    }
                    ]
            )
            ],
        _shorthand = "reassoc",
        _longhand = "([a, [b, c]]) => ([[a, b], c])"
    }
    reassocEither = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _name = "reassocEither",
                    _typeFrom = "TODO",
                    _typeTo = "TODO",
                    -- \\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }
                    _fnShorthand = "x => { throw new Error(\"TODO: reassocEither\"); }",
                    _fnLonghand = "x => { throw new Error(\"TODO: reassocEither\"); }"
                }
                ]
            )
            ],
        _shorthand = "reassocEither",
        _longhand = "TODO"
    }

-- instance Cochoice JS where

-- instance Costrong JS where

-- instance Apply JS where

instance PrimitiveBool JS where
    eq = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/bool", [
                Function {
                    _name = "eq",
                    _typeFrom = "[a, a]",
                    _typeTo = "boolean",
                    _fnShorthand = "([x, y]) => x === y",
                    _fnLonghand = "([x, y]) => x === y"
                }
            ])
        ],
        _shorthand = "eq",
        _longhand = "([x, y]) => x === y"
    }

instance PrimitiveConsole JS where
    outputString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/console", [
                Function {
                    _name = "outputString",
                    _typeFrom = "string",
                    _typeTo = "void",
                    _fnShorthand = "console.log",
                    _fnLonghand = "console.log"
                }
            ])
        ],
        _shorthand = "outputString",
        _longhand = "console.log"
    }
    inputString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/console", [
                Function {
                    _name = "inputString",
                    _typeFrom = "void",
                    _typeTo = "string",
                    _fnShorthand = "x => { throw new Error(\"TODO Node or browser?\"); }",
                    _fnLonghand = "x => { throw new Error(\"TODO Node or browser?\"); }"
                }
            ])
        ],
        _shorthand = "inputString",
        _longhand = "TODO Node or browser?"
    }

instance PrimitiveExtra JS where
    intToString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/extra", [
                Function {
                    _name = "intToString",
                    _typeFrom = "number",
                    _typeTo = "string",
                    _fnShorthand = "String",
                    _fnLonghand = "String"
                }
                ]
            )
            ],
        _shorthand = "intToString",
        _longhand = "String"
    }
    concatString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/extra", [
                Function {
                    _name = "concatString",
                    _typeFrom = "[string, string]",
                    _typeTo = "string",
                    _fnShorthand = "([a, b]) => a + b",
                    _fnLonghand = "([a, b]) => a + b"
                }
                ]
            )
            ],
        _shorthand = "concatString",
        _longhand = "([a, b]) => a + b"
    }
    constString s = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => \"" <> BSL.fromStrict (TE.encodeUtf8 s) <> "\"",
        _longhand = "_x => \"" <> BSL.fromStrict (TE.encodeUtf8 s) <> "\""
    }

instance PrimitiveFile JS where
    readFile' = JS $ Code {
        _externalImports = [
            ("fs", ["readFileSync"])
        ],
        _internalImports = [
            ("control/category/primitive/file", [
                Function {
                    _name = "readFile'",
                    _typeFrom = "string",
                    _typeTo = "string",
                    _fnShorthand = "readFileSync",
                    _fnLonghand = "x => readFileSync(x)"
                }
                ]
            )
            ],
        _shorthand = "readFile'",
        _longhand = "x => require('fs').readFileSync(x)"
    }
    writeFile' = JS $ Code {
        _externalImports = [
            ("fs", ["writeFileSync"])
        ],
        _internalImports = [
            ("control/category/primitive/file", [
                Function {
                    _name = "writeFile'",
                    _typeFrom = "[string, string]",
                    _typeTo = "void",
                    _fnShorthand = "x => writeFileSync(x[0], x[1])",
                    _fnLonghand = "x => writeFileSync(x[0], x[1])"
                }
                ]
            )
            ],
        _shorthand = "writeFile'",
        _longhand = "x => writeFileSync(x[0], x[1])"
    }

instance PrimitiveString JS where
    reverseString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/string", [
                Function {
                    _name = "reverseString",
                    _typeFrom = "string",
                    _typeTo = "string",
                    _fnShorthand = "x => x.split('').reverse().join('')",
                    _fnLonghand = "x => x.split('').reverse().join('')"
                }
                ]
            )
            ],
        _shorthand = "reverseString",
        _longhand = "x => x.split('').reverse().join('')"
    }

instance Numeric JS where
    num n = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => " <> BSL.pack (show n),
        _longhand = "_x => " <> BSL.pack (show n)
    }
    negate' = JS $ Code {
        _externalImports = [],
        _internalImports = [(
            "control/category/numeric", [
                Function {
                    _name = "negate",
                    _typeFrom = "number",
                    _typeTo = "number",
                    _fnShorthand = "x => -x",
                    _fnLonghand = "x => -x"
                }
            ]
        )],
        _shorthand = "negate",
        _longhand = "x => -x"
    }
    add = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _name = "add",
                    _typeFrom = "[number, number]",
                    _typeTo = "number",
                    _fnShorthand = "([x, y]) => x + y",
                    _fnLonghand = "([x, y]) => x + y"
                }
                ]
            )
            ],
        _shorthand = "add",
        _longhand = "([x, y]) => x + y"
    }
    mult = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _name = "mult",
                    _typeFrom = "[number, number]",
                    _typeTo = "number",
                    _fnShorthand = "([x, y]) => x * y",
                    _fnLonghand = "([x, y]) => x * y"
                }
                ]
            )
            ],
        _shorthand = "mult",
        _longhand = "([x, y]) => x * y"
    }
    div' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _name = "div",
                    _typeFrom = "[number, number]",
                    _typeTo = "number",
                    _fnShorthand = "([x, y]) => Math.floor(x / y)",
                    _fnLonghand = "([x, y]) => Math.floor(x / y)"
                }
                ]
            )
            ],
        _shorthand = "div",
        _longhand = "([x, y]) => Math.floor(x / y)"
    }
    mod' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _name = "mod",
                    _typeFrom = "[number, number]",
                    _typeTo = "number",
                    _fnShorthand = "([x, y]) => x % y",
                    _fnLonghand = "([x, y]) => x % y"
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

-- instance ExecuteJSLonghand JS where
--     executeJSLonghand cat param = do
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
-- instance ExecuteJSShorthand JS where
--     executeJSShorthand cat param = do
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
-- instance ExecuteJSImports JS where
--     executeJSImports cat param = do
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
-- This means we need to deal with both within JS sessions. Let's try to use Pure/Monadic... or maybe JSPure / JSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand JS where
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

instance ExecuteStdioImports JS where
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

instance ExecuteStdioShorthand JS where
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

instance ExecuteJSONLonghand JS where
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

instance ExecuteJSONShorthand JS where
    executeJSONShorthand :: (ToJSON input, FromJSON output, MonadIO m) ⇒ JS input output → input → m output
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

instance ExecuteJSONImports JS where
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
