{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.Haskell (HS(..)) where

-- import Control.Arrow
import Control.Category
import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Compile.Imports
import Control.Category.Compile.Longhand
import Control.Category.Compile.Shorthand
import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Shorthand
-- import Control.Category.Execute.JSON.Imports
-- import Control.Category.Execute.JSON.Longhand
-- import Control.Category.Execute.JSON.Shorthand
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
import Control.Exception                          hiding (bracket)
import Control.Lens hiding (Choice)
import Control.Monad.IO.Class
-- import Data.Aeson
import Data.ByteString.Lazy.Char8                 qualified as BSL
import Data.Code.Generic
import Data.Foldable
-- import Data.Map                                         (Map)
import Data.Map                                   qualified as M
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
import Data.Set                                   qualified as S
-- import Data.String
import Data.Text.Encoding                         qualified as TE
-- import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                                    hiding (id, (.))
import System.Process
import Text.Read
import Control.Arrow
-- import Data.Typeable
-- import Debug.Trace

newtype HS a b = HS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (HS a b) k1 a k2 b where
    code = coerced

toExternalCLIImports ∷ HS a b → [String]
toExternalCLIImports hs = GHC.IsList.toList (view externalImports hs) >>=
    \(moduleName', _) -> [{-}"-e", ":l", BSL.unpack moduleName,-} "-e", BSL.unpack $ "import " <> moduleName' {-<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")\""-}]

toInternalCLIImports ∷ HS a b → [String]
toInternalCLIImports hs = GHC.IsList.toList (view internalImports hs) >>=
    \(moduleName', _) -> ["-e", BSL.unpack $ "import " <> moduleName']

toShorthandCLIDefinitions ∷ HS a b → [String]
toShorthandCLIDefinitions hs = GHC.IsList.toList (view internalImports hs) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        "-e", BSL.unpack $
            view functionName function' <> " :: " <> view functionTypeFrom  function' <> " -> " <> view functionTypeTo  function' <> "; " <>
            view functionName function' <> " = " <> view functionLonghand function'
        ]

toInternalFileImports ∷ HS a b → [BSL.ByteString]
toInternalFileImports hs = (
    \(moduleName, functions) ->
        "import " <> moduleName <> " (" <> BSL.intercalate ", " (view functionName <$> S.toList functions) <> ")"
    ) <$> M.toList (getMapSet (view internalImports hs))

toShorthandFileDefinitions ∷ HS a b → [BSL.ByteString]
toShorthandFileDefinitions hs = foldMap' (\(_, functions) ->
    foldMap' (\fn ->
        [view functionName fn <> " :: " <> view functionTypeFrom  fn <> " -> " <> view functionTypeTo  fn <> "\n" <>
            view functionName fn <> " = " <> view functionLonghand fn <> "\n"]
    )
    functions
    ) $ M.toList (getMapSet (view internalImports hs))

toExternalFileImports ∷ HS a b → [BSL.ByteString]
toExternalFileImports hs = (
    \(moduleName, functions) ->
        "import " <> moduleName <> " (" <> BSL.intercalate ", " (S.toList functions) <> ")"
    ) <$> M.toList (getMapSet (view externalImports hs))

instance RenderStatementLonghand (HS a b) where
    renderStatementLonghand = view longhand

instance RenderStatementShorthand (HS a b) where
    renderStatementShorthand = view shorthand

moduleNameToFilename ∷ BSL.ByteString → FilePath
moduleNameToFilename = BSL.unpack . (<> ".hs") . BSL.map (\c -> if c == '.' then '/' else c)

instance RenderLibraryInternalShorthand (HS a b) where
    renderLibraryInternalShorthand hs = GHC.IsList.toList (view internalImports hs) >>=
        \(module'', functions) -> [(
            moduleNameToFilename module'',
            "module " <> module'' <> " (" <> BSL.intercalate ", " (view functionName <$> S.toList functions) <> ") where\n" <>
            "\n" <> BSL.unlines (toExternalFileImports hs) <>
            BSL.unlines (
                (\function' ->
                    "\n" <> view functionName function' <> " :: " <> view functionTypeFrom  function' <> " -> " <> view functionTypeTo  function' <>
                    "\n" <> view functionName function' <> " = " <> view functionShorthand function') <$> GHC.IsList.toList functions)
        )]

instance RenderLibraryInternalLonghand (HS a b) where
    renderLibraryInternalLonghand hs = GHC.IsList.toList (view internalImports hs) >>=
        \(module'', functions) -> [(
            moduleNameToFilename module'',
            "module " <> module'' <> " (" <> BSL.intercalate ", " (view functionName <$> S.toList functions) <> ") where\n" <>
            "\n" <> BSL.unlines (toExternalFileImports hs) <>
            BSL.unlines (
                (\function' ->
                    "\n" <> view functionName function' <> " :: " <> view functionTypeFrom function' <> " -> " <> view functionTypeTo function' <>
                    "\n" <> view functionName function' <> " = " <> view functionLonghand function') <$> GHC.IsList.toList functions)
        )]

-- TODO do we really need this?
-- This is just dependencies of a library
-- We also need the actual library
instance RenderLibraryInternalImports (HS a b) where
    renderLibraryInternalImports hs = GHC.IsList.toList (view internalImports hs) >>=
        \(module'', functions) -> [(
            moduleNameToFilename module'',
            "module " <> module'' <> " (" <> BSL.intercalate ", " (view functionName <$> S.toList functions) <> ") where\n" <>
            "\n" <> BSL.unlines (toExternalFileImports hs) <>
            BSL.unlines (
                (\function' ->
                    "\n" <> view functionName function' <> " :: " <> view functionTypeFrom function' <> " -> " <> view functionTypeTo function' <>
                    "\n" <> view functionName function' <> " = " <> view functionShorthand function') <$> GHC.IsList.toList functions)
        )]


-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderLibraryExternalShorthand (HS a b) where
    renderLibraryExternalShorthand newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        "\nmodule " <> newModule <> " (" <> newFunctionName <> ") where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> newFunctionName <> " :: " <> newFunctionTypeFrom <> " -> " <> newFunctionTypeTo <>
        "\n" <> newFunctionName <> " = " <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderLibraryExternalLonghand (HS a b) where
    renderLibraryExternalLonghand newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        "\nmodule " <> newModule <> " (" <> newFunctionName <> ") where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        "\n" <> newFunctionName <> " :: " <> newFunctionTypeFrom <> " -> " <> newFunctionTypeTo <>
        "\n" <> newFunctionName <> " = " <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderLibraryExternalImports (HS a b) where
    renderLibraryExternalImports newModule newFunctionName newFunctionTypeFrom newFunctionTypeTo cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ") where\n\n" <>
        "\nmodule " <> newModule <> " (" <> newFunctionName <> ") where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toInternalFileImports cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> -- <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> newFunctionName <> " :: " <> newFunctionTypeFrom <> " -> " <> newFunctionTypeTo <>
        "\n" <> newFunctionName <> " = " <> renderStatementShorthand cat


-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (HS () ()) where
    renderProgramShorthand cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        "\nmodule Main (main) where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\nmain :: IO ()" <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view functionName cat <> " = " <> renderStatementShorthand cat
        "\nmain = runKleisli " <> renderStatementShorthand cat <> " ()"

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (HS () ()) where
    renderProgramLonghand cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        "\nmodule Main (main) where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\nmain :: IO ()" <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view functionName cat <> " = " <> renderStatementLonghand cat
        "\nmain = runKleisli " <> renderStatementLonghand cat <> " ()"

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderProgramImports (HS () ()) where
    renderProgramImports cat =
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ") where\n\n" <>
        "\nmodule Main (main) where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toInternalFileImports cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> -- <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\nmain :: IO ()" <> -- <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> view functionName cat <> " = " <> renderStatementShorthand cat
        "\nmain = runKleisli " <> renderStatementShorthand cat <> " ()"

instance Bracket HS where
    bracket f = HS $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "(" <> renderStatementShorthand f <> ")",
        _longhand = "(" <> renderStatementLonghand f <> ")"
    }

instance Category HS where
    id = HS $ Code {
        _externalImports = [
            ("Control.Category", ["id"])
        ],
        _internalImports = [],
        _shorthand = "id",
        _longhand = "\\x -> x"
    }
    a . b = HS $ Code {
        _externalImports = view externalImports a <> view externalImports b <> [
            ("Control.Category", ["(.)"])
        ],
        _internalImports = view internalImports a <> view internalImports b,
        _shorthand = "(" <> view shorthand a <> " . " <> view shorthand b <> ")",
        _longhand = "(\\x y z -> x (y z))(" <> view longhand a <> ")(" <> view longhand b <> ")"
    }

-- this will only work whenever there is no kleisli inside.
-- Maybe we need to split on purely pure HS and monadic.
instance Apply HS where
    app = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["app"])
        ],
        _internalImports = [],
        _shorthand = "app",
        _longhand = "\\(f, x) -> f x"
    }

-- ???
instance Arrow HS where
    arr _ = error "Arbitrary functions cannot be injected into HS. Use Archery functions instead."
    first f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor", ["first"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "first (" <> view shorthand f <> ")",
        _longhand = "\\(a, b) -> ((" <> view longhand f <> ") a, b)"
    }
    second f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor", ["second"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "second (" <> view shorthand f <> ")",
        _longhand = "\\(a, b) -> (a, (" <> view longhand f <> ") b)"
    }
    f *** g = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor", ["bimap"])
        ] <> view externalImports f <> view externalImports g,
        _internalImports = view internalImports f <> view internalImports g,
        _shorthand = "bimap (" <> view shorthand f <> ") (" <> view shorthand g <> ")",
        _longhand = "\\(a, b) -> ((" <> view longhand f <> ") a, (" <> view longhand g <> ")b)"
    }
 -- f &&& g = dunno yet does it matter

instance Cartesian HS where
    copy = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cartesian", [
                Function {
                    _functionName = "copy",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "(a, a)",
                    _functionShorthand = "copy",
                    _functionLonghand = "\\x -> (x, x)"
                }
                ]
            )
        ],
        _shorthand = "copy",
        _longhand = "\\x -> (x, x)"
    }
    consume = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cartesian", [
                Function {
                    _functionName = "consume",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "()",
                    _functionShorthand = "consume",
                    _functionLonghand = "\\x -> ()"
                }
                ]
            )
        ],
        _shorthand = "consume",
        _longhand = "\\x -> ()"
    }
    fst' = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cartesian", [
                Function {
                    _functionName = "fst'",
                    _functionTypeFrom = "(a, b)",
                    _functionTypeTo = "a",
                    _functionShorthand = "fst",
                    _functionLonghand = "\\(a, b) -> a"
                }
                ]
            )
        ],
        _shorthand = "fst",
        _longhand = "\\(a, b) -> a"
    }
    snd' = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cartesian", [
                Function {
                    _functionName = "snd'",
                    _functionTypeFrom = "(a, b)",
                    _functionTypeTo = "b",
                    _functionShorthand = "snd",
                    _functionLonghand = "\\(a, b) -> b"
                }
                ]
            )
        ],
        _shorthand = "snd'",
        _longhand = "\\(a, b) -> b"
    }

instance Cocartesian HS where
    injectL = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cocartesian", [
                Function {
                    _functionName = "injectL",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "Either a b",
                    _functionShorthand = "injectL",
                    _functionLonghand = "\\a -> Left a"
                }
                ]
            )
        ],
        _shorthand = "injectL",
        _longhand = "\\a -> Left a"
    }
    injectR = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cocartesian", [
                Function {
                    _functionName = "injectR",
                    _functionTypeFrom = "b",
                    _functionTypeTo = "Either a b",
                    _functionShorthand = "injectR",
                    _functionLonghand = "\\b -> Right b"
                }
                ]
            )
        ],
        _shorthand = "injectR",
        _longhand = "\\b -> Right b"
    }
    unify = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cocartesian", [
                Function {
                    _functionName = "unify",
                    _functionTypeFrom = "Either a a",
                    _functionTypeTo = "a",
                    _functionShorthand = "unify",
                    _functionLonghand = "\\case { Left a -> a; Right a -> a; }"
                }
                ]
            )
        ],
        _shorthand = "unify",
        _longhand = "\\case { Left a -> a; Right a -> a; }"
    }
    tag = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cocartesian", [
                Function {
                    _functionName = "tag",
                    _functionTypeFrom = "(Bool, a)",
                    _functionTypeTo = "Either a a",
                    _functionShorthand = "tag",
                    _functionLonghand = "\\case { (False, a) -> Left a; (True, a) -> Right a; }"
                }
                ]
            )
        ],
        _shorthand = "tag",
        _longhand = "\\case { (False, a) -> Left a; (True, a) -> Right a; }"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: HS String String
-- HS {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "a", _functionTypeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_functionName = "fst", _functionTypeFrom = "(a, b)", _functionTypeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_functionName = "(.)", _functionTypeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _functionTypeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: HS String String)

instance Strong HS where
    first' f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor qualified as Bifunctor", ["Bifunctor.first"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "Bifunctor.first (" <> view shorthand f <> ")",
        _longhand = "\\(a, b) -> ((" <> view longhand f <> ") a, b)"
    }
    second' f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor qualified as Bifunctor", ["Bifunctor.second"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "Bifunctor.second (" <> view shorthand f <> ")",
        _longhand = "\\(a, b) -> (a, (" <> view longhand f <> ") b)"
    }

instance Choice HS where
    left' f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor qualified as Bifunctor", ["Bifunctor.first"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "Bifunctor.first (" <> view shorthand f <> ")",
        _longhand = "\\case { Left x -> Left ((" <> view longhand f <> ") x); Right x -> Right x; }"
    }
    right' f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor qualified as Bifunctor", ["Bifunctor.second"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "Bifunctor.second (" <> view shorthand f <> ")",
        _longhand = "\\case { Left x -> Left x; Right x -> Right ((" <> view longhand f <> ") x); }"
    }

-- todo define more functions
instance ArrowChoice HS where
    left f = HS $ Code {
        _externalImports = [
            ("Data.Bifunctor qualified as Bifunctor", ["Bifunctor.first"])
        ] <> view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "Bifunctor.first (" <> view shorthand f <> ")",
        _longhand = "\\case { Left x -> Left ((" <> view longhand f <> ") x); Right x -> Right x; }"
    }

instance Symmetric HS where
    swap = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Symmetric", [
                Function {
                    _functionName = "swap",
                    _functionTypeFrom = "(a, b)",
                    _functionTypeTo = "(b, a)",
                    _functionShorthand = "\\(a, b) -> (b, a)",
                    _functionLonghand = "\\(a, b) -> (b, a)"
                }
                ]
            )
            ],
        _shorthand = "swap",
        _longhand = "\\(a, b) -> (b, a)"
    }
    swapEither = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Symmetric", [
                Function {
                    _functionName = "swapEither",
                    _functionTypeFrom = "Either a a",
                    _functionTypeTo = "Either a a",
                    _functionShorthand = "\\case { Left a -> Right a; Right a -> Left a; }",
                    _functionLonghand = "\\case { Left a -> Right a; Right a -> Left a; }"
                }
                ]
            )
        ],
        _shorthand = "swapEither",
        _longhand = "\\case { Left a -> Right a; Right a -> Left a; }"
    }
    reassoc = HS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Symmetric",
                [
                    Function {
                        _functionName = "reassoc",
                        _functionTypeFrom = "(a, (b, c))",
                        _functionTypeTo = "((a, b), c)",
                        _functionShorthand = "\\(a, (b, c)) -> ((a, b), c)",
                        _functionLonghand = "\\(a, (b, c)) -> ((a, b), c)"
                    }
                    ]
            )
            ],
        _shorthand = "reassoc",
        _longhand = "\\(a, (b, c)) -> ((a, b), c)"
    }
    reassocEither = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Symmetric", [
                Function {
                    _functionName = "reassocEither",
                    _functionTypeFrom = "Either a (Either b c)",
                    _functionTypeTo = "Either (Either a b) c",
                    _functionShorthand = "\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }",
                    _functionLonghand = "\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }"
                }
                ]
            )
            ],
        _shorthand = "reassocEither",
        _longhand = "\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }"
    }

-- instance Cochoice HS where

-- instance Costrong HS where

-- instance Apply HS where

instance PrimitiveBool HS where
    eq = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["arr"]),
            ("Control.Category", ["(.)"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.Bool", [
                Function {
                    _functionName = "eq",
                    _functionTypeFrom = "Eq a => (a, a)",
                    _functionTypeTo = "Bool",
                    _functionShorthand = "(arr . uncurry $ (==))",
                    _functionLonghand = "(arr . uncurry $ (==))"
                }
            ])
        ],
        _shorthand = "eq",
        _longhand = "(arr . uncurry $ (==))"
    }

instance PrimitiveConsole HS where
    outputString = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.Bool", [
                Function {
                    _functionName = "outputString",
                    _functionTypeFrom = "String",
                    _functionTypeTo = "IO ()",
                    _functionShorthand = "Kleisli putStr",
                    _functionLonghand = "Kleisli putStr"
                }
            ])
        ],
        _shorthand = "outputString",
        _longhand = "Kleisli putStr"
    }
    inputString = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.Console", [
                Function {
                    _functionName = "inputString",
                    _functionTypeFrom = "()",
                    _functionTypeTo = "IO String",
                    _functionShorthand = "Kleisli (const getContents)",
                    _functionLonghand = "Kleisli (const getContents)"
                }
            ])
        ],
        _shorthand = "inputString",
        _longhand = "Kleisli (const getContents)"
    }

instance PrimitiveExtra HS where
    intToString = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Primitive.Extra", [
                Function {
                    _functionName = "intToString",
                    _functionTypeFrom = "Int",
                    _functionTypeTo = "String",
                    _functionShorthand = "show",
                    _functionLonghand = "show"
                }
                ]
            )
            ],
        _shorthand = "intToString",
        _longhand = "show"
    }
    concatString = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Primitive.Extra", [
                Function {
                    _functionName = "concatString",
                    _functionTypeFrom = "(String, String)",
                    _functionTypeTo = "String",
                    _functionShorthand = "uncurry (<>)",
                    _functionLonghand = "uncurry (<>)"
                }
                ]
            )
            ],
        _shorthand = "concatString",
        _longhand = "uncurry (<>)"
    }
    constString s = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "const \"" <> BSL.fromStrict (TE.encodeUtf8 s) <> "\"",
        _longhand = "const \"" <> BSL.fromStrict (TE.encodeUtf8 s) <> "\""
    }

instance PrimitiveFile HS where
    readFile' = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"]),
            ("Control.Category", ["(.)"]),
            ("Control.Monad.IO.Class", ["liftIO"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.File", [
                Function {
                    _functionName = "readFile'",
                    _functionTypeFrom = "String",
                    _functionTypeTo = "IO String",
                    _functionShorthand = "(Kleisli $ liftIO . readFile)",
                    _functionLonghand = "(Kleisli $ liftIO . readFile)"
                }
                ]
            )
            ],
        _shorthand = "readFile'",
        _longhand = "(Kleisli $ liftIO . readFile)"
    }
    writeFile' = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"]),
            ("Control.Category", ["(.)"]),
            ("Control.Monad.IO.Class", ["liftIO"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.File", [
                Function {
                    _functionName = "writeFile'",
                    _functionTypeFrom = "(String, String)",
                    _functionTypeTo = "IO ()",
                    _functionShorthand = "(Kleisli $ liftIO . uncurry writeFile)",
                    _functionLonghand = "(Kleisli $ liftIO . uncurry writeFile)"
                }
                ]
            )
            ],
        _shorthand = "writeFile'",
        _longhand = "(Kleisli $ liftIO . uncurry writeFile)"
    }

instance PrimitiveString HS where
    reverseString = HS $ Code {
        _externalImports = [
            ("Control.Arrow", ["arr"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.String", [
                Function {
                    _functionName = "reverseString",
                    _functionTypeFrom = "String",
                    _functionTypeTo = "String",
                    _functionShorthand = "arr reverse",
                    _functionLonghand = "arr reverse"
                }
                ]
            )
            ],
        _shorthand = "reverseString",
        _longhand = "arr reverse"
    }

instance Numeric HS where
    num n = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "const " <> BSL.pack (show n),
        _longhand = "const " <> BSL.pack (show n)
    }
    negate' = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "negate",
        _longhand = "negate"
    }
    add = HS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Numeric",  [
                Function {
                    _functionName = "add",
                    _functionTypeFrom = "(Int, Int)",
                    _functionTypeTo = "Int",
                    _functionShorthand = "uncurry (+)",
                    _functionLonghand = "uncurry (+)"
                }
                ]
            )
            ],
        _shorthand = "add",
        _longhand = "uncurry (+)"
    }
    mult = HS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Numeric",
                [
                    Function {
                        _functionName = "mult",
                        _functionTypeFrom = "(Int, Int)",
                        _functionTypeTo = "Int",
                        _functionShorthand = "uncurry (*)",
                        _functionLonghand = "uncurry (*)"
                    }
                    ]
            )
            ],
        _shorthand = "mult",
        _longhand = "uncurry (*)"
    }
    div' = HS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Numeric",
                [
                    Function {
                        _functionName = "div'",
                        _functionTypeFrom = "(Int, Int)",
                        _functionTypeTo = "Int",
                        _functionShorthand = "uncurry div",
                        _functionLonghand = "uncurry div"
                    }
                    ]
            )
            ],
        _shorthand = "div'",
        _longhand = "uncurry div"
    }
    mod' = HS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Numeric",
                [
                    Function {
                        _functionName = "mod'",
                        _functionTypeFrom = "(Int, Int)",
                        _functionTypeTo = "Int",
                        _functionShorthand = "uncurry mod",
                        _functionLonghand = "uncurry mod"
                    }
                    ]
            )
            ],
        _shorthand = "mod'",
        _longhand = "uncurry mod"
    }


-- OTHER LIBss
-- instance 


{-}
-- I don't quite know how to call ghci or cabal new-repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
-}

instance ExecuteHaskellLonghand HS where
    executeGHCiLonghand cat param = do
        let params ∷ [String]
            params = [
                -- "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementLonghand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteHaskellShorthand HS where
    executeGHCiShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                toShorthandCLIDefinitions cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteHaskellImports HS where
    executeGHCiImports cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                toInternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within Haskell sessions. Let's try to use Pure/Monadic... or maybe HSPure / HSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand HS where
    executeStdioLonghand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "runKleisli (" <> BSL.unpack (renderStatementLonghand cat) <> ") ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioImports HS where
    executeStdioImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)",
                -- ] <>
                -- toCLIImports cat <>
                -- [
                "-e", "runKleisli (" <> BSL.unpack (renderStatementShorthand cat) <> ") ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioShorthand HS where
    executeStdioShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "runKleisli (" <> BSL.unpack (renderStatementShorthand cat) <> ") ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

-- we don't know what this should be encoding to necessarily...
-- we know it's at least a free object but not sure about its properties
-- we first need to sort out decoding.

-- instance ExecuteJSONLonghand HS where
--     executeJSONLonghand cat param = do
--         let params ∷ [String]
--             params = [
--                 "-v0", "exec", "--", "ghci",
--                 -- "-e", ":set -ilibrary",
--                 "-e", ":set -XGHC2024",
--                 "-e", "import Prelude hiding ((.), id)",
--                 "-e", "import Data.Aeson",
--                 "-e", "import Data.ByteString.Lazy.Char8 as BSL"
--                 ] <>
--                 toExternalCLIImports cat <>
--                 [
--                 "-e", "encode ((" <> BSL.unpack (renderStatementLonghand cat) <> ") (decode(BSL.pack(" <> show (BSL.unpack (encode param)) <> "))))"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "cabal" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))
-- 
-- instance ExecuteJSONShorthand HS where
--     executeJSONShorthand cat param = do
--         let params ∷ [String]
--             params = [
--                 "-v0", "exec", "--", "ghci",
--                 "-e", ":set -ilibrary",
--                 "-e", ":set -XGHC2024",
--                 "-e", "import Prelude hiding ((.), id)",
--                 "-e", "import Data.Aeson",
--                 "-e", "import Data.ByteString.Lazy.Char8 as BSL"
--                 ] <>
--                 toExternalCLIImports cat <>
--                 toShorthandCLIDefinitions cat <>
--                 [
--                 "-e", "encode ((" <> BSL.unpack (renderStatementShorthand cat) <> ") (decode(BSL.pack(" <> show (BSL.unpack (encode param)) <> "))))"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "cabal" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))
-- 
-- instance ExecuteJSONImports HS where
--     executeJSONImports cat param = do
--         let params ∷ [String]
--             params = [
--                 "-v0", "exec", "--", "ghci",
--                 -- "-e", ":set -ilibrary",
--                 "-e", ":set -XGHC2024",
--                 "-e", "import Prelude hiding ((.), id)",
--                 "-e", "import Data.Aeson",
--                 "-e", "import Data.ByteString.Lazy.Char8 as BSL"
--                 ] <>
--                 toExternalCLIImports cat <>
--                 toInternalCLIImports cat <>
--                 [
--                 "-e", "encode ((" <> BSL.unpack (renderStatementShorthand cat) <> ") (decode(BSL.pack(" <> show (BSL.unpack (encode param)) <> "))))"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "cabal" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance CompileLonghand HS where
    compileLonghand file cat = do
        let params ∷ [String]
            params = [
                -- "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "runKleisli (" <> BSL.unpack (renderStatementLonghand cat) <> ") ()",
                "-o", file
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghc" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> do
                liftIO $ putStrLn stdout
                liftIO $ putStrLn stderr

instance CompileImports HS where
    compileImports file cat = do
        let params ∷ [String]
            params = [
                -- "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)",
                -- ] <>
                -- toCLIImports cat <>
                -- [
                "-e", "runKleisli (" <> BSL.unpack (renderStatementShorthand cat) <> ") ()",
                "-o", file
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghc" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> do
                liftIO $ putStrLn stdout
                liftIO $ putStrLn stderr

instance CompileShorthand HS where
    compileShorthand file cat = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "runKleisli (" <> BSL.unpack (renderStatementShorthand cat) <> ") ()",
                "-o", file
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghc" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> do
                liftIO $ putStrLn stdout
                liftIO $ putStrLn stderr
