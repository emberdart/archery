{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.PHP (PHP(..)) where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
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
import Control.Exception                          hiding (bracket)
import Control.Lens hiding (Choice)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8         qualified as BSL
import Data.Code.Generic
import Data.Foldable
-- import Data.Map                                         (Map)
import Data.Map                           qualified as M
import Data.MapSet
-- import Data.Maybe
import Data.Render.Program.Imports
import Data.Render.Program.Longhand
import Data.Render.Program.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Set                                         (Set)
-- import Data.Set                                   qualified as S
-- import Data.String
import Data.Text                          qualified as T
import Data.Text.Encoding                 qualified as TE
-- import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                            hiding (id, (.))
import System.Process
import Text.Read
import Control.Arrow

-- TODO declare(strict_types=1);

newtype PHP a b = PHP {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode (PHP a b) k1 a k2 b where
    code = coerced

-- moduleNameToFilename ∷ BSL.ByteString → FilePath
-- moduleNameToFilename = BSL.unpack . (<> ".php") . BSL.map (\c -> if c == '\\' then '/' else c)

toExternalCLIImports ∷ PHP a b → [String]
toExternalCLIImports php = GHC.IsList.toList (view externalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_functionName' -> [{-BSL.unpack $ "use function " <> moduleName <> "\\" <> functionName' <> ";" -}]

toInternalCLIImports ∷ PHP a b → [String]
toInternalCLIImports php = GHC.IsList.toList (view externalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_functionName' -> [{-BSL.unpack $ "use function " <> moduleName <> "\\" <> functionName' <> ";" -}]

toShorthandCLIDefinitions ∷ PHP a b → [String]
toShorthandCLIDefinitions php = GHC.IsList.toList (view internalImports php) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        BSL.unpack $
            -- Why not both?
            "$" <> view functionName function' <> " = " <> view functionLonghand function' <> ";"
            -- "function " <> view functionName function' <> "($param) { return (" <> view functionLonghand function' <> ")($param); } " -- spacey
        ]

toInternalFileImports ∷ PHP a b → [BSL.ByteString]
toInternalFileImports php = GHC.IsList.toList (view internalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_function' -> [{-}"use function " <> moduleName <> "\\" <> view functionName function' <> ";" -}]

toShorthandFileDefinitions ∷ PHP a b → [BSL.ByteString]
toShorthandFileDefinitions php = foldMap' (\(_, functions) ->
    foldMap' (\fn ->
        [
            -- again, why not both?
            "$" <> view functionName fn <> " = " <> view functionLonghand fn <> ";\n" -- <>
            -- "function " <> view functionName fn <> "($param) { return (" <> view functionLonghand fn <> ")($param); }\n" -- spacey
        ]
    )
    functions
    ) $ M.toList (getMapSet (view internalImports php))

toExternalFileImports ∷ PHP a b → [BSL.ByteString]
toExternalFileImports php = GHC.IsList.toList (view externalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_functionName' -> [{-}"use function " <> moduleName <> "\\" <> functionName' <> ";"-}]

instance RenderStatementLonghand (PHP a b) where
    renderStatementLonghand = view longhand

instance RenderStatementShorthand (PHP a b) where
    renderStatementShorthand = view shorthand

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (PHP a b) where
    renderProgramShorthand cat =
        "<?php\n" <>
        "define(strict_types=1);\n\n" <>
        -- "\nmodule " <> module' cat <> " (" <> view functionName cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> renderStatementShorthand cat <> ";"

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (PHP a b) where
    renderProgramLonghand cat =
        "<?php\n" <>
        "define(strict_types=1);\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view functionName cat <> " :: " <> view functionTypeFrom  cat <> " -> " <> view functionTypeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> renderStatementLonghand cat <> ";"

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderProgramImports (PHP a b) where
    renderProgramImports cat =
        "<?php\n" <>
        "define(strict_types=1);\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toInternalFileImports cat) <>
        "\n" <> renderStatementShorthand cat <> ";"

instance Bracket PHP where
    bracket f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "(" <> renderStatementShorthand f <> ")",
        _longhand = "(" <> renderStatementLonghand f <> ")"
    }

instance Category PHP where
    id = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control\\Category", [
                    Function {
                        _functionName = "id",
                        _functionTypeFrom = "",
                        _functionTypeTo = "",
                        _functionShorthand = "fn($a) => $a",
                        _functionLonghand = "fn($a) => $a"
                    }
                ]
            )
        ],
        _shorthand = "$id",
        _longhand = "fn($a) => $a"
    }
    a . b = PHP $ Code {
        _externalImports = view externalImports a <> view externalImports b,
        _internalImports = view internalImports a <> view internalImports b <> [
            ("Control\\Category", [
                Function {
                    _functionName = "compose",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($f) => fn($g) => fn($x) => $f($g($x))",
                    _functionLonghand = "fn($f) => fn($g) => fn($x) => $f($g($x))"
                }
            ])
        ],
        _shorthand = "$compose(" <> view shorthand a <> ")(" <> view shorthand b <> ")",
        _longhand = "(fn ($f) => fn ($g) => fn($x) => $f($g($x)))(" <> view longhand a <> ")(" <> view longhand b <> ")"
    }

instance Cartesian PHP where
    copy = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "copy",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => ([$x, $x])",
                    _functionLonghand = "fn ($x) => ([$x, $x])"
                }
                ]
            )
        ],
        _shorthand = "$copy",
        _longhand = "fn ($x) => ([$x, $x])"
    }
    consume = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "consume",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => null",
                    _functionLonghand = "fn ($x) => null"
                }
                ]
            )
        ],
        _shorthand = "$consume",
        _longhand = "fn ($x) => null"
    }
    fst' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "fst",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => $x[0]",
                    _functionLonghand = "fn ($x) => $x[0]"
                }
                ]
            )
        ],
        _shorthand = "$fst",
        _longhand = "fn ($x) => $x[0]"
    }
    snd' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control/Category/Cartesian.php", [
                Function {
                    _functionName = "snd",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => $x[1]",
                    _functionLonghand = "fn ($x) => $x[1]"
                }
                ]
            )
        ],
        _shorthand = "$snd",
        _longhand = "fn ($x) => $x[1]"
    }

instance Cocartesian PHP where
    injectL = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "injectL",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => [\"Left\" => $x]",
                    _functionLonghand = "fn($x) => [\"Left\" => $x]"
                }
                ]
            )
        ],
        _shorthand = "$injectL",
        _longhand = "fn($x) => [\"Left\" => $x]"
    }
    injectR = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "injectR",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => [\"Right\" => $x]",
                    _functionLonghand = "fn($x) => [\"Right\" => $x]"
                }
                ]
            )
        ],
        _shorthand = "$injectR",
        _longhand = "fn($x) => [\"Right\" => $x]"
    }
    unify = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "unify",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]",
                    _functionLonghand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]"
                }
                ]
            )
        ],
        _shorthand = "$unify",
        _longhand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]"
    }
    tag = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "tag",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]",
                    _functionLonghand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]"
                }
                ]
            )
        ],
        _shorthand = "$tag",
        _longhand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: PHP String String
-- PHP {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control\\Category",fromList [Function {_functionName = "compose", _functionTypeFrom = "", _functionTypeTo = "", _functionShorthand = "fn($f) => fn($g) => fn($x) => $f($g($x))", _functionLonghand = "fn($f) => fn($g) => fn($x) => $f($g($x))"}]),("Control\\Category\\Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "", _functionTypeTo = "", _functionShorthand = "fn ($x) => ([$x, $x])", _functionLonghand = "fn ($x) => ([$x, $x])"},Function {_functionName = "fst", _functionTypeFrom = "", _functionTypeTo = "", _functionShorthand = "fn ($x) => $x[0]", _functionLonghand = "fn ($x) => $x[0]"}])]}, _shorthand = "$compose($fst)($copy)", _longhand = "(fn ($f) => fn ($g) => fn($x) => $f($g($x)))(fn ($x) => $x[0])(fn ($x) => ([$x, $x]))"}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: PHP String String)
-- "(fn ($f) => fn ($g) => fn($x) => $f($g($x)))(fn ($x) => $x[0])(fn ($x) => ([$x, $x]))"

instance Strong PHP where
    first' f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn ($x) => [(" <> view shorthand f <> ")($x[0]), $x[1]]",
        _longhand = "fn ($x) => [(" <> view longhand f <> ")($x[0]), $x[1]]"
    }
    second' f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn ($x) => [$x[0], (" <> view shorthand f <> ")($x[1])]",
        _longhand = "fn ($x) => [$x[0], (" <> view longhand f <> ")($x[1])]"
    }

instance Arrow PHP where
    arr = error "Arbitrary functions cannot be injected into PHP. Use Archery functions instead."
    first f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn ($x) => [(" <> view shorthand f <> ")($x[0]), $x[1]]",
        _longhand = "fn ($x) => [(" <> view longhand f <> ")($x[0]), $x[1]]"
    }
    second f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn ($x) => [$x[0], (" <> view shorthand f <> ")($x[1])]",
        _longhand = "fn ($x) => [$x[0], (" <> view longhand f <> ")($x[1])]"
    }

instance Choice PHP where
    left' f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn($x) => isset($x[\"Left\"]) ? [ \"Left\" => (" <> view shorthand f <> ")($x[\"Left\"]) ] : $x",
        _longhand = "fn($x) => isset($x[\"Left\"]) ? [ \"Left\" => (" <> view longhand f <> ")($x[\"Left\"]) ] : $x"
    }
    right' f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn($x) => isset($x[\"Right\"]) ? [ \"Right\" => (" <> view shorthand f <> ")($x[\"Right\"]) ] : $x",
        _longhand = "fn($x) => isset($x[\"Right\"]) ? [ \"Right\" => (" <> view longhand f <> ")($x[\"Right\"]) ] : $x"
    }

instance ArrowChoice PHP where
    left f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn($x) => isset($x[\"Left\"]) ? [ \"Left\" => (" <> view shorthand f <> ")($x[\"Left\"]) ] : $x",
        _longhand = "fn($x) => isset($x[\"Left\"]) ? [ \"Left\" => (" <> view longhand f <> ")($x[\"Left\"]) ] : $x"
    }
    right f = PHP $ Code {
        _externalImports = view externalImports f,
        _internalImports = view internalImports f,
        _shorthand = "fn($x) => isset($x[\"Right\"]) ? [ \"Right\" => (" <> view shorthand f <> ")($x[\"Right\"]) ] : $x",
        _longhand = "fn($x) => isset($x[\"Right\"]) ? [ \"Right\" => (" <> view longhand f <> ")($x[\"Right\"]) ] : $x"
    }

instance Symmetric PHP where
    swap = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Symmetric", [
                Function {
                    _functionName = "swap",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => [$x[1], $x[0]]",
                    _functionLonghand = "fn ($x) => [$x[1], $x[0]]"
                }
                ]
            )
            ],
        _shorthand = "$swap",
        _longhand = "fn ($x) => [$x[1], $x[0]]"
    }
    swapEither = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Symmetric", [
                Function {
                    _functionName = "swapEither",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]",
                    _functionLonghand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]"
                }
                ]
            )
        ],
        _shorthand = "$swapEither",
        _longhand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]"
    }
    reassoc = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control\\Category\\Symmetric",
                [
                    Function {
                        _functionName = "reassoc",
                        _functionTypeFrom = "",
                        _functionTypeTo = "",
                        _functionShorthand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]",
                        _functionLonghand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]"
                    }
                    ]
            )
            ],
        _shorthand = "$reassoc",
        _longhand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]"
    }
    reassocEither = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Symmetric", [
                Function {
                    _functionName = "reassocEither",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    -- \\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }
                    _functionShorthand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }",
                    _functionLonghand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }"
                }
                ]
            )
            ],
        _shorthand = "$reassocEither",
        _longhand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }"
    }

-- instance Cochoice PHP where

-- instance Costrong PHP where

-- instance Apply PHP where

instance PrimitiveBool PHP where
    eq = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\Bool", [
                Function {
                    _functionName = "eq",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => $x[0] == $x[1]",
                    _functionLonghand = "fn ($x) => $x[0] == $x[1]"
                }
            ])
        ],
        _shorthand = "$eq",
        _longhand = "fn ($x) => $x[0] === $x[1]"
    }


instance PrimitiveConsole PHP where
    outputString = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\Console", [
                Function {
                    _functionName = "outputString",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "void",
                    _functionShorthand = "echo",
                    _functionLonghand = "fn($x) => echo $x"
                }
            ])
        ],
        _shorthand = "$outputString",
        _longhand = "fn($x) => echo $x"
    }
    inputString = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\Console", [
                Function {
                    _functionName = "inputString",
                    _functionTypeFrom = "void",
                    _functionTypeTo = "string",
                    _functionShorthand = "fn($x) => { throw new Exception(\"TODO how do you get it?\"); }",
                    _functionLonghand = "fn($x) => { throw new Exception(\"TODO how do you get it?\"); }"
                }
            ])
        ],
        _shorthand = "$inputString",
        _longhand = "fn($x) => { throw new Exception(\"TODO how do you get it?\"); }"
    }


instance PrimitiveExtra PHP where
    intToString = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\Extra", [
                Function {
                    _functionName = "intToString",
                    _functionTypeFrom = "int",
                    _functionTypeTo = "string",
                    _functionShorthand = "(string)",
                    _functionLonghand = "fn($x) => (string)$x"
                }
                ]
            )
            ],
        _shorthand = "$intToString",
        _longhand = "fn($x) => (string)$x"
    }
    concatString = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\Extra", [
                Function {
                    _functionName = "concatString",
                    _functionTypeFrom = "[string, string]",
                    _functionTypeTo = "string",
                    _functionShorthand = "fn($x) => $x[0] . $x[1]",
                    _functionLonghand = "fn($x) => $x[0] . $x[1]"
                }
                ]
            )
            ],
        _shorthand = "$concatString",
        _longhand = "fn($x) => $x[0] . $x[1]"
    }
    constString t = PHP $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "fn($x) => \"" <> BSL.fromStrict (TE.encodeUtf8 t) <> "\"",
        _longhand = "fn($x) => \"" <> BSL.fromStrict (TE.encodeUtf8 t) <> "\""
    }

instance PrimitiveFile PHP where
    readFile' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\File", [
                Function {
                    _functionName = "readFile'",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "string",
                    _functionShorthand = "file_get_contents",
                    _functionLonghand = "fn($fn) => file_get_contents($fn)"
                }
                ]
            )
            ],
        _shorthand = "$readFile'",
        _longhand = "fn($x) => file_get_contents($x)"
    }
    writeFile' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\File", [
                Function {
                    _functionName = "writeFile'",
                    _functionTypeFrom = "[string, string]",
                    _functionTypeTo = "void",
                    _functionShorthand = "fn($x) => file_put_contents($x[0], $x[1])",
                    _functionLonghand = "fn($x) => file_put_contents($x[0], $x[1])"
                }
                ]
            )
            ],
        _shorthand = "$writeFile'",
        _longhand = "fn($x) => file_put_contents($x[0], $x[1])"
    }

instance PrimitiveString PHP where
    reverseString = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\String", [
                Function {
                    _functionName = "reverseString",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "string",
                    _functionShorthand = "strrev",
                    _functionLonghand = "fn($x) => strrev($x)"
                }
                ]
            )
            ],
        _shorthand = "$reverseString",
        _longhand = "fn($x) => strrev($x)"
    }

instance Numeric PHP where
    num n = PHP $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "fn($x) => " <> BSL.fromStrict (TE.encodeUtf8 (T.show n)),
        _longhand = "fn($x) => " <> BSL.fromStrict (TE.encodeUtf8 (T.show n))
    }
    negate' = PHP $ Code {
        _externalImports = [],
        _internalImports = [(
            "Control\\Category\\Numeric", [
                Function {
                    _functionName = "negate",
                    _functionTypeFrom = "int|float|double",
                    _functionTypeTo = "int|float|double",
                    _functionShorthand = "fn($x) => -$x",
                    _functionLonghand = "fn($x) => -$x"
                }
            ]
        )],
        _shorthand = "$negate",
        _longhand = "fn($x) => -$x"
    }
    add = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Numeric",  [
                Function {
                    _functionName = "add",
                    _functionTypeFrom = "[int|float|double, int|float|double]",
                    _functionTypeTo = "int|float|double",
                    _functionShorthand = "fn($x) => $x[0] + $x[1]",
                    _functionLonghand = "fn($x) => $x[0] + $x[1]"
                }
                ]
            )
            ],
        _shorthand = "$add",
        _longhand = "fn($x) => $x[0] + $x[1]"
    }
    mult = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Numeric",  [
                Function {
                    _functionName = "mult",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "fn($x) => $x[0] * $x[1]",
                    _functionLonghand = "fn($x) => $x[0] * $x[1]"
                }
                ]
            )
            ],
        _shorthand = "$mult",
        _longhand = "fn($x) => $x[0] * $x[1]"
    }
    div' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Numeric",  [
                Function {
                    _functionName = "div",
                    _functionTypeFrom = "[int|float|double, int|float|double]",
                    _functionTypeTo = "int|float|double",
                    _functionShorthand = "fn($x) => $x[0] / $x[1]",
                    _functionLonghand = "fn($x) => $x[0] / $x[1]"
                }
                ]
            )
            ],
        _shorthand = "$div",
        _longhand = "fn($x) => $x[0] / $x[1]"
    }
    mod' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Numeric",  [
                Function {
                    _functionName = "mod",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "fn($x) => $x[0] % $x[1]",
                    _functionLonghand = "fn($x) => $x[0] % $x[1]"
                }
                ]
            )
            ],
        _shorthand = "$mod",
        _longhand = "fn($x) => $x[0] % $x[1]"
    }


-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within PHP sessions. Let's try to use Pure/Monadic... or maybe PHPPure / PHPMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand PHP where
    executeStdioLonghand cat stdin = do
        let params ∷ [String]
            params = [
                "-r",
                unwords (toExternalCLIImports cat) <>
                    "(" <> BSL.unpack (renderStatementLonghand cat) <> ")(null);"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioImports PHP where
    executeStdioImports cat stdin = do
        let params ∷ [String]
            params = [
                "-r",
                -- toCLIImports cat <>
                -- [
                    "(" <> BSL.unpack (renderStatementShorthand cat) <> ")(null);"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioShorthand PHP where
    executeStdioShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-r",
                unwords (toExternalCLIImports cat) <>
                    "(" <> BSL.unpack (renderStatementShorthand cat) <> ")(null);"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteJSONLonghand PHP where
    executeJSONLonghand cat param = do
        let params ∷ [String]
            params = [
                "-r",
                unwords (toExternalCLIImports cat) <>
                    "echo(json_encode((" <> BSL.unpack (renderStatementLonghand cat) <> ")(json_decode(" <> show (BSL.unpack (encode param)) <> ", true))));"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONShorthand PHP where
    executeJSONShorthand :: (ToJSON input, FromJSON output, MonadIO m) => PHP input output -> input -> m output
    executeJSONShorthand cat param = do
        let params ∷ [String]
            params = [
                "-r",
                unwords (toExternalCLIImports cat) <>
                    unwords (toShorthandCLIDefinitions cat) <>
                    "echo(json_encode((" <> BSL.unpack (renderStatementShorthand cat) <> ")(json_decode(" <> show (BSL.unpack (encode param)) <> ", true))));"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONImports PHP where
    executeJSONImports cat param = do
        let params ∷ [String]
            params = [
                    "-r",
                    unwords (toExternalCLIImports cat) <>
                        unwords (toInternalCLIImports cat) <>
                        "echo(json_encode((" <> BSL.unpack (renderStatementShorthand cat) <> ")(json_decode(" <> show (BSL.unpack (encode param)) <> ", true))));"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

