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
        \_name' -> [{-BSL.unpack $ "use function " <> moduleName <> "\\" <> name' <> ";" -}]

toInternalCLIImports ∷ PHP a b → [String]
toInternalCLIImports php = GHC.IsList.toList (view externalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_name' -> [{-BSL.unpack $ "use function " <> moduleName <> "\\" <> name' <> ";" -}]

toShorthandCLIDefinitions ∷ PHP a b → [String]
toShorthandCLIDefinitions php = GHC.IsList.toList (view internalImports php) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        BSL.unpack $
            -- Why not both?
            "$" <> view name function' <> " = " <> view fnLonghand function' <> ";"
            -- "function " <> view name function' <> "($param) { return (" <> view fnLonghand function' <> ")($param); } " -- spacey
        ]

toInternalFileImports ∷ PHP a b → [BSL.ByteString]
toInternalFileImports php = GHC.IsList.toList (view internalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_function' -> [{-}"use function " <> moduleName <> "\\" <> view name function' <> ";" -}]

toShorthandFileDefinitions ∷ PHP a b → [BSL.ByteString]
toShorthandFileDefinitions php = foldMap' (\(_, functions) ->
    foldMap' (\fn ->
        [
            -- again, why not both?
            "$" <> view name fn <> " = " <> view fnLonghand fn <> ";\n" -- <>
            -- "function " <> view name fn <> "($param) { return (" <> view fnLonghand fn <> ")($param); }\n" -- spacey
        ]
    )
    functions
    ) $ M.toList (getMapSet (view internalImports php))

toExternalFileImports ∷ PHP a b → [BSL.ByteString]
toExternalFileImports php = GHC.IsList.toList (view externalImports php) >>=
    \(_moduleName, functions) -> GHC.IsList.toList functions >>=
        \_name' -> [{-}"use function " <> moduleName <> "\\" <> name' <> ";"-}]

instance RenderStatementLonghand (PHP a b) where
    renderStatementLonghand = view longhand

instance RenderStatementShorthand (PHP a b) where
    renderStatementShorthand = view shorthand

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (PHP a b) where
    renderProgramShorthand cat =
        "<?php\n" <>
        "define(strict_types=1);\n\n" <>
        -- "\nmodule " <> module' cat <> " (" <> view name cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> view name cat <> " :: " <> view typeFrom  cat <> " -> " <> view typeTo  cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> renderStatementShorthand cat <> ";"

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (PHP a b) where
    renderProgramLonghand cat =
        "<?php\n" <>
        "define(strict_types=1);\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> view name cat <> " :: " <> view typeFrom  cat <> " -> " <> view typeTo  cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
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
                        _name = "id",
                        _typeFrom = "",
                        _typeTo = "",
                        _fnShorthand = "fn($a) => $a",
                        _fnLonghand = "fn($a) => $a"
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
                    _name = "compose",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn($f) => fn($g) => fn($x) => $f($g($x))",
                    _fnLonghand = "fn($f) => fn($g) => fn($x) => $f($g($x))"
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
                    _name = "copy",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => ([$x, $x])",
                    _fnLonghand = "fn ($x) => ([$x, $x])"
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
                    _name = "consume",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => null",
                    _fnLonghand = "fn ($x) => null"
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
                    _name = "fst",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => $x[0]",
                    _fnLonghand = "fn ($x) => $x[0]"
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
                    _name = "snd",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => $x[1]",
                    _fnLonghand = "fn ($x) => $x[1]"
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
                    _name = "injectL",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn($x) => [\"Left\" => $x]",
                    _fnLonghand = "fn($x) => [\"Left\" => $x]"
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
                    _name = "injectR",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn($x) => [\"Right\" => $x]",
                    _fnLonghand = "fn($x) => [\"Right\" => $x]"
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
                    _name = "unify",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]",
                    _fnLonghand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]"
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
                    _name = "tag",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]",
                    _fnLonghand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]"
                }
                ]
            )
        ],
        _shorthand = "$tag",
        _longhand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: PHP String String
-- PHP {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control\\Category",fromList [Function {_name = "compose", _typeFrom = "", _typeTo = "", _shorthand = "fn($f) => fn($g) => fn($x) => $f($g($x))", _longhand = "fn($f) => fn($g) => fn($x) => $f($g($x))"}]),("Control\\Category\\Cartesian",fromList [Function {_name = "copy", _typeFrom = "", _typeTo = "", _shorthand = "fn ($x) => ([$x, $x])", _longhand = "fn ($x) => ([$x, $x])"},Function {_name = "fst", _typeFrom = "", _typeTo = "", _shorthand = "fn ($x) => $x[0]", _longhand = "fn ($x) => $x[0]"}])]}, _shorthand = "$compose($fst)($copy)", _longhand = "(fn ($f) => fn ($g) => fn($x) => $f($g($x)))(fn ($x) => $x[0])(fn ($x) => ([$x, $x]))"}}

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
                    _name = "swap",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => [$x[1], $x[0]]",
                    _fnLonghand = "fn ($x) => [$x[1], $x[0]]"
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
                    _name = "swapEither",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]",
                    _fnLonghand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]"
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
                        _name = "reassoc",
                        _typeFrom = "",
                        _typeTo = "",
                        _fnShorthand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]",
                        _fnLonghand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]"
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
                    _name = "reassocEither",
                    _typeFrom = "",
                    _typeTo = "",
                    -- \\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }
                    _fnShorthand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }",
                    _fnLonghand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }"
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
                    _name = "eq",
                    _typeFrom = "",
                    _typeTo = "",
                    _fnShorthand = "fn ($x) => $x[0] == $x[1]",
                    _fnLonghand = "fn ($x) => $x[0] == $x[1]"
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
                    _name = "outputString",
                    _typeFrom = "string",
                    _typeTo = "void",
                    _fnShorthand = "echo",
                    _fnLonghand = "fn($x) => echo $x"
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
                    _name = "inputString",
                    _typeFrom = "void",
                    _typeTo = "string",
                    _fnShorthand = "fn($x) => { throw new Exception(\"TODO how do you get it?\"); }",
                    _fnLonghand = "fn($x) => { throw new Exception(\"TODO how do you get it?\"); }"
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
                    _name = "intToString",
                    _typeFrom = "int",
                    _typeTo = "string",
                    _fnShorthand = "(string)",
                    _fnLonghand = "fn($x) => (string)$x"
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
                    _name = "concatString",
                    _typeFrom = "[string, string]",
                    _typeTo = "string",
                    _fnShorthand = "fn($x) => $x[0] . $x[1]",
                    _fnLonghand = "fn($x) => $x[0] . $x[1]"
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
                    _name = "readFile'",
                    _typeFrom = "string",
                    _typeTo = "string",
                    _fnShorthand = "file_get_contents",
                    _fnLonghand = "fn($fn) => file_get_contents($fn)"
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
                    _name = "writeFile'",
                    _typeFrom = "[string, string]",
                    _typeTo = "void",
                    _fnShorthand = "fn($x) => file_put_contents($x[0], $x[1])",
                    _fnLonghand = "fn($x) => file_put_contents($x[0], $x[1])"
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
                    _name = "reverseString",
                    _typeFrom = "string",
                    _typeTo = "string",
                    _fnShorthand = "strrev",
                    _fnLonghand = "fn($x) => strrev($x)"
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
                    _name = "negate",
                    _typeFrom = "int|float|double",
                    _typeTo = "int|float|double",
                    _fnShorthand = "fn($x) => -$x",
                    _fnLonghand = "fn($x) => -$x"
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
                    _name = "add",
                    _typeFrom = "[int|float|double, int|float|double]",
                    _typeTo = "int|float|double",
                    _fnShorthand = "fn($x) => $x[0] + $x[1]",
                    _fnLonghand = "fn($x) => $x[0] + $x[1]"
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
                    _name = "mult",
                    _typeFrom = "[number, number]",
                    _typeTo = "number",
                    _fnShorthand = "fn($x) => $x[0] * $x[1]",
                    _fnLonghand = "fn($x) => $x[0] * $x[1]"
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
                    _name = "div",
                    _typeFrom = "[int|float|double, int|float|double]",
                    _typeTo = "int|float|double",
                    _fnShorthand = "fn($x) => $x[0] / $x[1]",
                    _fnLonghand = "fn($x) => $x[0] / $x[1]"
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
                    _name = "mod",
                    _typeFrom = "[number, number]",
                    _typeTo = "number",
                    _fnShorthand = "fn($x) => $x[0] % $x[1]",
                    _fnLonghand = "fn($x) => $x[0] % $x[1]"
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

