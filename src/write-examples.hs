{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe -Wwarn #-}

module Main (main) where

-- import Control.Category.Compile.Imports
-- import Control.Category.Compile.Longhand
-- import Control.Category.Compile.Shorthand
-- import Control.Category.Interpret
import Data.Aeson                           qualified as A
import Data.Bifunctor
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8           qualified as BSL
import Data.Code.Haskell
import Data.Code.JS
import Data.Code.PHP
import Data.Code.TS
import Data.Foldable
-- import Data.Function.Arrowy
import Data.Function.AskName
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.HelloWorld
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
import Data.Prims
import Data.PrimsIO

-- import Data.Render.Library.External.Imports
-- import Data.Render.Library.External.Longhand
-- import Data.Render.Library.External.Shorthand
-- import Data.Render.Library.Internal.Imports
-- import Data.Render.Library.Internal.Longhand
-- import Data.Render.Library.Internal.Shorthand
import Data.Render.Program.Imports
import Data.Render.Program.Longhand
import Data.Render.Program.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Person
import Data.Render.Library.Internal.Imports
import Data.Text                            (Text)
import Data.Yaml                            qualified as Y
import System.Directory
import System.FilePath

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True

writeToPrefix ∷ FilePath → [(FilePath, BSL.ByteString)] → IO ()
writeToPrefix prefix = traverse_ (
    (\(file, contents) -> do
        mkdirp (dropFileName file)
        BSL.writeFile file contents
    ) . first (prefix <>))

main ∷ IO ()
main = do
    removeDirectoryRecursive "data/examples"

    -- removeDirectoryRecursive "data/examples/jcat"

    do -- jcat
        do -- lib
            mkdirp "data/examples/jcat/lib"

            -- A.encodeFile "data/examples/jcat/lib/arrowy.json" (arrowy :: FreeFunc Prims Text Text)
            A.encodeFile "data/examples/jcat/lib/collatzStep.json" (collatzStep :: FreeFunc Prims Int Int)
            A.encodeFile "data/examples/jcat/lib/isPalindrome.json" (isPalindrome :: FreeFunc Prims Text Bool)
            -- A.encodeFile "data/examples/jcat/lib/greetData.json" (greetData :: FreeFunc Prims Person Text)
            A.encodeFile "data/examples/jcat/lib/greetTuple.json" (greetTuple :: FreeFunc Prims (Text, Int) Text)
        do -- src
            mkdirp "data/examples/jcat/src"

            A.encodeFile "data/examples/jcat/src/askName.json" (askName :: FreeFunc PrimsIO () ())
            A.encodeFile "data/examples/jcat/src/helloWorld.json" (helloWorld :: FreeFunc PrimsIO () ())
            A.encodeFile "data/examples/jcat/src/reverseInput.json" (revInputProgram :: FreeFunc PrimsIO () ())
    do -- ycat
        do -- lib
            mkdirp "data/examples/ycat/lib"

            -- Y.encodeFile "data/examples/ycat/lib/arrowy.yaml" (arrowy :: FreeFunc Prims Text Text)
            Y.encodeFile "data/examples/ycat/lib/collatzStep.yaml" (collatzStep :: FreeFunc Prims Int Int)
            Y.encodeFile "data/examples/ycat/lib/isPalindrome.yaml" (isPalindrome :: FreeFunc Prims Text Bool)
            -- Y.encodeFile "data/examples/ycat/lib/greetData.yaml" (greetData :: FreeFunc Prims Person Text)
            Y.encodeFile "data/examples/ycat/lib/greetTuple.yaml" (greetTuple :: FreeFunc Prims (Text, Int) Text)
        do -- src
            mkdirp "data/examples/ycat/src"

            Y.encodeFile "data/examples/ycat/src/askName.yaml" (askName :: FreeFunc PrimsIO () ())
            Y.encodeFile "data/examples/ycat/src/helloWorld.yaml" (helloWorld :: FreeFunc PrimsIO () ())
            Y.encodeFile "data/examples/ycat/src/reverseInput.yaml" (revInputProgram :: FreeFunc PrimsIO () ())
    do -- HS
        do -- St
            do -- LH
                do -- lib
                    mkdirp "data/examples/statements/longhand/haskell/lib"

                    -- BSL.writeFile "data/examples/statements/longhand/haskell/lib/Arrowy.hs" $ renderStatementLonghand (arrowy :: HS Text Text)
                    BSL.writeFile "data/examples/statements/longhand/haskell/lib/CollatzStep.hs" $ renderStatementLonghand (collatzStep :: HS Int Int)
                    BSL.writeFile "data/examples/statements/longhand/haskell/lib/IsPalindrome.hs" $ renderStatementLonghand (isPalindrome :: HS Text Bool)
                    BSL.writeFile "data/examples/statements/longhand/haskell/lib/GreetTuple.hs" $ renderStatementLonghand (greetTuple :: HS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/longhand/haskell/src"

                    BSL.writeFile "data/examples/statements/longhand/haskell/src/askName.hs" $ renderStatementLonghand (askName :: HS () ())
                    BSL.writeFile "data/examples/statements/longhand/haskell/src/helloWorld.hs" $ renderStatementLonghand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/statements/longhand/haskell/src/reverseInput.hs" $ renderStatementLonghand (revInputProgram :: HS () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/statements/shorthand/haskell/lib"

                    -- BSL.writeFile "data/examples/statements/shorthand/haskell/lib/Arrowy.hs" $ renderStatementShorthand (arrowy :: HS Text Text)
                    BSL.writeFile "data/examples/statements/shorthand/haskell/lib/CollatzStep.hs" $ renderStatementShorthand (collatzStep :: HS Int Int)
                    BSL.writeFile "data/examples/statements/shorthand/haskell/lib/IsPalindrome.hs" $ renderStatementShorthand (isPalindrome :: HS Text Bool)
                    BSL.writeFile "data/examples/statements/shorthand/haskell/lib/GreetTuple.hs" $ renderStatementShorthand (greetTuple :: HS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/shorthand/haskell/src"

                    BSL.writeFile "data/examples/statements/shorthand/haskell/src/askName.hs" $ renderStatementShorthand (askName :: HS () ())
                    BSL.writeFile "data/examples/statements/shorthand/haskell/src/helloWorld.hs" $ renderStatementShorthand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/statements/shorthand/haskell/src/reverseInput.hs" $ renderStatementShorthand (revInputProgram :: HS () ())
        do -- Pr
            do -- Im
                do -- lib
                    mkdirp "data/examples/libraries/imports/haskell"

                    -- writeToPrefix "data/examples/libraries/imports/haskell/arrowy/" $ renderLibraryInternalImports (arrowy :: HS Text Text)
                    writeToPrefix "data/examples/libraries/imports/haskell/collatzStep/" $ renderLibraryInternalImports (collatzStep :: HS Int Int)
                    writeToPrefix "data/examples/libraries/imports/haskell/isPalindrome/" $ renderLibraryInternalImports (isPalindrome :: HS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/haskell/CollatzStep.hs" $ renderLibraryImports (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/haskell/IsPalindrome.hs" $ renderLibraryImports (isPalindrome :: HS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/haskell/GreetTuple.hs" $ renderLibraryImports (greetTuple :: HS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/imports/haskell"

                    BSL.writeFile "data/examples/programs/imports/haskell/askName.hs" $ renderProgramImports (askName :: HS () ())
                    BSL.writeFile "data/examples/programs/imports/haskell/helloWorld.hs" $ renderProgramImports (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/programs/imports/haskell/reverseInput.hs" $ renderProgramImports (revInputProgram :: HS () ())
            do -- LH
                do -- lib
                    mkdirp "data/examples/libraries/longhand/haskell"

                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/Arrowy.hs" $ renderLibraryLonghand (arrowy :: HS Text Text)
                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/CollatzStep.hs" $ renderLibraryLonghand (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/IsPalindrome.hs" $ renderLibraryLonghand (isPalindrome :: HS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/GreetTuple.hs" $ renderLibraryLonghand (greetTuple :: HS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/longhand/haskell"

                    BSL.writeFile "data/examples/programs/longhand/haskell/askName.hs" $ renderProgramLonghand (askName :: HS () ())
                    BSL.writeFile "data/examples/programs/longhand/haskell/helloWorld.hs" $ renderProgramLonghand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/programs/longhand/haskell/reverseInput.hs" $ renderProgramLonghand (revInputProgram :: HS () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/libraries/shorthand/haskell"

                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/Arrowy.hs" $ renderLibraryShorthand (arrowy :: HS Text Text)
                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/CollatzStep.hs" $ renderLibraryShorthand (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/IsPalindrome.hs" $ renderLibraryShorthand (isPalindrome :: HS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/GreetTuple.hs" $ renderLibraryShorthand (greetTuple :: HS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/shorthand/haskell"

                    BSL.writeFile "data/examples/programs/shorthand/haskell/askName.hs" $ renderProgramShorthand (askName :: HS () ())
                    BSL.writeFile "data/examples/programs/shorthand/haskell/helloWorld.hs" $ renderProgramShorthand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/programs/shorthand/haskell/reverseInput.hs" $ renderProgramShorthand (revInputProgram :: HS () ())
    do -- JS
        do -- St
            do -- LH
                do -- lib
                    mkdirp "data/examples/statements/longhand/js/lib"

                    -- BSL.writeFile "data/examples/statements/longhand/js/lib/Arrowy.js" $ renderStatementLonghand (arrowy :: JS Text Text)
                    BSL.writeFile "data/examples/statements/longhand/js/lib/CollatzStep.js" $ renderStatementLonghand (collatzStep :: JS Int Int)
                    BSL.writeFile "data/examples/statements/longhand/js/lib/IsPalindrome.js" $ renderStatementLonghand (isPalindrome :: JS Text Bool)
                    BSL.writeFile "data/examples/statements/longhand/js/lib/GreetTuple.js" $ renderStatementLonghand (greetTuple :: JS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/longhand/js/src"

                    BSL.writeFile "data/examples/statements/longhand/js/src/askName.js" $ renderStatementLonghand (askName :: JS () ())
                    BSL.writeFile "data/examples/statements/longhand/js/src/helloWorld.js" $ renderStatementLonghand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/statements/longhand/js/src/reverseInput.js" $ renderStatementLonghand (revInputProgram :: JS () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/statements/shorthand/js/lib"

                    -- BSL.writeFile "data/examples/statements/shorthand/js/lib/Arrowy.js" $ renderStatementShorthand (arrowy :: JS Text Text)
                    BSL.writeFile "data/examples/statements/shorthand/js/lib/collatzStep.js" $ renderStatementShorthand (collatzStep :: JS Int Int)
                    BSL.writeFile "data/examples/statements/shorthand/js/lib/isPalindrome.js" $ renderStatementShorthand (isPalindrome :: JS Text Bool)
                    BSL.writeFile "data/examples/statements/shorthand/js/lib/greetTuple.js" $ renderStatementShorthand (greetTuple :: JS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/shorthand/js/src"

                    BSL.writeFile "data/examples/statements/shorthand/js/src/askName.js" $ renderStatementShorthand (askName :: JS () ())
                    BSL.writeFile "data/examples/statements/shorthand/js/src/helloWorld.js" $ renderStatementShorthand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/statements/shorthand/js/src/reverseInput.js" $ renderStatementShorthand (revInputProgram :: JS () ())
        do -- Pr
            do -- Im
                do -- lib
                    mkdirp "data/examples/libraries/imports/js"
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/js/arrowy/" <>) $ renderLibraryImports (arrowy :: JS Text Text)
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/js/collatzStep/" <>) $ renderLibraryImports (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/js/Arrowy.js" $ renderLibraryImports (arrowy :: JS Text Text)
                    -- BSL.writeFile "data/examples/libraries/imports/js/CollatzStep.js" $ renderLibraryImports (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/js/IsPalindrome.js" $ renderLibraryImports (isPalindrome :: JS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/js/GreetTuple.js" $ renderLibraryImports (greetTuple :: JS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/imports/js"

                    BSL.writeFile "data/examples/programs/imports/js/askName.js" $ renderProgramImports (askName :: JS () ())
                    BSL.writeFile "data/examples/programs/imports/js/helloWorld.js" $ renderProgramImports (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/programs/imports/js/reverseInput.js" $ renderProgramImports (revInputProgram :: JS () ())
            do -- LH
                do -- lib
                    mkdirp "data/examples/libraries/longhand/js"

                    -- BSL.writeFile "data/examples/libraries/longhand/js/Arrowy.js" $ renderLibraryLonghand (arrowy :: JS Text Text)
                    -- BSL.writeFile "data/examples/libraries/longhand/js/CollatzStep.js" $ renderLibraryLonghand (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/longhand/js/IsPalindrome.js" $ renderLibraryLonghand (isPalindrome :: JS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/longhand/js/GreetTuple.js" $ renderLibraryLonghand (greetTuple :: JS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/longhand/js"

                    BSL.writeFile "data/examples/programs/longhand/js/askName.js" $ renderProgramLonghand (askName :: JS () ())
                    BSL.writeFile "data/examples/programs/longhand/js/helloWorld.js" $ renderProgramLonghand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/programs/longhand/js/reverseInput.js" $ renderProgramLonghand (revInputProgram :: JS () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/libraries/shorthand/js"

                    -- BSL.writeFile "data/examples/libraries/shorthand/js/Arrowy.js" $ renderLibraryShorthand (arrowy :: JS Text Text)
                    -- BSL.writeFile "data/examples/libraries/shorthand/js/CollatzStep.js" $ renderLibraryShorthand (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/shorthand/js/IsPalindrome.js" $ renderLibraryShorthand (isPalindrome :: JS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/shorthand/js/GreetTuple.js" $ renderLibraryShorthand (greetTuple :: JS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/shorthand/js"

                    BSL.writeFile "data/examples/programs/shorthand/js/askName.js" $ renderProgramShorthand (askName :: JS () ())
                    BSL.writeFile "data/examples/programs/shorthand/js/helloWorld.js" $ renderProgramShorthand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/programs/shorthand/js/reverseInput.js" $ renderProgramShorthand (revInputProgram :: JS () ())
    do -- PHP
        do -- St
            do -- LH
                do -- lib
                    mkdirp "data/examples/statements/longhand/php/lib"

                    -- BSL.writeFile "data/examples/statements/longhand/php/lib/Arrowy.php" $ renderStatementLonghand (arrowy :: PHP Text Text)
                    BSL.writeFile "data/examples/statements/longhand/php/lib/CollatzStep.php" $ renderStatementLonghand (collatzStep :: PHP Int Int)
                    BSL.writeFile "data/examples/statements/longhand/php/lib/IsPalindrome.php" $ renderStatementLonghand (isPalindrome :: PHP Text Bool)
                    BSL.writeFile "data/examples/statements/longhand/php/lib/GreetTuple.php" $ renderStatementLonghand (greetTuple :: PHP (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/longhand/php/src"

                    BSL.writeFile "data/examples/statements/longhand/php/src/askName.php" $ renderStatementLonghand (askName :: PHP () ())
                    BSL.writeFile "data/examples/statements/longhand/php/src/helloWorld.php" $ renderStatementLonghand (helloWorld :: PHP () ())
                    BSL.writeFile "data/examples/statements/longhand/php/src/reverseInput.php" $ renderStatementLonghand (revInputProgram :: PHP () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/statements/shorthand/php/lib"

                    -- BSL.writeFile "data/examples/statements/shorthand/php/lib/arrowy.php" $ renderStatementShorthand (arrowy :: PHP Text Text)
                    BSL.writeFile "data/examples/statements/shorthand/php/lib/collatzStep.php" $ renderStatementShorthand (collatzStep :: PHP Int Int)
                    BSL.writeFile "data/examples/statements/shorthand/php/lib/isPalindrome.php" $ renderStatementShorthand (isPalindrome :: PHP Text Bool)
                    BSL.writeFile "data/examples/statements/shorthand/php/lib/greetTuple.php" $ renderStatementShorthand (greetTuple :: PHP (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/shorthand/php/src"

                    BSL.writeFile "data/examples/statements/shorthand/php/src/askName.php" $ renderStatementShorthand (askName :: PHP () ())
                    BSL.writeFile "data/examples/statements/shorthand/php/src/helloWorld.php" $ renderStatementShorthand (helloWorld :: PHP () ())
                    BSL.writeFile "data/examples/statements/shorthand/php/src/reverseInput.php" $ renderStatementShorthand (revInputProgram :: PHP () ())
        do -- Pr
            do -- Im
                do -- lib
                    mkdirp "data/examples/libraries/imports/php"
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/php/arrowy/" <>) $ renderLibraryImports (arrowy :: PHP Text Text)
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/php/collatzStep/" <>) $ renderLibraryImports (collatzStep :: PHP Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/php/Arrow.php" $ renderLibraryImports (arrowy :: PHP Text Text)
                    -- BSL.writeFile "data/examples/libraries/imports/php/CollatzStep.php" $ renderLibraryImports (collatzStep :: PHP Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/php/IsPalindrome.php" $ renderLibraryImports (isPalindrome :: PHP Text Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/php/GreetTuple.php" $ renderLibraryImports (greetTuple :: PHP (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/imports/php"

                    BSL.writeFile "data/examples/programs/imports/php/askName.php" $ renderProgramImports (askName :: PHP () ())
                    BSL.writeFile "data/examples/programs/imports/php/helloWorld.php" $ renderProgramImports (helloWorld :: PHP () ())
                    BSL.writeFile "data/examples/programs/imports/php/reverseInput.php" $ renderProgramImports (revInputProgram :: PHP () ())
            do -- LH
                do -- lib
                    mkdirp "data/examples/libraries/longhand/php"

                    -- BSL.writeFile "data/examples/libraries/longhand/php/Arrowy.php" $ renderLibraryLonghand (arrowy :: PHP Text Text)
                    -- BSL.writeFile "data/examples/libraries/longhand/php/CollatzStep.php" $ renderLibraryLonghand (collatzStep :: PHP Int Int)
                    -- BSL.writeFile "data/examples/libraries/longhand/php/IsPalindrome.php" $ renderLibraryLonghand (isPalindrome :: PHP Text Bool)
                    -- BSL.writeFile "data/examples/libraries/longhand/php/GreetTuple.php" $ renderLibraryLonghand (greetTuple :: PHP (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/longhand/php"

                    BSL.writeFile "data/examples/programs/longhand/php/askName.php" $ renderProgramLonghand (askName :: PHP () ())
                    BSL.writeFile "data/examples/programs/longhand/php/helloWorld.php" $ renderProgramLonghand (helloWorld :: PHP () ())
                    BSL.writeFile "data/examples/programs/longhand/php/reverseInput.php" $ renderProgramLonghand (revInputProgram :: PHP () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/libraries/shorthand/php"

                    -- BSL.writeFile "data/examples/libraries/shorthand/php/Arrowy.php" $ renderLibraryShorthand (arrowy :: PHP Text Text)
                    -- BSL.writeFile "data/examples/libraries/shorthand/php/CollatzStep.php" $ renderLibraryShorthand (collatzStep :: PHP Int Int)
                    -- BSL.writeFile "data/examples/libraries/shorthand/php/IsPalindrome.php" $ renderLibraryShorthand (isPalindrome :: PHP Text Bool)
                    -- BSL.writeFile "data/examples/libraries/shorthand/php/GreetTuple.php" $ renderLibraryShorthand (greetTuple :: PHP (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/shorthand/php"

                    BSL.writeFile "data/examples/programs/shorthand/php/askName.php" $ renderProgramShorthand (askName :: PHP () ())
                    BSL.writeFile "data/examples/programs/shorthand/php/helloWorld.php" $ renderProgramShorthand (helloWorld :: PHP () ())
                    BSL.writeFile "data/examples/programs/shorthand/php/reverseInput.php" $ renderProgramShorthand (revInputProgram :: PHP () ())
    do -- TS
        do -- St
            do -- LH
                do -- lib
                    mkdirp "data/examples/statements/longhand/ts/lib"

                    -- BSL.writeFile "data/examples/statements/longhand/ts/lib/Arrowy.ts" $ renderStatementLonghand (arrowy :: TS Text Text)
                    BSL.writeFile "data/examples/statements/longhand/ts/lib/CollatzStep.ts" $ renderStatementLonghand (collatzStep :: TS Int Int)
                    BSL.writeFile "data/examples/statements/longhand/ts/lib/IsPalindrome.ts" $ renderStatementLonghand (isPalindrome :: TS Text Bool)
                    BSL.writeFile "data/examples/statements/longhand/ts/lib/GreetTuple.ts" $ renderStatementLonghand (greetTuple :: TS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/longhand/ts/src"

                    BSL.writeFile "data/examples/statements/longhand/ts/src/askName.ts" $ renderStatementLonghand (askName :: TS () ())
                    BSL.writeFile "data/examples/statements/longhand/ts/src/helloWorld.ts" $ renderStatementLonghand (helloWorld :: TS () ())
                    BSL.writeFile "data/examples/statements/longhand/ts/src/reverseInput.ts" $ renderStatementLonghand (revInputProgram :: TS () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/statements/shorthand/ts/lib"

                    -- BSL.writeFile "data/examples/statements/shorthand/ts/lib/arrowy.ts" $ renderStatementShorthand (arrowy :: TS Text Text)
                    BSL.writeFile "data/examples/statements/shorthand/ts/lib/collatzStep.ts" $ renderStatementShorthand (collatzStep :: TS Int Int)
                    BSL.writeFile "data/examples/statements/shorthand/ts/lib/isPalindrome.ts" $ renderStatementShorthand (isPalindrome :: TS Text Bool)
                    BSL.writeFile "data/examples/statements/shorthand/ts/lib/greetTuple.ts" $ renderStatementShorthand (greetTuple :: TS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/statements/shorthand/ts/src"

                    BSL.writeFile "data/examples/statements/shorthand/ts/src/askName.ts" $ renderStatementShorthand (askName :: TS () ())
                    BSL.writeFile "data/examples/statements/shorthand/ts/src/helloWorld.ts" $ renderStatementShorthand (helloWorld :: TS () ())
                    BSL.writeFile "data/examples/statements/shorthand/ts/src/reverseInput.ts" $ renderStatementShorthand (revInputProgram :: TS () ())
        do -- Pr
            do -- Im
                do -- lib
                    mkdirp "data/examples/libraries/imports/ts"      
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/ts/arrowy/" <>) $ renderLibraryImports (arrowy :: TS Text Text)
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/ts/collatzStep/" <>) $ renderLibraryImports (collatzStep :: TS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/ts/Arrowy.ts" $ renderLibraryImports (arrowy :: TS Text Text)
                    -- BSL.writeFile "data/examples/libraries/imports/ts/CollatzStep.ts" $ renderLibraryImports (collatzStep :: TS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/ts/IsPalindrome.ts" $ renderLibraryImports (isPalindrome :: TS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/ts/GreetTuple.ts" $ renderLibraryImports (greetTuple :: TS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/imports/ts"

                    BSL.writeFile "data/examples/programs/imports/ts/askName.ts" $ renderProgramImports (askName :: TS () ())
                    BSL.writeFile "data/examples/programs/imports/ts/helloWorld.ts" $ renderProgramImports (helloWorld :: TS () ())
                    BSL.writeFile "data/examples/programs/imports/ts/reverseInput.ts" $ renderProgramImports (revInputProgram :: TS () ())
            do -- LH
                do -- lib
                    mkdirp "data/examples/libraries/longhand/ts"

                    -- BSL.writeFile "data/examples/libraries/longhand/ts/Arrowy.ts" $ renderLibraryLonghand (arrowy :: TS Text Text)
                    -- BSL.writeFile "data/examples/libraries/longhand/ts/CollatzStep.ts" $ renderLibraryLonghand (collatzStep :: TS Int Int)
                    -- BSL.writeFile "data/examples/libraries/longhand/ts/IsPalindrome.ts" $ renderLibraryLonghand (isPalindrome :: TS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/longhand/ts/GreetTuple.ts" $ renderLibraryLonghand (greetTuple :: TS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/longhand/ts"

                    BSL.writeFile "data/examples/programs/longhand/ts/askName.ts" $ renderProgramLonghand (askName :: TS () ())
                    BSL.writeFile "data/examples/programs/longhand/ts/helloWorld.ts" $ renderProgramLonghand (helloWorld :: TS () ())
                    BSL.writeFile "data/examples/programs/longhand/ts/reverseInput.ts" $ renderProgramLonghand (revInputProgram :: TS () ())
            do -- SH
                do -- lib
                    mkdirp "data/examples/libraries/shorthand/ts"

                    -- BSL.writeFile "data/examples/libraries/shorthand/ts/Arrowy.ts" $ renderLibraryShorthand (arrowy :: TS Text Text)
                    -- BSL.writeFile "data/examples/libraries/shorthand/ts/CollatzStep.ts" $ renderLibraryShorthand (collatzStep :: TS Int Int)
                    -- BSL.writeFile "data/examples/libraries/shorthand/ts/IsPalindrome.ts" $ renderLibraryShorthand (isPalindrome :: TS Text Bool)
                    -- BSL.writeFile "data/examples/libraries/shorthand/ts/GreetTuple.ts" $ renderLibraryShorthand (greetTuple :: TS (Text, Int) Text)
                do -- src
                    mkdirp "data/examples/programs/shorthand/ts"

                    BSL.writeFile "data/examples/programs/shorthand/ts/askName.ts" $ renderProgramShorthand (askName :: TS () ())
                    BSL.writeFile "data/examples/programs/shorthand/ts/helloWorld.ts" $ renderProgramShorthand (helloWorld :: TS () ())
                    BSL.writeFile "data/examples/programs/shorthand/ts/reverseInput.ts" $ renderProgramShorthand (revInputProgram :: TS () ())
    -- etc
