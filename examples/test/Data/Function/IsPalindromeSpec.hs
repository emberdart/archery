{-# LANGUAGE TemplateHaskell #-}

module Data.Function.IsPalindromeSpec (spec) where

import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Shorthand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Shorthand
import Control.Monad.IO.Class
import Data.Aeson
import Data.Code.Haskell
import Data.Code.JS
import Data.Code.PHP
import Data.Code.TS
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Prims
import Data.Text                                  (Text)
import Data.Text                                  qualified as T
import Data.Text.Arbitrary ()
import Test.Hspec                                 hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Char

{- HLINT ignore "Use camelCase" -}

prop_HSGHCiIsCorrectLonghand ∷ Text → Property
prop_HSGHCiIsCorrectLonghand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeGHCiLonghand (isPalindrome :: HS Text Bool) t
    pure $ answer === isPalindrome t

xprop_HSGHCiIsCorrectImports ∷ Text → Property
xprop_HSGHCiIsCorrectImports t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeGHCiImports (isPalindrome :: HS Text Bool) t
    pure $ answer === isPalindrome t

prop_HSGHCiIsCorrectShorthand ∷ Text → Property
prop_HSGHCiIsCorrectShorthand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeGHCiShorthand (isPalindrome :: HS Text Bool) t
    pure $ answer === isPalindrome t

-- xprop_HSJSONIsCorrectLonghand ∷ Text → Property
-- xprop_HSJSONIsCorrectLonghand t = T.length t > 1 && T.all (\c -> notElem c "$" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
--     answer <- executeJSONLonghand (isPalindrome :: HS Text Bool) t
--     pure $ answer === isPalindrome t
-- 
-- xprop_HSJSONIsCorrectImports ∷ Text → Property
-- xprop_HSJSONIsCorrectImports t = T.length t > 1 && T.all (\c -> notElem c "$" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
--     answer <- executeJSONImports (isPalindrome :: HS Text Bool) t
--     pure $ answer === isPalindrome t
-- 
-- xprop_HSJSONIsCorrectShorthand ∷ Text → Property
-- xprop_HSJSONIsCorrectShorthand t = T.length t > 1 && T.all (\c -> notElem c "$" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
--     answer <- executeJSONShorthand (isPalindrome :: HS Text Bool) t
--     pure $ answer === isPalindrome t

-- TODO bad control characters
prop_JSIsCorrectLonghand ∷ Text → Property
prop_JSIsCorrectLonghand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: JS Text Bool) t
    pure $ answer === isPalindrome t

xprop_JSIsCorrectImports ∷ Text → Property
xprop_JSIsCorrectImports t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: JS Text Bool) t
    pure $ answer === isPalindrome t

prop_JSIsCorrectShorthand ∷ Text → Property
prop_JSIsCorrectShorthand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: JS Text Bool) t
    pure $ answer === isPalindrome t

prop_TSIsCorrectLonghand ∷ Text → Property
prop_TSIsCorrectLonghand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: TS Text Bool) t
    pure $ answer === isPalindrome t

xprop_TSIsCorrectImports ∷ Text → Property
xprop_TSIsCorrectImports t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: TS Text Bool) t
    pure $ answer === isPalindrome t

prop_TSIsCorrectShorthand ∷ Text → Property
prop_TSIsCorrectShorthand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: TS Text Bool) t
    pure $ answer === isPalindrome t


prop_PHPIsCorrectLonghand ∷ Text → Property
prop_PHPIsCorrectLonghand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: PHP Text Bool) t
    pure $ answer === isPalindrome t

xprop_PHPIsCorrectImports ∷ Text → Property
xprop_PHPIsCorrectImports t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: PHP Text Bool) t
    pure $ answer === isPalindrome t

prop_PHPIsCorrectShorthand ∷ Text → Property
prop_PHPIsCorrectShorthand t = T.length t > 1 && T.all (\c -> notElem c "$\\" && isPrint c && isAscii c) t ==> withNumTests 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: PHP Text Bool) t
    pure $ answer === isPalindrome t

{-}

myInterpret = _

prosp_ViaJSONIsCorrect :: Text -> Property
prosp_ViaJSONIsCorrect s = length s > 1 && T.all (\c -> notElem c "$" && isPrint c && isAscii c) s ==> withNumTests 50 $
    (myInterpret <$> decode (encode (isPalindrome :: FreeFunc p Text Bool)) <*> Just s) === Just (isPalindrome s)
-}

    {-}
    describe "JSON" $ do
        it "is correct" $
            decode (encode (isPalindrome :: FreeFunc p Text Bool)) `shouldBe` Just (isPalindrome :: FreeFunc p Text Bool)
    -}

pure []
runTests = $quickCheckAll

spec ∷ Spec
spec = parallel . xprop "IsPalindrome" . monadicIO . liftIO $ runTests
