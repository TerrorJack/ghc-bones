module Main where

import Data.Functor
import qualified GHC
import Language.Haskell.GHC.Session
import Test.Tasty
import Test.Tasty.HUnit

testLoad :: String -> IO Bool
testLoad s =
    runSessionT defSessionPref $ do
        dflags <- GHC.getSessionDynFlags
        void $ GHC.setSessionDynFlags dflags
        target <- GHC.guessTarget s Nothing
        GHC.setTargets [target]
        sflag <- GHC.load GHC.LoadAllTargets
        pure $ GHC.succeeded sflag

main :: IO ()
main =
    defaultMain $
    testCase "load Fact.hs" $ assert $ testLoad "test/case/Fact.hs"
