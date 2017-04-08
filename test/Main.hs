module Main where

import qualified GHC
import Language.Haskell.GHC.Session
import Test.Tasty
import Test.Tasty.HUnit

testLoad :: SessionPref -> String -> IO Bool
testLoad pref s =
    runSessionT pref $ do
        target <- GHC.guessTarget s Nothing
        GHC.setTargets [target]
        sflag <- GHC.load GHC.LoadAllTargets
        pure $ GHC.succeeded sflag

main :: IO ()
main =
    defaultMain $
    testGroup
        "load"
        [ testCase "Fact.hs" $
          assert $ testLoad defSessionPref "test/case/Fact.hs"
        ]
