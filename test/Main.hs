module Main where

import qualified GHC
import Language.Haskell.GHC.Session
import Test.Tasty
import Test.Tasty.HUnit
import Unsafe.Coerce

testLoad :: SessionPref -> String -> IO Bool
testLoad pref s =
    runSessionT pref $ do
        target <- GHC.guessTarget s Nothing
        GHC.setTargets [target]
        sflag <- GHC.load GHC.LoadAllTargets
        pure $ GHC.succeeded sflag

testEval :: SessionPref -> String -> IO a
testEval pref expr =
    runSessionT
        pref
        { dynFlags =
              dynFlags pref .
              (\dflags ->
                   dflags
                   { GHC.hscTarget = GHC.HscInterpreted
                   , GHC.ghcLink = GHC.LinkInMemory
                   })
        } $ do
        GHC.setContext
            [GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"]
        v <- GHC.compileExpr expr
        pure $ unsafeCoerce v

loadTest :: TestTree
loadTest =
    testGroup
        "load"
        [ testCase "Fact.hs" $
          assert $ testLoad defSessionPref "test/case/Fact.hs"
        ]

evalTest :: TestTree
evalTest =
    testGroup
        "eval"
        [ testCase "Int Literal" $
          assert $ (== (233 :: Int)) <$> testEval defSessionPref "233 :: Int"
        ]

main :: IO ()
main = defaultMain $ testGroup "SessionT" [loadTest, evalTest]
