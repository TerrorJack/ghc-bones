module Main where

import qualified GHC
import Language.Haskell.GHC.Session
import Test.Tasty
import Test.Tasty.HUnit
import Unsafe.Coerce

testLoad :: SessionPref -> FilePath -> IO Bool
testLoad pref src =
    runSessionT pref $ do
        GHC.setTargets [GHC.Target (GHC.TargetFile src Nothing) True Nothing]
        sflag <- GHC.load GHC.LoadAllTargets
        pure $ GHC.succeeded sflag

testEval :: SessionPref -> String -> IO a
testEval pref expr =
    runSessionT pref $ do
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
