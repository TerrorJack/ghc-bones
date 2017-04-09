{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Either
import Data.Functor
import qualified GHC
import Language.Haskell.GHC.Eval
import Language.Haskell.GHC.SessionT
import Test.Tasty
import Test.Tasty.HUnit
import Unsafe.Coerce

testLoad :: SessionPref -> [FilePath] -> IO Bool
testLoad pref srcs =
    runSessionT pref $ do
        GHC.setTargets
            [ GHC.Target (GHC.TargetFile src' Nothing) True Nothing
            | src' <- srcs
            ]
        sflag <- GHC.load GHC.LoadAllTargets
        pure $ GHC.succeeded sflag

testEval :: SessionPref
         -> [FilePath]
         -> [FilePath]
         -> [String]
         -> String
         -> IO a
testEval pref imps srcs mods expr' =
    runSessionT
        pref
        { dynFlags =
              \dflags ->
                  dflags {GHC.importPaths = imps ++ GHC.importPaths dflags}
        } $ do
        GHC.setTargets
            [ GHC.Target (GHC.TargetFile src' Nothing) True Nothing
            | src' <- srcs
            ]
        void $ GHC.load GHC.LoadAllTargets
        GHC.setContext
            [GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName m | m <- mods]
        v <- GHC.compileExpr expr'
        pure $ unsafeCoerce v

loadTest :: TestTree
loadTest =
    testGroup
        "load"
        [ testCase "Fact.hs" $
          assert $
          testLoad
              defSessionPref
              ["./test/case/Fact.hs", "./test/case/UnsafeFact.hs"]
        ]

evalTest :: TestTree
evalTest =
    testGroup
        "eval"
        [ testCase "Int Literal" $
          assert $
          (== (120 :: Int)) <$>
          testEval
              defSessionPref
              ["./test/case"]
              ["./test/case/Fact.hs"]
              ["Fact"]
              "fact 5"
        ]

safeEvalTest :: TestTree
safeEvalTest =
    testGroup
        "safe eval"
        [ testCase "timeout" $
          assert $
          isLeft <$>
          eval
              defSessionPref
              defEvalPref {timeLimit = 1000000}
              (Eval "" "let x = x in x" :: Eval ())
        , testCase "Int expr" $
          assert $
          (\case
               Right 2 -> True
               _ -> False) <$>
          eval
              defSessionPref
              defEvalPref
              (Eval "x :: Int\nx = 1 + 1\n" "x" :: Eval Int)
        ]

main :: IO ()
main = defaultMain $ testGroup "SessionT" [loadTest, evalTest, safeEvalTest]
