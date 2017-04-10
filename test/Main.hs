{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Either
import Data.Foldable
import Data.Functor
import qualified DriverPipeline as GHC
import qualified GHC
import qualified Hooks as GHC
import Language.Haskell.GHC.Eval
import Language.Haskell.GHC.SessionT
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Unsafe.Coerce

testLoad :: SessionPref -> [FilePath] -> IO Bool
testLoad pref srcs =
    runSessionT pref' $ do
        GHC.setTargets
            [ GHC.Target (GHC.TargetFile src' Nothing) True Nothing
            | src' <- srcs
            ]
        sflag <- GHC.load GHC.LoadAllTargets
        pure $ GHC.succeeded sflag
  where
    pref' =
        pref
        { dynFlags =
              \dflags ->
                  dflags
                  { GHC.hooks =
                        GHC.emptyHooks {GHC.runPhaseHook = Just run_phase_hook}
                  }
        }
    run_phase_hook
        :: GHC.PhasePlus
        -> FilePath
        -> GHC.DynFlags
        -> GHC.CompPipeline (GHC.PhasePlus, FilePath)
    run_phase_hook phase_plus input_fn dflags =
        GHC.P $ \pipe_env pipe_state -> do
            r@(_, (phase_plus', _)) <-
                GHC.unP
                    (GHC.runPhase phase_plus input_fn dflags)
                    pipe_env
                    pipe_state
            case phase_plus of
                GHC.RealPhase phase -> putStr $ "RealPhase " ++ show phase
                GHC.HscOut hsc_src _ _ -> putStr $ "HscOut " ++ show hsc_src
            case phase_plus' of
                GHC.RealPhase phase -> putStrLn $ ", RealPhase " ++ show phase
                GHC.HscOut hsc_src _ _ -> putStrLn $ ", HscOut " ++ show hsc_src
            pure r

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

cleanupTest :: TestTree
cleanupTest =
    testCase "cleanup" $
    assert $ do
        files <- listDirectory "./test/case"
        sequenceA_
            [ removeFile $ "./test/case/" ++ file
            | file <- files
            , takeExtension file `elem` [".hi", ".o"]
            ]

main :: IO ()
main =
    defaultMain $
    testGroup "SessionT" [loadTest, evalTest, safeEvalTest, cleanupTest]
