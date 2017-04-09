{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Eval where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception.Safe
import Data.Foldable
import Data.Functor
import Data.Int
import qualified GHC
import Language.Haskell.GHC.Session
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Mem
import Unsafe.Coerce

data EvalPref = EvalPref
    { timeLimit :: Int
    , allocLimit :: Int64
    }

defEvalPref :: EvalPref
defEvalPref = EvalPref {timeLimit = maxBound, allocLimit = maxBound}

data Eval a = Eval
    { src, expr :: String
    }

eval
    :: NFData a
    => SessionPref -> EvalPref -> Eval a -> IO (Either SomeException a)
eval spref EvalPref {..} Eval {..} = withAsync eval_act with_eval_async
  where
    eval_act
        :: NFData a
        => IO a
    eval_act = do
        setAllocationCounter allocLimit
        enableAllocationLimit
        withSystemTempFile "TEMP.hs" with_temp_hs
    with_temp_hs
        :: NFData a
        => FilePath -> Handle -> IO a
    with_temp_hs hs_path hs_handle = do
        sequenceA_
            [ hPutStr hs_handle s
            | s <- ["module ", takeBaseName hs_path, " where\n", src]
            ]
        hClose hs_handle
        finally (with_temp_hs' hs_path) (clean_temp_hs hs_path)
    with_temp_hs'
        :: NFData a
        => FilePath -> IO a
    with_temp_hs' hs_path =
        runSessionT
            spref
            { dynFlags =
                  \dflags ->
                      dflags
                      { GHC.importPaths =
                            takeDirectory hs_path : GHC.importPaths dflags
                      }
            }
            (session_act hs_path)
    session_act
        :: NFData a
        => FilePath -> SessionT IO a
    session_act hs_path = do
        GHC.setTargets
            [GHC.Target (GHC.TargetFile hs_path Nothing) True Nothing]
        void $ GHC.load GHC.LoadAllTargets
        GHC.setContext
            [ GHC.IIDecl $
              GHC.simpleImportDecl $ GHC.mkModuleName $ takeBaseName hs_path
            ]
        v <- unsafeCoerce <$> GHC.compileExpr expr
        v `deepseq` pure v
    clean_temp_hs :: FilePath -> IO ()
    clean_temp_hs hs_path =
        void $
        tryAny $ do
            removeFile (hs_path -<.> "hi")
            removeFile (hs_path -<.> "o")
    with_eval_async
        :: NFData a
        => Async a -> IO (Either SomeException a)
    with_eval_async eval_async =
        withAsync (timer_act eval_async) (with_eval_timer_async eval_async)
    timer_act
        :: NFData a
        => Async a -> IO ()
    timer_act eval_async = do
        threadDelay timeLimit
        eval_status <- poll eval_async
        case eval_status of
            Nothing -> uninterruptibleCancel eval_async
            _ -> pure ()
    with_eval_timer_async
        :: NFData a
        => Async a -> Async () -> IO (Either SomeException a)
    with_eval_timer_async eval_async timer_async = do
        eval_result <- waitCatch eval_async
        uninterruptibleCancel timer_async
        pure eval_result
