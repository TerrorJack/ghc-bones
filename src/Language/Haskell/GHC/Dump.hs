module Language.Haskell.GHC.Dump where

import qualified DriverPipeline as GHC
import qualified GHC
import qualified Hooks as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.SessionT

dumpCore :: SessionPref
         -> [String]
         -> (GHC.CgGuts -> GHC.ModSummary -> IO ())
         -> IO Bool
dumpCore pref targets dumper =
    runSessionT
        pref
        { dynFlags =
              dynFlags pref .
              (\dflags ->
                   dflags
                   { GHC.hooks =
                         (GHC.hooks dflags)
                         {GHC.runPhaseHook = Just run_phase_hook}
                   })
        } $ do
        traverse (`GHC.guessTarget` Nothing) targets >>= GHC.setTargets
        GHC.succeeded <$> GHC.load GHC.LoadAllTargets
  where
    run_phase_hook
        :: GHC.PhasePlus
        -> FilePath
        -> GHC.DynFlags
        -> GHC.CompPipeline (GHC.PhasePlus, FilePath)
    run_phase_hook phase_plus input_fn dflags =
        GHC.P $ \pipe_env pipe_state -> do
            r <-
                GHC.unP
                    (GHC.runPhase phase_plus input_fn dflags)
                    pipe_env
                    pipe_state
            case phase_plus of
                GHC.HscOut _ _ (GHC.HscRecomp cg mod_summary) ->
                    dumper cg mod_summary
                _ -> pure ()
            pure r
