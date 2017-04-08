{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.GHC.Session where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import qualified DynFlags as GHC
import qualified Exception as GHC
import qualified GHC
import qualified GHC.Paths as GHC
import qualified HscTypes as GHC
import qualified Panic as GHC

type Session = GHC.HscEnv

newtype SessionT m a = SessionT
    { unSessionT :: StateT Session m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadFix
               , MonadFail
               , Alternative
               , MonadPlus
               , MonadCont
               , MonadWriter w
               , MonadReader r
               , MonadError e
               , MonadTrans
               , MonadState Session
               , MonadIO
               , MonadBase b
               , MonadThrow
               , MonadCatch
               , MonadMask
               )

instance MonadTransControl SessionT where
    type StT SessionT a = StT (StateT Session) a
    liftWith = defaultLiftWith SessionT unSessionT
    restoreT = defaultRestoreT SessionT

instance MonadBaseControl b m =>
         MonadBaseControl b (SessionT m) where
    type StM (SessionT m) a = StM (StateT Session m) a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadIO m, MonadMask m) =>
         GHC.ExceptionMonad (SessionT m) where
    gcatch = catch
    gmask = mask
    gbracket = bracket
    gfinally = finally

instance Monad m =>
         GHC.HasDynFlags (SessionT m) where
    getDynFlags = fmap GHC.hsc_dflags get

instance (MonadIO m, MonadMask m) =>
         GHC.GhcMonad (SessionT m) where
    getSession = get
    setSession = put

data SessionPref = SessionPref
    { fatalMsg :: GHC.FatalMessager
    , flushOut :: GHC.FlushOut
    , libDir :: Maybe FilePath
    , dynFlags :: GHC.DynFlags -> GHC.DynFlags
    }

defSessionPref :: SessionPref
defSessionPref =
    SessionPref
    { fatalMsg = GHC.defaultFatalMessager
    , flushOut = GHC.defaultFlushOut
    , libDir = Just GHC.libdir
    , dynFlags =
          \dflags ->
              dflags
              {GHC.hscTarget = GHC.HscAsm, GHC.ghcLink = GHC.LinkInMemory}
    }

runSessionT
    :: (MonadIO m, MonadMask m)
    => SessionPref -> SessionT m a -> m a
runSessionT SessionPref {..} m =
    flip evalStateT (GHC.panic "Null Session") $
    unSessionT $
    GHC.defaultErrorHandler fatalMsg flushOut $ do
        liftIO GHC.installSignalHandlers
        GHC.initGhcMonad libDir
        GHC.withCleanupSession $ do
            dflags <- GHC.getSessionDynFlags
            void $ GHC.setSessionDynFlags $ dynFlags dflags
            m
