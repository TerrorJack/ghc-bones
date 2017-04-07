{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.GHC.Session where

import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import qualified DynFlags as GHC
import qualified Exception as GHC
import qualified GHC
import qualified HscTypes as GHC

type Session = GHC.HscEnv

newtype SessionT m a = SessionT
    { runSessionT :: StateT Session m a
    } deriving ( Functor
               , Applicative
               , Monad
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
    liftWith = defaultLiftWith SessionT runSessionT
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
