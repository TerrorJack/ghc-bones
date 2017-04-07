{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.GHC.Session where

import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.IORef
import qualified Exception as GHC
import qualified GHC

type Session = IORef GHC.HscEnv

newtype SessionT m a = SessionT
    { runSessionT :: ReaderT Session m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadReader Session
               , MonadIO
               , MonadBase b
               , MonadThrow
               , MonadCatch
               , MonadMask
               )

instance MonadTransControl SessionT where
    type StT SessionT a = StT (ReaderT Session) a
    liftWith = defaultLiftWith SessionT runSessionT
    restoreT = defaultRestoreT SessionT

instance MonadBaseControl b m =>
         MonadBaseControl b (SessionT m) where
    type StM (SessionT m) a = StM (ReaderT Session m) a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadIO m, MonadMask m) =>
         GHC.ExceptionMonad (SessionT m) where
    gcatch = catch
    gmask = mask
    gbracket = bracket
    gfinally = finally
