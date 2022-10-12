{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module FRP.Rhine.Type.Except where


-- rhine
import FRP.Rhine.Type
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- transformers
import Control.Monad.Trans.Except

-- base
import Data.Void

-- dunai
import Control.Monad.Trans.MSF.Except


flowExcept
  :: ( Monad m
     )
  => RhineExcept m () () Void
  -> m ()
flowExcept rhe = do
  msf <- eraseExcept rhe
  reactimate $ safely msf >>> arr (const ())


eraseExcept
  :: ( Monad m
     )
  => RhineExcept m a b e
  -> m (MSFExcept m a (Maybe b) e)
eraseExcept (Try rh) = do
  msf <- runExceptT $ eraseClock rh
  case msf of
    Left  _   -> error "This should not be possible."
    Right msf -> return $ try msf
  
eraseExcept (Safe rh) = do
  msf <- eraseClock rh
  return $ safe msf

eraseExcept (OnException rhe1 rhe2) = do
  msf1 <- eraseExcept rhe1
  let msf = do
        msf1
        msf2 <- once_ $ eraseExcept rhe2
        msf2
  return msf

eraseExcept (HandleException rhe f) = do
  msf1 <- eraseExcept rhe
  let
    msf2 = do
      e <- msf1
      msf3 <- once_ $ eraseExcept $ f e
      msf3
  return msf2

  

data RhineExcept m a b e where
  Try
    :: ( Monad m
       , Clock (ExceptT e m) cl
       , GetClockProxy cl
       ) 
    => Rhine (ExceptT e m) cl a b
    -> RhineExcept m a b e
  Safe
    :: ( Monad m
       , Clock m cl
       , GetClockProxy cl
       )
    => Rhine m cl a b
    -> RhineExcept m a b e
  OnException
    :: (Monad m
       )
    => RhineExcept m a b e1
    -> RhineExcept m a b e2
    -> RhineExcept m a b e2
  HandleException
    :: ( Monad m
       )
    => RhineExcept m a b e1
    -> (e1 -> RhineExcept m a b e2)
    -> RhineExcept m a b e2
