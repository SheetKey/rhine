{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.Schedule.Deterministically where

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Schedule


duplicateDualTick :: Monad m
                  => MSF m () (time, Either (Either a b) c)
                  -> MSF m () (time, Either (Either a b) (Either a c))
duplicateDualTick runningClock = concatS $ runningClock >>> arr dupA
  where
    dupA (time, Left (Left  a)) = [ (time, Left (Left a)), (time, Right (Left a)) ]
    dupA (time, Left (Right b)) = [ (time, Left (Right b)) ]
    dupA (time, Right c)        = [ (time, Right (Right c)) ]



schedDualPar :: Monad m
             => Schedule m (ParClock m cla clb) clc
             -> Schedule m (ParClock m cla clb) (ParClock m cla clc)
schedDualPar sched = Schedule $ \parab parac -> do
  let clc = parallelCl2 parac
  (runningClock, initTime) <- (initSchedule sched) parab clc
  return (duplicateDualTick runningClock, initTime)


schedSeqParLeft
  :: Monad m
  => 
  -> Schedule m (ParClock m cla clb) (SeqClock m (ParClock m cla clc) (ParClock m cla cld))
schedSeqParLeft sched = Schedule $ \par seq -> do
  let cla = parallelCl1 par
      clb = parallelCl2 par
      clc = parallelCl2 $ sequentialCl1 seq
      cld = parallelCl2 $ sequentialCl2 seq
  (runningClock, initTime) <- initSchedule concurrently clb seq
  return (duplicateMyTick runningClock, initTime)

duplicateMyTick
  :: Monad m
  => MSF m () (time, Either b (Either (Either a c) (Either a d)))
  -> MSF m () (time, Either (Either a b) (Either (Either a c) (Either a d)))
duplicatedMyTick runningClock = concatS $ runningClock >>> arr dupA
  where
    dupA (time, Right (Left (Left a))) = [ (time, Left (Left a)), (time, (Right, Left (Left a))) ]
    dupA (time, Left b) = [ (time, Left (Right b)) ]
    dupA x = [ x ]
