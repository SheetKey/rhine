{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.Schedule.Deterministically where

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Schedule

-- dunai
import Data.MonadicStreamFunction.Async



schedDualPar :: Monad m
             => Schedule m (ParClock m cla clb) clc
             -> Schedule m (ParClock m cla clb) (ParClock m cla clc)
schedDualPar sched = Schedule $ \parab parac -> do
  let clc = parallelCl2 parac
  (runningClock, initTime) <- (initSchedule sched) parab clc
  return (duplicateDualTick runningClock, initTime)


schedParLeftSeq
  :: (Monad m
     , Time cla ~ Time clb
     )
  => Schedule m clb (SeqClock m (ParClock m cla clc) cld)
  -> Schedule m (ParClock m cla clb) (SeqClock m (ParClock m cla clc) cld)
schedParLeftSeq sched = Schedule $ \par seq -> do
  let clb = parallelCl2 par
  (runningClock, initTime) <- (initSchedule sched) clb seq
  return (duplicateLeftCla runningClock, initTime)


duplicateDualTick :: Monad m
                  => MSF m () (time, Either (Either a b) c)
                  -> MSF m () (time, Either (Either a b) (Either a c))
duplicateDualTick runningClock = concatS $ runningClock >>> arr dupA
  where
    dupA (time, Left (Left  a)) = [ (time, Left (Left a)), (time, Right (Left a)) ]
    dupA (time, Left (Right b)) = [ (time, Left (Right b)) ]
    dupA (time, Right c)        = [ (time, Right (Right c)) ]


duplicateLeftCla
  :: Monad m
  => MSF m () (time, Either b (Either (Either a c) d))
  -> MSF m () (time, Either (Either a b) (Either (Either a c) d))
duplicateLeftCla runningClock = concatS $ runningClock >>> arr dupA
  where
    dupA (time, Left b) = [ (time, Left (Right b)) ]
    dupA (time, Right (Left (Left a))) = [ (time, Left (Left a)), (time, Right (Left (Left a))) ]
    dupA (time, Right x) = [ (time, Right x) ]

