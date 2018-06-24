{- |
Interpolation buffers.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.ResamplingBuffer.Interpolation where

-- containers
import Data.Sequence

-- dunai
import Data.VectorSpace
import Data.VectorSpace.Tuples

-- rhine
import FRP.Rhine
import FRP.Rhine.ResamplingBuffer.KeepLast

-- | A simple linear interpolation based on the last calculated position and velocity.
linear
  :: ( Monad m, Clock m cl1, Clock m cl2
     , VectorSpace v
     , Groundfield v ~ Diff (Time cl1)
     , Groundfield v ~ Diff (Time cl2)
     )
  => v -- ^ The initial velocity (derivative of the signal)
  -> v -- ^ The initial position
  -> ResamplingBuffer m cl1 cl2 v v
linear initVelocity initPosition
  =    (derivativeFrom initPosition &&& clId) &&& timeInfoOf sinceInit
  ^->> keepLast ((initVelocity, initPosition), 0)
  >>-^ proc ((velocity, lastPosition), sinceInit1) -> do
    sinceInit2 <- timeInfoOf sinceInit -< ()
    let diff = sinceInit2 - sinceInit1
    returnA -< lastPosition ^+^ velocity ^* diff

{- |
sinc-Interpolation, or Whittaker-Shannon-Interpolation.

The incoming signal is strictly bandlimited
by the frequency at which @cl1@ ticks.
Each incoming value is hulled in a sinc function,
these are added and sampled at @cl2@'s ticks.
In order not to produce a space leak,
the buffer only remembers the past values within a given window,
which should be chosen much larger than the average time between @cl1@'s ticks.
-}
sinc
  :: ( Monad m, Clock m cl1, Clock m cl2
     , VectorSpace v
     , Ord (Groundfield v)
     , Floating (Groundfield v)
     , Groundfield v ~ Diff (Time cl1)
     , Groundfield v ~ Diff (Time cl2)
     )
  => Groundfield v
  -- ^ The size of the interpolation window
  --   (for how long in the past to remember incoming values)
  -> ResamplingBuffer m cl1 cl2 v v
sinc windowSize = historySince windowSize ^->> keepLast empty >>-^ proc as -> do
  sinceInit2 <- sinceInitS -< ()
  returnA                  -< vectorSum $ mkSinc sinceInit2 <$> as
  where
    mkSinc sinceInit2 (TimeInfo {..}, as)
      = let t = pi * (sinceInit2 - sinceInit) / sinceLast
        in  as ^* (sin t / t)
    vectorSum = foldr (^+^) zeroVector

-- TODO Do we want to give initial values?
-- | Interpolates the signal with Hermite splines,
--   using 'threePointDerivative'.
--
--   Caution: In order to calculate the derivatives of the incoming signal,
--   it has to be delayed by two ticks of @cl1@.
--   In a non-realtime situation, a higher quality is achieved
--   if the ticks of @cl2@ are delayed by two ticks of @cl1@.
cubic
  :: ( Monad m
     , VectorSpace v
     , Groundfield v ~ Diff (Time cl1)
     , Groundfield v ~ Diff (Time cl2)
     )
  => ResamplingBuffer m cl1 cl2 v v
cubic = ((delay zeroVector &&& threePointDerivative) &&& (sinceInitS >-> delay 0))
    >-> (clId &&& delay (zeroVector, 0))
   ^->> keepLast ((zeroVector, 0), (zeroVector, 0))
   >>-^ proc (((dv, v), t1), ((dv', v'), t1')) -> do
     t2 <- sinceInitS -< ()
     let
       t        = (t1 - t1') / (t2 - t1')
       tsquared = t ^ 2
       tcubed   = t ^ 3
       vInter   = ( 2 * tcubed - 3 * tsquared     + 1) *^  v'
              ^+^ (     tcubed - 2 * tsquared + t    ) *^ dv'
              ^+^ (-2 * tcubed + 3 * tsquared        ) *^  v
              ^+^ (     tcubed -     tsquared        ) *^ dv
     returnA -< vInter
