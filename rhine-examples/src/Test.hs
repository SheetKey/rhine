{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Test where

import FRP.Rhine
import FRP.Rhine.Schedule.Deterministically
  
test1 :: Rhine IO (ParClock IO clL clR) a b
test1 = undefined

test2 :: Rhine IO (ParClock IO clL clR) a b
test2 = undefined

test3 :: Rhine IO (ParClock IO clL clR) a b
test3 = undefined

combine
  :: ( Time cla ~ Time clb
     , Time (Out cla) ~ Time clb
     , Time (In cla) ~ Time clb
     , Time cld ~ Time clb
     , GetClockProxy cla, GetClockProxy clb
     , GetClockProxy clc, GetClockProxy cld
     , Clock IO (Out clc), Clock IO (In cld)
     , Clock IO cla, Clock IO clb
     , Clock IO clc, Clock IO cld
     , Clock IO (Out cla), Clock IO (Out clb)
     , Clock IO (Out cld), Clock IO (In cla)
     , Clock IO (In clb), Clock IO (In clc)
     )
  => Rhine IO (SeqClock IO (ParClock IO cla clb) (SeqClock IO (ParClock IO cla clc) (ParClock IO cla cld))) a b
combine = test1 >-- fifoUnbounded -@- schedParLeftSeq concurrently -->
          (test2 >-- fifoUnbounded -@- schedDualPar    concurrently --> test3)

main :: IO ()
main = print 5
