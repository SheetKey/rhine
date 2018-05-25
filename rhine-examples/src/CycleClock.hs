{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}

-- transformers
import Control.Monad.IO.Class

-- rhine
import Control.Monad.Schedule
import FRP.Rhine
import FRP.Rhine.Clock.Cycle

type MyClock = CycleClock '[500, 1000]

everyNowAndThen :: Monad m => ClSF m MyClock arbitrary String
everyNowAndThen = sinceInitS >>> proc time ->
  returnA -< unwords ["It's now", show time, "o'clock."]

mainRhine :: MonadIO m => Rhine (ScheduleT Integer m) MyClock () ()
mainRhine = everyNowAndThen >-> arrMCl (liftIO . putStrLn) @@ CycleClock

main :: IO ()
main = runScheduleIO $ flow mainRhine
