module System.Metrics.Prometheus.Metric.Counter
       ( Counter
       , CounterSample (..)
       , new
       , add
       , inc
       , sample
       , addAndSample
       ) where


import           Control.Applicative  ((<$>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter)


newtype Counter = Counter { unCounter :: AtomicCounter }
newtype CounterSample = CounterSample { unCounterSample :: Int }


new :: MonadIO m => m Counter
new = liftIO (Counter <$> newCounter 0)


addAndSample :: MonadIO m => Int -> Counter -> m CounterSample
addAndSample by | by >= 0   = liftIO . fmap CounterSample . incrCounter by . unCounter
                | otherwise = error "must be >= 0"


add :: MonadIO m => Int -> Counter -> m ()
add by c = liftIO (addAndSample by c >> pure ())


inc :: MonadIO m => Counter -> m ()
inc = add 1


sample :: MonadIO m => Counter -> m CounterSample
sample = addAndSample 0
