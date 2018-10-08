module System.Metrics.Prometheus.Metric.Gauge
       ( Gauge
       , GaugeSample (..)
       , new
       , add
       , sub
       , inc
       , dec
       , set
       , sample
       , modifyAndSample
       ) where

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef          (IORef, atomicModifyIORef', newIORef)


newtype Gauge = Gauge { unGauge :: IORef Double }
newtype GaugeSample = GaugeSample { unGaugeSample :: Double }


new :: MonadIO m => m Gauge
new = liftIO (Gauge <$> newIORef 0)


modifyAndSample :: MonadIO m => (Double -> Double) -> Gauge -> m GaugeSample
modifyAndSample f = liftIO . flip atomicModifyIORef' g . unGauge
  where g v = (f v, GaugeSample $ f v)


add :: MonadIO m => Double -> Gauge -> m ()
add x g = modifyAndSample (+ x) g >> pure ()


sub :: MonadIO m => Double -> Gauge -> m ()
sub x g = modifyAndSample (subtract x) g >> pure ()


inc :: MonadIO m => Gauge -> m ()
inc = add 1


dec :: MonadIO m => Gauge -> m ()
dec = sub 1


set :: MonadIO m => Double -> Gauge -> m ()
set x g = modifyAndSample (const x) g >> pure ()


sample :: MonadIO m => Gauge -> m GaugeSample
sample = modifyAndSample id
