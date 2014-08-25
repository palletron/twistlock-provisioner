module TwistlockProvisioner.Pipes.Helpers where
import Control.Monad
import Control.Exception
import Control.Concurrent hiding (yield)
import Control.Monad.Trans.Control (control, MonadBaseControl)
import qualified Control.Concurrent.Lifted as L
import Pipes
import Pipes.Concurrent
 
{- Takes inputs from a list of producers, merging their outputs and outputting
 - as a single producer.
 - 
 - (Taken from Michael Snoyman https://gist.github.com/snoyberg/9500916)
 -}
merge :: (MonadIO m, MonadBaseControl IO m) => [Producer a m ()] -> Producer a m ()
merge producers = do
		(output, input) <- liftIO $ spawn Unbounded
		lift $ mapM_ (fork output) producers
		fromInput input
	where
		fork :: (MonadBaseControl IO m, MonadIO m) => Output a -> Producer a m () -> m ThreadId
		fork output producer = L.fork $ do
		runEffect $ producer >-> toOutput output
		liftIO performGC
