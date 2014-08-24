module TwistlockProvisioner.Actions.Helpers 
(
	runAction,
	runActionWithInput
)
where
import System.Process
import System.IO
import Pipes.Prelude
import Pipes.Core
import Pipes

type ActionResult m = (
	Producer String m (), -- Stdout 
	Producer String m (), -- Stderr
	ProcessHandle)

runAction :: MonadIO m
	=> String -- Action command
	-> IO (ActionResult m)
runAction command = do
	(_, outH, errH, pHandle) <- runInteractiveCommand command
	return $ actionResult outH errH pHandle

runActionWithInput
	:: MonadIO m => String -- Action command
	-> String -- Input
	-> IO (ActionResult m)
runActionWithInput command input = do
	(inp, outH, errH, pHandle) <- runInteractiveCommand command
	hPutStr inp input
	hClose inp
	return $ actionResult outH errH pHandle

actionResult :: MonadIO m => Handle -> Handle -> ProcessHandle -> ActionResult m 
actionResult outH errH pHandle = (outP, errP, pHandle)
	where
		outP = fromHandle outH
		errP = fromHandle errH
