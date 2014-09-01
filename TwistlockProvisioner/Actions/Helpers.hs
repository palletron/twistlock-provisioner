module TwistlockProvisioner.Actions.Helpers 
(
	runAction,
	runActionWithInput,
	ActionResult,
	readCommand
)
where
import System.Process
import System.IO
import Pipes.Prelude
import Pipes.Core
import Pipes
import Data.ByteString hiding (hGetContents)

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
	-> ByteString -- Input
	-> IO (ActionResult m)
runActionWithInput command input = do
	(inp, outH, errH, pHandle) <- runInteractiveCommand command
	hPut inp input
	hClose inp
	return $ actionResult outH errH pHandle

readCommand :: String -> IO String
readCommand command = do
	(_, outH, _, _) <- runInteractiveCommand command
	hGetContents outH

actionResult :: MonadIO m => Handle -> Handle -> ProcessHandle -> ActionResult m 
actionResult outH errH pHandle = (outP, errP, pHandle)
	where
		outP = fromHandle outH
		errP = fromHandle errH
