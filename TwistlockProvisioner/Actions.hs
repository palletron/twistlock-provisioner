module TwistlockProvisioner.Actions where
import System.Process
import System.Exit
import System.IO
import Pipes.Prelude
import Pipes.Core
import Pipes

echoHelloWorld :: IO ExitCode
echoHelloWorld = do
	handle <- runCommand "echo \"Hello World!\""
	waitForProcess handle

-- We want to run a command, and read its exit code and
-- its stdout.
--
-- We want to run a command pass it some file over stdin,
-- and read its exit code and its stdout.
runActionWithInput
	:: MonadIO m => String -- Action command
	-> String -- Input
	-> IO (
		Producer String m (), -- Stdout 
		Producer String m (), -- Stderr
		ProcessHandle)
runActionWithInput command input = do
	(inp, outH, errH, pHandle) <- runInteractiveCommand command
	hPutStr inp input
	hClose inp
	return (outH, errH, pHandle)
	let outP = fromHandle outH
	let errP = fromHandle errH
	return (outP, errP, pHandle)
        
echoHelloWorldWithInput :: IO ExitCode
echoHelloWorldWithInput = do
	(outP, _, pHandle) <- runActionWithInput "./hello-world" "Haskell World\n"
	runEffect $ outP >-> stdoutLn
	waitForProcess pHandle
