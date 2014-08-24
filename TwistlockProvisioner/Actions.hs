module TwistlockProvisioner.Actions where
import TwistlockProvisioner.Helpers
import System.Process
import System.Exit
import Pipes.Prelude
import Pipes.Core
import Pipes

echoHelloWorldWithInput :: IO ExitCode
echoHelloWorldWithInput = do
	(outP, _, pHandle) <- runActionWithInput "./hello-world" "Haskell World\n"
	runEffect $ outP >-> stdoutLn
	waitForProcess pHandle

echoHelloWorld :: IO ExitCode
echoHelloWorld = do
	handle <- runCommand "echo \"Hello World!\""
	waitForProcess handle
