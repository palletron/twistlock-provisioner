module TwistlockProvisioner.Actions where
import System.Process
import System.Exit

echoHelloWorld :: IO ExitCode
echoHelloWorld = do
  handle <- runCommand "echo \"Hello World\""
  waitForProcess handle
