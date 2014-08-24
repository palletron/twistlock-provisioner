module TwistlockProvisioner.Actions where
import TwistlockProvisioner.Actions.Helpers
import TwistlockProvisioner.Configuration
import Filesystem.Path.CurrentOS as FP
import System.Process
import System.Exit
import Pipes.Prelude
import Pipes.Core
import Pipes
import Data.Text

{- To build a container, we need the current configuration
 - so we can navigate to the container template directory.
 - In that directory we can execute ./control build
 -}
runBuild :: MonadIO m => Configuration -> Text -> IO (ActionResult m)
runBuild cfg name = runAction buildCommand 
  where
  	buildCommand = undefined
	path = FP.append (containerTemplateDir cfg) (fromText name) 

echoHelloWorldWithInput :: IO ExitCode
echoHelloWorldWithInput = do
	(outP, _, pHandle) <- runActionWithInput "./hello-world" "Haskell World\n"
	runEffect $ outP >-> stdoutLn
	waitForProcess pHandle

echoHelloWorld :: IO ExitCode
echoHelloWorld = do
	handle <- runCommand "echo \"Hello World!\""
	waitForProcess handle
