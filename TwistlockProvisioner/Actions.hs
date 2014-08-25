module TwistlockProvisioner.Actions where
import Prelude hiding (FilePath)
import TwistlockProvisioner.Actions.Helpers
import TwistlockProvisioner.Configuration
import Filesystem.Path.CurrentOS as FP
import System.Directory
import System.Process
import System.Exit
import Pipes.Prelude hiding (show)
import Pipes.Core
import Pipes
import Data.Text

{-- Download a container template via git --}
downloadContainerTemplateGit :: MonadIO m => Configuration -> Text -> Text -> IO (ActionResult m)
downloadContainerTemplateGit cfg name url = do
	createDirectoryIfMissing True (show templatePath)
	runAction getCommand 
  where
  	getCommand = "cd " ++ (show  templatePath) ++ "; " ++ "git clone " ++ (show url)
	templatePath = getTemplatePath cfg name

{- To build a container, we need the current configuration
 - so we can navigate to the container template directory.
 - In that directory we can execute ./control build
 -}
runBuild :: MonadIO m => Configuration -> Text -> IO (ActionResult m)
runBuild cfg name = runAction buildCommand 
  where
  	buildCommand = undefined
	path = getTemplatePath cfg name 

getTemplatePath :: Configuration -> Text -> FilePath
getTemplatePath cfg name = FP.append (containerTemplateDir cfg) (fromText name) 

echoHelloWorldWithInput :: IO ExitCode
echoHelloWorldWithInput = do
	(outP, _, pHandle) <- runActionWithInput "./hello-world" "Haskell World\n"
	runEffect $ outP >-> stdoutLn
	waitForProcess pHandle

echoHelloWorld :: IO ExitCode
echoHelloWorld = do
	handle <- runCommand "echo \"Hello World!\""
	waitForProcess handle
