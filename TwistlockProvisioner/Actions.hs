module TwistlockProvisioner.Actions where
import Prelude hiding (FilePath)
import TwistlockProvisioner.Actions.Helpers
import TwistlockProvisioner.Configuration
import Filesystem.Path.CurrentOS as FP
import System.Directory
import System.Process
import System.Exit
import Pipes.Prelude hiding (show, mapM, filter)
import Pipes.Core
import Pipes
import Data.Text hiding (filter)
import qualified Data.Yaml as Y

{-- Download a container template via git --}
downloadContainerTemplateGit :: MonadIO m => Configuration -> Text -> Text -> IO (ActionResult m)
downloadContainerTemplateGit cfg name url = do
	createDirectoryIfMissing True (unpack $ encode $ templatePath)
	runAction getCommand 
  where
  	getCommand = "cd " ++ (unpack $ encode templatePath) ++ "; " ++ "git clone " ++ (unpack $ url) ++ " " ++ (unpack name)
	templatePath = containerTemplateDir cfg

{- Return all container templates and their descriptions
 -}
listContainers :: Y.FromJSON a => Configuration -> IO ([(String, Maybe a)])
listContainers cfg = do
	names <- getDirectoryContents $ unpack $ encode $ templateDirPath
	let filteredNames = filter (\ n -> n /= "." && n /= "..") names
	mapM getDescription filteredNames
	where
	templateDirPath = containerTemplateDir cfg
	getDescription :: Y.FromJSON a => String -> IO (String, Maybe a)
	getDescription name = do
		description <- getContainerDescription cfg name
		return (name, description)

{- Get and parse the twistlock.yml of a container template
 - -}
getContainerDescription :: Y.FromJSON a => Configuration -> String -> IO (Maybe a)
getContainerDescription cfg name = Y.decodeFile containerTemplatePath
	where
	containerTemplatePath = encodeString $ (containerTemplateDir cfg) </> (decodeString name) </> (decodeString "twistlock.yml")

{-
 - Gets the git url of a container template
 - -}
getGitUrl :: Configuration -> String -> IO String
getGitUrl cfg name = readCommand (cdToTemplatePath ++ "; " ++ getUrlCommand)
	where
	templatePath = getTemplatePath cfg $ pack name
	cdToTemplatePath = "cd " ++ ( unpack $ encode $ templatePath)
	getUrlCommand = "git remote show -n origin|grep Fetch | awk '{ print $3 }'"
		

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
