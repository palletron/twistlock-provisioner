module TwistlockProvisioner.Actions where
import Prelude hiding (FilePath)
import TwistlockProvisioner.Actions.Helpers
import TwistlockProvisioner.Configuration
import Filesystem.Path.CurrentOS as FP
import System.Directory
import Pipes
import Data.Text hiding (filter)
import qualified Data.Yaml as Y
import qualified Data.Aeson as J
import Data.ByteString.Lazy (toStrict)

{-- Download a container template via git --}
downloadContainerTemplateGit :: MonadIO m => Configuration -> String -> String -> IO (ActionResult m)
downloadContainerTemplateGit cfg name url = do
	createDirectoryIfMissing True (encodeString templatePath)
	runAction getCommand 
  where
	getCommand = "cd " ++ (encodeString templatePath) ++ "; " ++ "git clone " ++ url ++ " " ++ name
	templatePath = containerTemplateDir cfg

{- Return all container templates and their descriptions
 -}
listContainers :: Y.FromJSON a => Configuration -> IO ([(String, Maybe a)])
listContainers cfg = do
	names <- getDirectoryContents $ encodeString templateDirPath
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
getGitUrl cfg name = readCommand ((cdToTemplatePath cfg name) ++ "; " ++ getUrlCommand)
	where
	getUrlCommand = "git remote show -n origin|grep Fetch | awk '{ print $3 }'"

updateContainerTemplateGit :: MonadIO m => Configuration -> String -> IO (ActionResult m)
updateContainerTemplateGit cfg name = runAction $ (cdToTemplatePath cfg name) ++ "; " ++ "git pull"

{- Command for navigating to template path
 - -}
cdToTemplatePath :: Configuration -> String -> String
cdToTemplatePath cfg name = "cd " ++ (encodeString $ getTemplatePath cfg $ pack name)

{- To build a container, we need the current configuration
 - so we can navigate to the container template directory.
 - In that directory we can execute ./control build
 -}
buildContainer :: MonadIO m => Configuration -> Text -> IO (ActionResult m)
buildContainer cfg name = runAction command 
	where
		command = (cdToTemplatePath cfg (unpack name)) ++ "; " ++ "./control build"

deleteContainer :: MonadIO m => Configuration -> Text -> IO (ActionResult m)
deleteContainer cfg name = runAction command 
	where
		command = "rm -r " ++ (encodeString $ getTemplatePath cfg name)

startContainer :: (MonadIO m) => Configuration -> Text -> Y.Value -> IO (ActionResult m)
startContainer cfg name options = runActionWithInput command input
	where
		input = toStrict $ J.encode options
		command = (cdToTemplatePath cfg (unpack name)) ++ "; " ++ "./control run"

linkContainer :: (MonadIO m) => Configuration -> Text -> Text -> Y.Value -> IO (ActionResult m)
linkContainer cfg name cid options = runActionWithInput command input
	where
		input = toStrict $ J.encode options
		cdToT = cdToTemplatePath cfg (unpack name)
		command = cdToT ++ "; " ++ "./control link " ++ (unpack cid)

getTemplatePath :: Configuration -> Text -> FilePath
getTemplatePath cfg name = FP.append (containerTemplateDir cfg) (fromText name) 

