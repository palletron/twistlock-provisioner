{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Aeson as J hiding (json)
import TwistlockProvisioner.Actions as Action
import TwistlockProvisioner.Actions.Helpers as Action
import TwistlockProvisioner.Configuration
import Control.Monad.IO.Class
import Filesystem.Path.CurrentOS
import System.Exit
import System.Process
import Pipes.Prelude hiding (show)
import Pipes.Core
import Pipes
import Control.Concurrent
import Data.ByteString.Lazy.Char8 as BL

main :: IO()
main = scotty 3000 $ do
	get "/status.json" $ do
		json $ object [ "status" .= ("ok" :: String) ]

	get "/templates" $ do
		templates <- liftIO $ (listContainers cfg :: IO [(String, Maybe Value)])
		json $ object ["templates" .= templates]

	get "/templates/:name" $ do
		name <- param "name"
		template <- liftIO $ getContainerDescription cfg name
		json $ case template of
			Just t -> t
			Nothing -> object [ "status" .= ("ok" :: String) ]

	post "/templates" $ do
		name <- param "name"
		url <- param "url"
		streamAction $ downloadContainerTemplateGit cfg name url

	post "/templates/:name/update" $ do
		name <- param "name"
		streamAction $ updateContainerTemplateGit cfg name

	post "/templates/:name/build" $ do
		name <- param "name"
		streamAction $ buildContainer cfg name

	delete "/templates/:name" $ do
		name <- param "name"
		streamAction $ deleteContainer cfg name

	get "/templates/:name/instances/:instance_id" $ do
		json $ object [ "status" .= ("not implemented" :: String) ]

	post "/templates/:name/instances" $ do
		name <- param "name"
		options <- jsonData
		streamAction $ startContainer cfg name options

	put "/templates/:name/instances/:instance_id/links" $ do
		name <- param "name"
		instanceId <- param "instance_id"
		options <- jsonData
		streamAction $ linkContainer cfg name instanceId options

	delete "/templates/:name/instances/:instance_id" $ do
		json $ object [ "status" .= ("not implemented" :: String) ]


	{- Posting a port and ip of a container to exposed will make the provisioner
	 - forward traffic on a given port to the container port and address
	 -}
	post "/exposed" $ do
		container_ip <- param "container_ip"
		container_port <- param "container_port"
		port <- param "port"
		json $ object [ "status" .= ("not implemented" :: String) ]

	where
		cfg = Configuration (fromText "templates/")

streamAction :: IO (ActionResult IO) -> ActionM ()
streamAction action = do
	(out, err, pHandle) <- liftIO $ action

	result <- liftIO $ fold (++) "" id out
	liftIO $ forkIO $ runEffect $ err >-> stdoutLn

	let maybeJson = (J.decode $ pack $ result :: Maybe Value)
	exitCode <- liftIO $ waitForProcess pHandle

	if exitCode == ExitSuccess
		then case maybeJson of
			Just v -> json $ v
			Nothing -> json $ object [ "status" .= ("ok, no json output" :: String)]
		else json $ object [ "status" .= ("not ok" :: String)]
