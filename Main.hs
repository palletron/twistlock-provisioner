{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Aeson hiding (json)
import TwistlockProvisioner.Actions as Action
import TwistlockProvisioner.Configuration
import Control.Monad.IO.Class
import Filesystem.Path.CurrentOS
import System.Exit
import System.Process
import Pipes.Prelude hiding (show)
import Pipes.Core
import Pipes
import Control.Concurrent

main :: IO()
main = scotty 3000 $ do
	get "/status.json" $ do
		json $ object [ "status" .= ("ok" :: String) ]

	get "/templates" $ do
		templates <- liftIO $ (listContainers cfg :: IO [(String, Maybe Value)])
		json $ object ["templates" .= templates]

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

	get "/templates/:name/instances/:instance_id" $ do
		json $ object [ "status" .= ("ok" :: String) ]

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
		json $ object [ "status" .= ("ok" :: String) ]

	where
		cfg = Configuration (fromText "templates/")
		streamAction action = do
			(out, err, pHandle) <- liftIO $ action
			liftIO $ forkIO $ runEffect $ out >-> stdoutLn
			liftIO $ forkIO $ runEffect $ err >-> stdoutLn

			exitCode <- liftIO $ waitForProcess pHandle
			if exitCode == ExitSuccess
				then json $ object [ "status" .= ("ok" :: String) ]
				else json $ object [ "status" .= ("not ok" :: String)]
