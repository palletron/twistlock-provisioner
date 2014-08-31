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
		exitCode <- liftIO Action.echoHelloWorldWithInput
		if exitCode == ExitSuccess
			then json $ object [ "status" .= ("ok" :: String) ]
			else json $ object [ "status" .= ("not ok" :: String)]

	{-
	 -get "/container-templates" $ do
	 -    templates <- liftIO $ listContainers cfg
	 -    json $ object ["container-templates" .= templates]
	 -}

	post "/container-templates" $ do
		name <- param "name"
		url <- param "url"
		streamAction $ downloadContainerTemplateGit cfg name url

	post "/container-templates/:name/update" $ do
		name <- param "name"
		streamAction $ updateContainerTemplateGit cfg name

	get "/container-instances" $ do
		json $ object [ "status" .= ("ok" :: String) ]

	post "/container-instances" $ do
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
