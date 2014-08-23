{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Aeson hiding (json)
import TwistlockProvisioner.Actions as Action
import Control.Monad.IO.Class
import System.Exit

main :: IO()
main = scotty 3000 $ do
	get "/status.json" $ do
		exitCode <- liftIO Action.echoHelloWorld
		if exitCode == ExitSuccess
		  then json $ object [ "status" .= ("ok" :: String) ]
		  else json $ object [ "status" .= ("not ok" :: String)]

	get "/container-templates" $ do
		json $ object [ "status" .= ("ok" :: String) ]

	post "/container-templates" $ do
		json $ object [ "status" .= ("ok" :: String) ]

	get "/container-instances" $ do
		json $ object [ "status" .= ("ok" :: String) ]

	post "/container-instances" $ do
		json $ object [ "status" .= ("ok" :: String) ]
