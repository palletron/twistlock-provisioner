{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Aeson hiding (json)

main = scotty 3000 $ do
	get "/" $ do
		json $ object [ "status" .= ("ok" :: String) ]
