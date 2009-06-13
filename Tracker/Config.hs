module Tracker.Config(loadConfig
                     , ConfigParser
                     , getToken
                     , getProject) where

import Data.ConfigFile
import Data.Either.Utils(forceEither)

loadConfig :: FilePath -> IO ConfigParser
loadConfig fp = readfile emptyCP fp >>= return . forceEither

forceGet :: String -> ConfigParser -> String
forceGet k cp = forceEither $ get cp "" k

getToken :: ConfigParser -> String
getToken    = forceGet "token"

getProject :: ConfigParser -> String
getProject  = forceGet "project"

