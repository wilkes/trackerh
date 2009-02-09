module Main where

import System.Environment
import qualified Network.Curl as Curl
import Text.XML.HaXml

main = do args <- getArgs
          case args of
            ["token", username, password] -> token username password
            _ -> printUsage

printUsage = putStrLn "Usage: trackerh command [args]\n\
                       \trackerh token username password\n"

token :: String -> String -> IO ()
token username password =
    do (code, res) <- Curl.curlGetString url opts
       case code of
         Curl.CurlOK -> putStrLn $ getToken $ xmlParse "pivotal tracker repsonse" res
         _ -> fail $ show code
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [Curl.CurlUserPwd $ username ++ ":" ++ password]

getToken :: Document -> String
getToken (Document _ _ e _) = verbatim $ tag "token" /> tag "guid" /> txt $ CElem e
