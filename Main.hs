module Main where

import System.Environment
import qualified Network.Curl as Curl
import Text.XML.HaXml

main = do args <- getArgs
          case args of
            ["token", username, password] -> token username password
            ["projects", token] -> projects token
            _ -> printUsage

printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\
                      \trackerh projects token\
                      \\n"

parseResponse = xmlParse "pivotal tracker response"

token :: String -> String -> IO ()
token username password = callRemote url opts callback
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [Curl.CurlUserPwd $ username ++ ":" ++ password]
          callback = (putStrLn . getToken . parseResponse)

getToken :: Document -> String
getToken (Document _ _ e _) = verbatim $ tag "token" /> tag "guid" /> txt $ CElem e

projects :: String -> IO ()
projects token = callRemote url opts putStrLn
    where url = "https://www.pivotaltracker.com/services/v2/projects"
          opts = [Curl.CurlHttpHeaders ["X-TrackerToken: " ++ token, "Content-type: application/xml"]]

callRemote :: String -> [Curl.CurlOption] -> (String -> IO ()) -> IO () 
callRemote url opts callback = 
    do (code, res) <- Curl.curlGetString url opts
       case code of
         Curl.CurlOK -> callback res
         _ -> fail $ show code