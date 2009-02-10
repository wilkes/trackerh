module Main where

import System.Environment
import qualified Network.Curl as Curl
import Text.XML.HaXml

main = do args <- getArgs
          case args of
            ["token", username, password]        -> token username password
            ["projects", token]                  -> projects token
            ["project", token, projectID]        -> project token projectID
            ["stories", token, projectID]        -> stories token projectID
            ["story", token, projectID, storyID] -> story token projectID storyID
            _ -> printUsage

printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\n\
                      \trackerh projects token\n\
                      \trackerh project token projectID\n\
                      \trackerh stories token projectID\n\
                      \trackerh story token projectID storyID\n\
                      \\n"

serviceURL = "https://www.pivotaltracker.com/services/v2/"
projectURL = serviceURL ++ "projects/" 

token :: String -> String -> IO ()
token username password = callRemote url opts callback
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [Curl.CurlUserPwd $ username ++ ":" ++ password]
          callback = (putStrLn . getToken . (xmlParse "pivotal tracker response"))

getToken :: Document -> String
getToken (Document _ _ e _) = verbatim $ tag "token" /> tag "guid" /> txt $ CElem e

projects :: String -> IO ()
projects token = tokenCall url token putStrLn
    where url = serviceURL ++ "projects"

project :: String -> String -> IO ()
project token projectID = tokenCall url token putStrLn
    where url = projectURL ++ projectID

stories :: String -> String -> IO ()
stories token projectID = tokenCall url token putStrLn
    where url = projectURL ++ projectID ++ "/stories"

story :: String -> String -> String -> IO ()
story token projectID storyID = tokenCall url token putStrLn
    where url = projectURL ++ projectID ++ "/stories/" ++ storyID

tokenCall :: String -> String -> (String -> IO ()) -> IO ()
tokenCall url token callback = callRemote url opts callback
    where opts = [Curl.CurlHttpHeaders ["X-TrackerToken: " ++ token, "Content-type: application/xml"]]

callRemote :: String -> [Curl.CurlOption] -> (String -> IO ()) -> IO () 
callRemote url opts callback = 
    do (code, res) <- Curl.curlGetString url opts
       case code of
         Curl.CurlOK -> callback res
         _ -> fail $ show code