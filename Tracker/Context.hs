module Tracker.Context
    ( ProjectM
    , callRemote
    , projectURL
    , activitiesURL
    , unpickleWith
    , unpickle
    , unpickleResponse
    , storiesURL
    , tokenDELETE
    , tokenPOST
    , tokenPUT
    , pushEntity
    , (<++>)
    , runProjectM
    ) 
    where

import Tracker.Pickle

import Text.XML.HXT.Arrow.Pickle
import Control.Monad.Reader
import Network.Curl

data Config = Config { cfgToken :: String
                     , cfgProjectID :: String
                     }

type ProjectM = ReaderT Config IO

runProjectM :: ProjectM a -> String -> String -> IO a
runProjectM f tk pid = runReaderT f (Config tk pid)

projectID :: ProjectM String
projectID = ask >>= return . cfgProjectID

token :: ProjectM String
token = ask >>= return . cfgToken

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: ProjectM String
projectURL = projectID >>= return . url
    where url pid = serviceURL ++ "projects/" ++ pid

storiesURL :: ProjectM String
storiesURL  = projectURL <++> "/stories"

activitiesURL :: ProjectM String
activitiesURL = projectID >>= \pid ->
                case pid of
                  "" -> return $ serviceURL ++ "activities"
                  _ -> projectURL <++> "/activities"

(<++>) :: ProjectM String -> String -> ProjectM String
m <++> s = m >>= return . (++ s)

unpickle :: (XmlPickler a) => String -> ProjectM a
unpickle = unpickleWith xpickle

unpickleWith :: (XmlPickler a) => PU a -> String -> ProjectM a
unpickleWith xp url = tokenGET url >>= unpickleResponse xp

tokenGET :: String -> ProjectM String
tokenGET url = callRemoteWith url []

tokenPOST :: [String] -> String -> ProjectM String
tokenPOST ps url = callRemoteWith url [CurlPost True, CurlPostFields ps]

tokenPUT :: [String] -> String -> ProjectM String
tokenPUT ps url = callRemoteWith url [CurlCustomRequest "PUT", CurlPostFields ps]

tokenDELETE :: String -> ProjectM String
tokenDELETE url = callRemoteWith url [CurlCustomRequest "DELETE", CurlPost False]

pushEntity :: (XmlPickler a) => a -> PU a -> ([String] -> String -> ProjectM String) -> String -> ProjectM a
pushEntity ent pickler webAction url = (liftIO $ runPickle pickler ent) >>=
                                        (flip webAction url) >>=
                                        unpickleResponse pickler

callRemoteWith :: String -> [CurlOption] -> ProjectM String
callRemoteWith url extras = do
  h <- defaultHeaders
  liftIO $ callRemote url $ h ++ extras


unpickleResponse :: (XmlPickler a) => PU a -> String -> ProjectM a
unpickleResponse xp s = liftIO $ runUnpickle xp s >>= return . head

defaultHeaders :: ProjectM [CurlOption]
defaultHeaders = token >>= return . opts
    where opts tk = [CurlHttpHeaders
                     ["X-TrackerToken: " ++ tk
                     , "Content-type: application/xml"]]

callRemote :: String -> [CurlOption] -> IO String
callRemote url opts = 
    getResponse >>= \response ->
        case (respCurlCode response) of
          CurlOK -> return $ respBody response
          _      -> fail $ msg response 
    where getResponse :: IO (CurlResponse_ [(String, String)] String)
          getResponse = curlGetResponse_ url opts
          msg r = url ++ "\n" ++ 
                  (show $ respStatus r) ++ respStatusLine r
