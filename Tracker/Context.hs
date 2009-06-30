module Tracker.Context
    ( TrackerM
    , runTrackerM
    , projectURL
    , storiesURL
    , activitiesURL
    , unpickle
    , unpickleWith
    , unpickleResponse
    , doDelete
    , doPost
    , doPut
    , pushEntity
    , callRemote
    , (<++>)
    ) 
    where

import Tracker.Pickle

import Text.XML.HXT.Arrow.Pickle
import Control.Monad.Reader
import Network.Curl

data Config = Config { cfgToken :: String
                     , cfgProjectID :: String
                     }

type TrackerM = ReaderT Config IO

runTrackerM :: TrackerM a -> String -> String -> IO a
runTrackerM f tk pid = runReaderT f (Config tk pid)

projectID :: TrackerM String
projectID = ask >>= return . cfgProjectID

token :: TrackerM String
token = ask >>= return . cfgToken

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: TrackerM String
projectURL = projectID >>= return . url
    where url pid = serviceURL ++ "projects/" ++ pid

storiesURL :: TrackerM String
storiesURL  = projectURL <++> "/stories"

activitiesURL :: TrackerM String
activitiesURL = projectID >>= \pid ->
                case pid of
                  "" -> return $ serviceURL ++ "activities"
                  _ -> projectURL <++> "/activities"

(<++>) :: TrackerM String -> String -> TrackerM String
m <++> s = m >>= return . (++ s)

unpickle :: (XmlPickler a) => String -> TrackerM a
unpickle = unpickleWith xpickle

unpickleWith :: (XmlPickler a) => PU a -> String -> TrackerM a
unpickleWith xp url = doGet url >>= unpickleResponse xp

doGet :: String -> TrackerM String
doGet url = callRemoteWith url []

doPost :: [String] -> String -> TrackerM String
doPost ps url = callRemoteWith url [CurlPost True, CurlPostFields ps]

doPut :: [String] -> String -> TrackerM String
doPut ps url = callRemoteWith url [CurlCustomRequest "PUT", CurlPostFields ps]

doDelete :: String -> TrackerM String
doDelete url = callRemoteWith url [CurlCustomRequest "DELETE", CurlPost False]

pushEntity :: (XmlPickler a) => a -> PU a -> ([String] -> String -> TrackerM String) -> String -> TrackerM a
pushEntity ent pickler webAction url = (liftIO $ runPickle pickler ent) >>=
                                        (flip webAction url) >>=
                                        unpickleResponse pickler

callRemoteWith :: String -> [CurlOption] -> TrackerM String
callRemoteWith url extras = do
  h <- defaultHeaders
  liftIO $ callRemote url $ h ++ extras


unpickleResponse :: (XmlPickler a) => PU a -> String -> TrackerM a
unpickleResponse xp s = liftIO $ runUnpickle xp s >>= return . head

defaultHeaders :: TrackerM [CurlOption]
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
