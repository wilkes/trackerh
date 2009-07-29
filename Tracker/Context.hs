module Tracker.Context
    ( TrackerM
    , runTrackerM
    , token
    , projectID
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
import Control.Applicative((<$>), Applicative(..))
import Control.Monad.Reader
import Network.Curl

data Config = Config { cfgToken :: String
                     , cfgProjectID :: String
                     }

type TrackerM = ReaderT Config IO

instance Monad m => Applicative (ReaderT s m) where 
    pure = return
    (<*>) = ap 

runTrackerM :: TrackerM a -> String -> String -> IO a
runTrackerM f tk pid = runReaderT f (Config tk pid)

projectID :: TrackerM String
projectID = cfgProjectID <$> ask

token :: TrackerM String
token = cfgToken <$> ask

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: TrackerM String
projectURL = url <$> projectID
    where url pid = serviceURL ++ "projects/" ++ pid

storiesURL :: TrackerM String
storiesURL  = projectURL <++> "/stories"

activitiesURL :: TrackerM String
activitiesURL = projectID >>= \pid ->
                case pid of
                  "" -> return $ serviceURL ++ "activities"
                  _ -> projectURL <++> "/activities"

(<++>) :: TrackerM String -> String -> TrackerM String
m <++> s = (++ s) <$> m

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
                                        flip webAction url >>=
                                        unpickleResponse pickler

callRemoteWith :: String -> [CurlOption] -> TrackerM String
callRemoteWith url extras = do
  h <- defaultHeaders
  liftIO $ callRemote url $ h ++ extras

unpickleResponse :: (XmlPickler a) => PU a -> String -> TrackerM a
unpickleResponse xp s = liftIO $ head <$> runUnpickle xp s

defaultHeaders :: TrackerM [CurlOption]
defaultHeaders = opts <$> token
    where opts tk = [CurlHttpHeaders
                     ["X-TrackerToken: " ++ tk
                     , "Content-type: application/xml"]]

callRemote :: String -> [CurlOption] -> IO String
callRemote url opts = 
    getResponse >>= \response -> do
        case (respCurlCode response) of
          CurlOK -> return $ respBody response
          _      -> fail $ msg response 
    where getResponse :: IO (CurlResponse_ [(String, String)] String)
          getResponse = curlGetResponse_ url opts
          msg r = url ++ "\n" ++ 
                  (show $ respStatus r) ++ respStatusLine r
