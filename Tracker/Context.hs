module Tracker.Context where

import Tracker.Pickle

import Text.XML.HXT.Arrow.Pickle
import Control.Monad.Reader

import Network.Curl

data Config = Config {cfgToken :: String, cfgProjectID :: String }

type ProjectM = ReaderT Config IO

runProjectM :: ProjectM a -> String -> String -> IO a
runProjectM f token pid = runReaderT f (Config token pid)

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: String
projectURL = serviceURL ++ "projects"

projectIDM :: ProjectM String
projectIDM = ask >>= return . cfgProjectID

tokenM :: ProjectM String
tokenM = ask >>= return . cfgToken

projectURLM :: ProjectM String
projectURLM = projectIDM >>= \pid -> return $ projectURL ++ "/" ++ pid

storiesURLM :: ProjectM String
storiesURLM  = projectURLM <++> "/stories"

(<++>) :: ProjectM String -> String -> ProjectM String
m <++> s = m >>= return . (++ s)

unpickleM :: (XmlPickler a) => String -> ProjectM a
unpickleM = unpickleWithM xpickle

unpickleWithM :: (XmlPickler a) => PU a -> String -> ProjectM a
unpickleWithM xp url = tokenGETM url >>= unpickleResponseM xp

tokenGETM :: String -> ProjectM String
tokenGETM url = callRemoteWith url []

tokenPOSTM :: [String] -> String -> ProjectM String
tokenPOSTM ps url = callRemoteWith url [CurlPost True, CurlPostFields ps]

tokenPUTM :: [String] -> String -> ProjectM String
tokenPUTM ps url = callRemoteWith url [CurlCustomRequest "PUT", CurlPostFields ps]

tokenDELETEM :: String -> ProjectM String
tokenDELETEM url = callRemoteWith url [CurlCustomRequest "DELETE", CurlPost False]

pushEntityM :: (XmlPickler a) => a -> PU a -> ([String] -> String -> ProjectM String) -> String -> ProjectM a
pushEntityM ent pickler webAction url = (liftIO $ runPickle pickler ent) >>=
                                        (flip webAction url) >>=
                                        unpickleResponseM pickler

callRemoteWith :: String -> [CurlOption] -> ProjectM String
callRemoteWith url extras = do
  h <- defaultHeadersM
  liftIO $ callRemote url $ h ++ extras


unpickleResponseM :: (XmlPickler a) => PU a -> String -> ProjectM a
unpickleResponseM xp s = liftIO $ runUnpickle xp s >>= return . head

defaultHeadersM :: ProjectM [CurlOption]
defaultHeadersM = tokenM >>= return . opts
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
