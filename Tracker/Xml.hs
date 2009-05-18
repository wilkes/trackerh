module Tracker.Xml where 

import Text.XML.HaXml
import Tracker.Types

parseResponse :: String -> Content
parseResponse res = content $ xmlParse "response" res
    where content (Document _ _ e _) = CElem e

getToken :: String -> String
getToken s = item "token" content "guid"
    where content = parseResponse s

item :: String -> Content -> String -> String
item parent content key = 
    verbatim $ tag parent /> tag key /> txt $ content

class XmlRecord a where
    xml2Records :: CFilter -> String -> [a]
    xml2Records path s = map contentToRecord $ path $ parseResponse s

    toRecord :: String -> a
    toRecord = contentToRecord . parseResponse

    contentToRecord :: Content -> a
    toRecords :: String -> [a]


instance XmlRecord Project where
    toRecords = xml2Records $ tag "projects" /> tag "project"
    contentToRecord c = 
        Project { prjID = st "id",
                  prjName = st "name",
                  prjIterationLength = st "iteration_length",
                  prjWeekStartDay = st "week_start_day",
                  prjPointScale = st "point_scale" }
        where st = item "project" c

instance XmlRecord Story where
    toRecords = xml2Records $ tag "stories" /> tag "story"
    contentToRecord c = 
        Story { stID           = st "id",
                stType         = st "story_type",
                stURL          = st "url",
                stEstimate     = st "estimate",
                stCurrentState = st "current_state",
                stDescription  = st "description",
                stName         = st "name",
                stRequestedBy  = st "requested_by",
                stCreatedAt    = st "craeted_at",
                stLabels       = st "labels" }
        where st = item "story" c
