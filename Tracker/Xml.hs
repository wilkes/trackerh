module Tracker.Xml where 

import Text.XML.HaXml
import Tracker.Types

parseResponse :: String -> Content
parseResponse = content . (xmlParse "response")
    where content (Document _ _ e _) = CElem e

getToken :: Content -> String
getToken e = verbatim $ tag "token" /> tag "guid" /> txt $ e

xml2Stories :: Content -> [Story]
xml2Stories e = map xml2Story (tag "stories" /> tag "story" $ e)

xml2Story :: Content -> Story
xml2Story e = 
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
    where st k = verbatim $ tag "story" /> tag k /> txt $ e

xml2Projects :: Content -> [Project]
xml2Projects e = map xml2Project (tag "projects" /> tag "project" $ e)

xml2Project :: Content -> Project
xml2Project e = 
    Project { prjID = st "id",
              prjName = st "name",
              prjIterationLength = st "iteration_length",
              prjWeekStartDay = st "week_start_day",
              prjPointScale = st "point_scale" }
    where st k = verbatim $ tag "project" /> tag k /> txt $ e
