module Tracker.Pickle where

import Text.XML.HXT.Arrow.Pickle
import Tracker.Types

instance XmlPickler Project where
    xpickle = xpProject

instance XmlPickler Story where
    xpickle = xpStory

instance XmlPickler Iteration where
    xpickle = xpIteration

xpProjects :: PU Projects
xpProjects = xpListOf "projects"

xpProject :: PU Project
xpProject = xpElem "project" $
            xpWrap ( \(pid,name,itlength,day,scale) -> Project pid name itlength day scale
                   , \p -> (prjID p, prjName p, prjIterationLength p, prjWeekStartDay p, prjPointScale p)
                   ) $
            xp5Tuple (xpElVal "id")
                     (xpElVal "name")
                     (xpElVal "iteration_length")
                     (xpElVal "week_start_day")
                     (xpElVal "point_scale")

xpStories :: PU Stories
xpStories = xpListOf "stories" 

xpStory :: PU Story
xpStory = xpElem "story" $
          xpWrap ( \ ((id,typ,url,est,curr),(desc,name,req,created,labels)) ->
                       Story id typ url est curr desc name req created labels
                 , \st -> ((stID st, stType st, stURL st
                           ,stEstimate st, stCurrentState st)
                          ,(stDescription st,stName st, stRequestedBy st
                           ,stCreatedAt st, stLabels st))
                 ) $
          xpPair (xp5Tuple (xpElVal "id")
                           (xpElVal "story_type")
                           (xpElVal "url")
                           (xpElVal "estimate")
                           (xpElVal "current_state")
                 )
                 (xp5Tuple (xpElVal "description")
                           (xpElVal "name")
                           (xpElVal "requested_by")
                           (xpElVal "created_at")
                           (xpOption (xpElVal "labels"))
                 )

xpIterations :: PU Iterations
xpIterations = xpListOf "iterations"

xpIteration :: PU Iteration
xpIteration = xpElem "iteration" $
              xpWrap ( \(a,b,c,d,e) -> Iteration a b c d e
                     , \itr -> ( itrID itr, itrNumber itr
                               , itrStartDate itr, itrEndDate itr, itrStories itr)
                     ) $ 
              xp5Tuple (xpElVal "id")
                       (xpElVal "number")
                       (xpElVal "start")
                       (xpElVal "finish")
                       xpStories

xpElVal :: String -> PU String
xpElVal t = xpElem t xpText0

xpListOf :: (XmlPickler a) => String -> PU [a]
xpListOf t = xpElem t $ xpList $ xpickle

