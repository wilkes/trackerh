module Tracker.Pickle where

import Text.XML.HXT.Arrow.Pickle
import Tracker.Types

instance XmlPickler Project where
    xpickle = xpProject

instance XmlPickler Story where
    xpickle = xpStory

instance XmlPickler Iteration where
    xpickle = xpIteration

instance XmlPickler Token where
    xpickle = xpToken

xpToken :: PU Token
xpToken = xpElem "token" $
          xpWrap ( uncurry Token
                 , \t -> (tkGuid t, tkID t)
                 ) $
          xpPair (xpElVal "guid")
                 (xpElVal "id")

xpProjects :: PU Projects
xpProjects = xpListOf "projects"

xpProject :: PU Project
xpProject = xpElem "project" $
            xpWrap ( \(pid,name,itlength,day,scale) ->
                         Project pid name itlength day scale
                   , \p -> ( prjID p, prjName p, prjIterationLength p
                           , prjWeekStartDay p, prjPointScale p)
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
          xpWrap ( \ ((f1,f2,f3,f4,f5),(f6,f7,f8,f9,f10),(f11,f12,f13)) ->
                       Story f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13
                 , \st -> ( ( stID st
                            , stType st
                            , stURL st
                            , stEstimate st
                            , stCurrentState st
                            )
                          , ( stDescription st
                            , stName st
                            , stRequestedBy st
                            , stOwnedBy st
                            , stCreatedAt st
                            )
                          , ( stAcceptedAt st
                            , stIteration st
                            , stLabels st
                            )
                          )
                 ) $
          xpTriple (xp5Tuple (xpElVal "id")
                             (xpElVal "story_type")
                             (xpElVal "url")
                             (xpOption (xpElVal "estimate"))
                             (xpElVal "current_state")
                   )
                   (xp5Tuple (xpElVal "description")
                             (xpElVal "name")
                             (xpElVal "requested_by")
                             (xpOption (xpElVal "owned_by"))
                             (xpElVal "created_at")
                   )
                   (xpTriple (xpOption (xpElVal "accepted_at"))
                             (xpOption xpIteration)
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
              xp5Tuple (xpOption (xpElVal "id"))
                       (xpElVal "number")
                       (xpElVal "start")
                       (xpElVal "finish")
                       xpStories

xpElVal :: String -> PU String
xpElVal t = xpElem t xpText0

xpListOf :: (XmlPickler a) => String -> PU [a]
xpListOf t = xpElem t $ xpList $ xpickle

