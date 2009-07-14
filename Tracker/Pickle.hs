module Tracker.Pickle where

import Text.XML.HXT.Arrow
--import Text.XML.HXT.Arrow.Pickle
import Tracker.Types
import Data.Char(toLower,toUpper)

instance XmlPickler Project where
    xpickle = xpProject

instance XmlPickler Story where
    xpickle = xpStory

instance XmlPickler Iteration where
    xpickle = xpIteration

instance XmlPickler Token where
    xpickle = xpToken

instance XmlPickler Note where
    xpickle = xpNote

instance XmlPickler Activity where
    xpickle = xpActivity

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
          xpTriple (xp5Tuple (xpOption $ xpElVal "id")
                             (xpOption xpStoryType)
                             (xpOption (xpElVal "url"))
                             (xpOption (xpElVal "estimate"))
                             (xpOption xpStoryState)
                   )
                   (xp5Tuple (xpOption $ xpElVal "description")
                             (xpOption $ xpElVal "name")
                             (xpOption $ xpElVal "requested_by")
                             (xpOption $ xpElVal "owned_by")
                             (xpOption $ xpElVal "created_at")
                   )
                   (xpTriple (xpOption $ xpElVal "accepted_at")
                             (xpOption   xpIteration)
                             (xpOption $ xpElVal "labels")
                   )
          

xpStoryState :: PU StoryState
xpStoryState = xpEnumerated "current_state"

xpStoryType :: PU StoryType
xpStoryType = xpEnumerated "story_type"

xpEnumerated :: (Read a, Show a) => String -> PU a
xpEnumerated tag = xpElem tag $
                   xpWrap (read . tcase, (map toLower) . show)
                   $ xpText0
    where tcase "" = ""
          tcase (h:rest) = (toUpper h):rest

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

xpNote :: PU Note
xpNote = xpElem "note" $
         xpWrap ( \(a,b,c,d) -> Note a b c d
                , \n -> (ntID n, ntText n, ntAuthor n, ntNotedAt n)
                ) $
         xp4Tuple (xpOption (xpElVal "id"))
                  (xpElVal "text")
                  (xpOption (xpElVal "author"))
                  (xpOption (xpElVal "noted_at"))

xpActivities :: PU [Activity]
xpActivities = xpListOf "activities"

xpActivity :: PU Activity
xpActivity = xpElem "activity" $
             xpWrap ( \((a,b,c),(d,e,f)) -> Activity a b c d e f
                    , \act -> ( (actID act, actProject act, actStory act)
                             , (actDescription act, actAuthor act, actWhen act))
                    ) $
             xpPair (xpTriple (xpElVal "id")
                               (xpElVal "project")
                               (xpElVal "story")
                     )
                     (xpTriple (xpElVal "description")
                               (xpElVal "author")
                               (xpElVal "when")
                     )
                     

xpElVal :: String -> PU String
xpElVal t = xpElem t xpText0

xpListOf :: (XmlPickler a) => String -> PU [a]
xpListOf t = xpElem t $ xpList xpickle

runUnpickle :: (XmlPickler a) => PU a -> String -> IO [a]
runUnpickle xp xml = runX $ readString options xml >>> xunpickleVal xp
    where options = [ (a_validate,v_0)
		    , (a_remove_whitespace,v_1)
                    -- , (a_trace,v_1)
		    , (a_preserve_comment, v_0)
		    ]

runPickle :: (XmlPickler a) => PU a -> a -> IO [String]
runPickle xp rec = runX $ constA rec >>>
                          xpickleVal xp >>>
                          writeDocumentToString []
