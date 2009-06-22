module Tracker.Pickle where

import Text.XML.HXT.Arrow.Pickle
import Tracker.Types

instance XmlPickler Project where
    xpickle = xpProject

instance XmlPickler Story where
    xpickle = xpStory

instance XmlPickler Iteration where
    xpickle = xpIteration

xpProject = undefined
xpIteration = undefined


xpElVal t = xpElem t xpText0

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

