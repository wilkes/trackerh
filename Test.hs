module Main where

import Text.XML.HXT.Arrow

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Tracker.Types
import Tracker.Pickle

main = defaultMain tests

tests = [ testGroup "Story"
          [ testCase "test_story_unpickle" test_story_unpickle
          , testCase "test_stories_unpickle" test_stories_unpickle
          , testCase "test_note_unpickle" test_note_unpickle
          ]
        , testGroup "Project"
          [ testCase "test_project_unpickle" test_project_unpickle
          , testCase "test_projects_unpickle" test_projects_unpickle
          ]
        , testGroup "Iterations"
          [ testCase "test_iterations_unpickle" test_iterations_unpickle
          ]
        ]

test_story_unpickle = do
  [st] <- runUnpickle xpStory storyXml
  storyRecord @=? st

test_stories_unpickle = do
  [stories] <- runUnpickle xpStories (storiesXml 3)
  (replicate 3 storyRecord) @=? stories


test_note_unpickle = do 
  [note] <- runUnpickle xpNote noteXml
  noteRecord @=? note
             
test_project_unpickle = do
  [project] <- runUnpickle xpProject projectXml
  projectRecord @=? project

test_projects_unpickle = do
  [projects] <- runUnpickle xpProjects (projectsXml 3)
  (replicate 3 projectRecord) @=? projects

test_iterations_unpickle = do
  [iterations] <- runUnpickle xpIterations (iterationsXml 2 3) 
  (replicate 2 $ iterationRecord 3) @=? iterations

storyXml = "<story>\
        \<id type=\"integer\">804610</id>\
        \<story_type>feature</story_type>\
        \<url>http://www.pivotaltracker.com/story/show/804610</url>\
        \<estimate type=\"integer\">2</estimate>\
        \<current_state>unstarted</current_state>\
        \<description></description>\
        \<name>Add support for ssl or not</name>\
        \<requested_by>Wilkes Joiner</requested_by>\
        \<created_at type=\"datetime\">2009/06/14 14:08:45 GMT</created_at>\
      \</story>"

storyRecord = emptyStory { stID           = Just "804610"
                         , stType         = Just Feature
                         , stURL          = Just "http://www.pivotaltracker.com/story/show/804610"
                         , stEstimate     = Just "2"
                         , stCurrentState = Just Unstarted
                         , stDescription  = Just ""
                         , stName         = Just "Add support for ssl or not"
                         , stRequestedBy  = Just "Wilkes Joiner"
                         , stCreatedAt    = Just "2009/06/14 14:08:45 GMT"
                         , stLabels       = Nothing
                         , stIteration    = Nothing
                         }

storiesXml n = "<stories type=\"array\">" ++ stories ++ "</stories>"
    where stories = concat $ replicate n storyXml


xmlPI = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
projectXml = "<project>\
  \<id>18898</id>\
  \<name>TrackerH</name>\
  \<iteration_length type=\"integer\">1</iteration_length>\
  \<week_start_day>Sunday</week_start_day>\
  \<point_scale>0,1,2,3,5,8</point_scale>\
\</project>"

projectsXml n = "<projects type=\"array\">" ++ projects ++ "</projects>"
    where projects = concat $ replicate n projectXml

projectRecord = Project { prjID              = "18898"
                        , prjName            = "TrackerH"
                        , prjIterationLength = "1"
                        , prjWeekStartDay    = "Sunday"
                        , prjPointScale      = "0,1,2,3,5,8"
                        }
 
iterationsXml n m = "<iterations type=\"array\">" ++ iterations ++ "</iterations>"
   where iterations = concat $ replicate n iteration
         iteration = "<iteration>\
                     \<id type=\"integer\">1</id>\
                     \<number type=\"integer\">1</number>\
                     \<start type=\"datetime\">2009/06/14 00:00:00 UTC</start>\
                     \<finish type=\"datetime\">2009/06/21 00:00:00 UTC</finish>" ++
                     storiesXml m ++
                     "</iteration>"

iterationRecord m = emptyIteration { itrID            = Just "1"
                                   , itrNumber        = "1"
                                   , itrStartDate     = "2009/06/14 00:00:00 UTC"
                                   , itrEndDate       = "2009/06/21 00:00:00 UTC"
                                   , itrStories       = replicate m storyRecord
                                   }

noteXml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<note>\
    \  <id type=\"integer\">1</id>\
    \  <text>new note via API</text>\
    \  <author>Spock (young)</author>\
    \  <noted_at type=\"datetime\">2009/01/16 18:53:51 UTC</noted_at>\
    \</note>"

noteRecord = Note { ntID = Just "1"
                  , ntText = "new note via API"
                  , ntAuthor = Just "Spock (young)"
                  , ntNotedAt = Just "2009/01/16 18:53:51 UTC"
                  }