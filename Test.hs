module Main where

import Text.XML.HXT.Arrow

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Tracker.Types
import Tracker.Xml
import Tracker.Pickle

main = defaultMain tests

tests = [ testGroup "Story"
          [ testCase "test_story_to_record" test_story_to_record
          , testCase "test_stories_to_records" test_stories_to_records
          , testCase "test_story_unpickle" test_story_unpickle
          , testCase "test_stories_unpickle" test_stories_unpickle
          ]
        , testGroup "Project"
          [ testCase "test_project_to_record" test_project_to_record
          , testCase "test_projects_to_records" test_projects_to_records
          , testCase "test_project_unpickle" test_project_unpickle
          , testCase "test_projects_unpickle" test_projects_unpickle
          ]
        , testGroup "Iterations"
          [ testCase "test_iterations_to_records" test_iterations_to_records
          , testCase "test_iterations_unpickle" test_iterations_unpickle
          ]
        ]

test_story_to_record    = toRecord storyXml @=? storyRecord
test_stories_to_records = toRecords (storiesXml 3) @=? (replicate 3 storyRecord)

test_story_unpickle = do
  st <- runUnpickle storyXml xpStory
  1 @=? (length st)
  storyRecord @=? (head st)

test_stories_unpickle = do
  (stories:[]) <- runUnpickle (storiesXml 3) xpStories
  3 @=? (length stories)
  (replicate 3 storyRecord) @=? stories

test_project_unpickle = do
  (project:[]) <- runUnpickle projectXml xpProject
  projectRecord @=? project

test_projects_unpickle = do
  (projects:[]) <- runUnpickle (projectsXml 3) xpProjects
  3 @=? (length projects)
  (replicate 3 projectRecord) @=? projects

test_iterations_unpickle = do
  (iterations:[]) <- runUnpickle (iterationsXml 2 3) xpIterations
  (replicate 2 $ iterationRecord 3) @=? iterations

test_project_to_record   = toRecord projectXml @=? projectRecord
test_projects_to_records = toRecords (projectsXml 3) @=? (replicate 3 projectRecord)

test_iterations_to_records = toRecords (iterationsXml 2 3) @=? (replicate 2 $ iterationRecord 3)



runUnpickle :: String -> PU a -> IO [a]
runUnpickle xml pickler = runX $ readString [] xml >>> xunpickleVal pickler

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

storyRecord = Story { stID           = "804610"
                    , stType         = "feature"
                    , stURL          = "http://www.pivotaltracker.com/story/show/804610"
                    , stEstimate     = "2"
                    , stCurrentState = "unstarted"
                    , stDescription  = ""
                    , stName         = "Add support for ssl or not"
                    , stRequestedBy  = "Wilkes Joiner"
                    , stCreatedAt    = "2009/06/14 14:08:45 GMT"
                    , stLabels       = Nothing
                    }

storiesXml n = "<stories type=\"array\">" ++ stories ++ "</stories>"
    where stories = concat $ replicate n storyXml


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

iterationRecord m = Iteration { itrID            = "1"
                              , itrNumber        = "1"
                              , itrStartDate     = "2009/06/14 00:00:00 UTC"
                              , itrEndDate       = "2009/06/21 00:00:00 UTC"
                              , itrStories       = replicate m storyRecord
                              }

