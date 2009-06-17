module Main where


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Tracker.Types
import Tracker.Xml

main = defaultMain tests

tests = [ testGroup "Story"
          [ testCase "test_story_to_record" test_story_to_record
          , testCase "test_stories_to_records" test_stories_to_records]
        , testGroup "Project"
          [ testCase "test_project_to_record" test_project_to_record
          , testCase "test_projects_to_records" test_projects_to_records]
        ]

test_story_to_record = toRecord storyXml @=? storyRecord
test_stories_to_records = toRecords (storiesXml 3) @=? (replicate 3 storyRecord)
test_project_to_record = toRecord projectXml @=? projectRecord
test_projects_to_records = toRecords (projectsXml 3) @=? (replicate 3 projectRecord)


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
                    , stLabels       = ""
                    }

storiesXml n = "<stories type=\"array\">" ++ stories ++ "</stories>"
    where stories = foldl (++) "" $ replicate n storyXml


projectXml = "<project>\
  \<id>18898</id>\
  \<name>TrackerH</name>\
  \<iteration_length type=\"integer\">1</iteration_length>\
  \<week_start_day>Sunday</week_start_day>\
  \<point_scale>0,1,2,3,5,8</point_scale>\
\</project>"

projectsXml n = "<projects type=\"array\">" ++ projects ++ "</projects>"
    where projects = foldl (++) "" $ replicate n projectXml

projectRecord = Project { prjID              = "18898"
                        , prjName            = "TrackerH"
                        , prjIterationLength = "1"
                        , prjWeekStartDay    = "Sunday"
                        , prjPointScale      = "0,1,2,3,5,8"
                        }