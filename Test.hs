module Main where


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Tracker.Types
import Tracker.Xml

main = defaultMain tests

tests = [testCase "test_story_to_record" test_story_to_record]

test_story_to_record = toRecord storyXml @=? storyRecord

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
