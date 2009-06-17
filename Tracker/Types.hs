module Tracker.Types where

data Project =
    Project { prjID              :: String
            , prjName            :: String
            , prjIterationLength :: String
            , prjWeekStartDay    :: String
            , prjPointScale      :: String
            }
    deriving (Eq, Show)

data Story = 
    Story { stID           :: String
          , stType         :: String
          , stURL          :: String
          , stEstimate     :: String
          , stCurrentState :: String
          , stDescription  :: String
          , stName         :: String
          , stRequestedBy  :: String
          , stCreatedAt    :: String
          , stLabels       :: String
          }
    deriving (Eq, Show)

data Iteration =
    Iteration { itrID        :: String
              , itrNumber    :: String
              , itrStartDate :: String
              , itrEndDate   :: String
              , itrStories   :: [Story]
              }
    deriving (Eq, Show)