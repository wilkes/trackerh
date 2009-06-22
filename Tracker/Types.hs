module Tracker.Types where

type Projects = [Project]
data Project =
    Project { prjID              :: String
            , prjName            :: String
            , prjIterationLength :: String
            , prjWeekStartDay    :: String
            , prjPointScale      :: String
            }
    deriving (Eq, Show)


type Stories = [Story]
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
          , stLabels       :: Maybe String
          }
    deriving (Eq, Show)


type Iterations = [Iteration]
data Iteration =
    Iteration { itrID        :: String
              , itrNumber    :: String
              , itrStartDate :: String
              , itrEndDate   :: String
              , itrStories   :: Stories
              }
    deriving (Eq, Show)
