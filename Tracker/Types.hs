module Tracker.Types where

data Token = Token { tkGuid :: String
                   , tkID   :: String
                   }
           deriving (Eq, Show)

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
          , stEstimate     :: Maybe String
          , stCurrentState :: String
          , stDescription  :: String
          , stName         :: String
          , stRequestedBy  :: String
          , stOwnedBy      :: Maybe String
          , stCreatedAt    :: String
          , stAcceptedAt   :: Maybe String
          , stIteration    :: Maybe Iteration
          , stLabels       :: Maybe String
          }
    deriving (Eq, Show)

emptyStory :: Story
emptyStory = Story { stID           = ""
                   , stType         = ""
                   , stURL          = ""
                   , stEstimate     = Nothing
                   , stCurrentState = ""
                   , stDescription  = ""
                   , stName         = ""
                   , stRequestedBy  = ""
                   , stOwnedBy      = Nothing
                   , stCreatedAt    = ""
                   , stAcceptedAt   = Nothing
                   , stIteration    = Nothing
                   , stLabels       = Nothing
                   }

type Iterations = [Iteration]
data Iteration =
    Iteration { itrID        :: Maybe String
              , itrNumber    :: String
              , itrStartDate :: String
              , itrEndDate   :: String
              , itrStories   :: Stories
              }
    deriving (Eq, Show)

emptyIteration :: Iteration
emptyIteration =
    Iteration { itrID        = Nothing
              , itrNumber    = ""
              , itrStartDate = ""
              , itrEndDate   = ""
              , itrStories   = []
              }

