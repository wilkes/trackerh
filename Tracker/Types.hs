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
    Story { stID           :: Maybe String
          , stType         :: Maybe String
          , stURL          :: Maybe String
          , stEstimate     :: Maybe String
          , stCurrentState :: Maybe String
          , stDescription  :: Maybe String
          , stName         :: Maybe String
          , stRequestedBy  :: Maybe String
          , stOwnedBy      :: Maybe String
          , stCreatedAt    :: Maybe String
          , stAcceptedAt   :: Maybe String
          , stIteration    :: Maybe Iteration
          , stLabels       :: Maybe String
          }
    deriving (Eq, Show, Ord)

emptyStory :: Story
emptyStory = Story { stID           = Nothing
                   , stType         = Nothing
                   , stURL          = Nothing
                   , stEstimate     = Nothing
                   , stCurrentState = Nothing
                   , stDescription  = Nothing
                   , stName         = Nothing
                   , stRequestedBy  = Nothing
                   , stOwnedBy      = Nothing
                   , stCreatedAt    = Nothing
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
    deriving (Eq, Show, Ord)

emptyIteration :: Iteration
emptyIteration =
    Iteration { itrID        = Nothing
              , itrNumber    = ""
              , itrStartDate = ""
              , itrEndDate   = ""
              , itrStories   = []
              }

data NamedIteration = Done
                    | Current
                    | Backlog

instance Show NamedIteration where
    show Done    = "done"
    show Current = "current"
    show Backlog = "backlog"

data Note = Note { ntID      :: Maybe String
                 , ntText    :: String
                 , ntAuthor  :: Maybe String
                 , ntNotedAt :: Maybe String
                 }          
          deriving (Eq, Show)

emptyNote :: Note
emptyNote = Note { ntID      = Nothing
                 , ntText    = ""
                 , ntAuthor  = Nothing
                 , ntNotedAt = Nothing
                 }

data Activity = Activity { actID          :: String
                         , actProject     :: String
                         , actStory       :: String
                         , actDescription :: String
                         , actAuthor      :: String
                         , actWhen        :: String
                         }