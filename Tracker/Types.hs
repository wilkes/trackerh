module Tracker.Types where

import Data.List

data Token = Token { tkGuid :: String
                   , tkID   :: String
                   }
           deriving (Eq, Show, Ord)

type Projects = [Project]
data Project =
    Project { prjID              :: String
            , prjName            :: String
            , prjIterationLength :: String
            , prjWeekStartDay    :: String
            , prjPointScale      :: String
            }
    deriving (Eq, Show, Ord)

type Stories = [Story]
data Story = 
    Story { stID           :: Maybe String
          , stType         :: Maybe StoryType
          , stURL          :: Maybe String
          , stEstimate     :: Maybe String
          , stCurrentState :: Maybe StoryState
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

data StoryState = Unstarted
                | Started
                | Finished
                | Delivered
                | Accepted
                | Rejected
                  deriving (Eq, Ord, Show, Read)

data StoryType = Feature 
               | Bug     
               | Chore   
               | Release 
                 deriving (Eq, Ord, Show, Read)


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

data SearchTerm = Label     String
                | Type      StoryType
                | State     StoryState
                | Requestor String
                | Owner     String
                | MyWork    String
                | StoryID   String
                | Query [SearchTerm]
                  deriving (Eq)

instance Show SearchTerm where
    show (Label     v) = "label:"     ++ quote v
    show (Type      v) = "type:"      ++ show  v 
    show (State     v) = "state:"     ++ show  v 
    show (Requestor v) = "requestor:" ++ quote v 
    show (Owner     v) = "owner:"     ++ quote v 
    show (MyWork    v) = "mywork:"    ++ quote v 
    show (StoryID   v) = "id:"        ++ quote v 
    show (Query     v) = intercalate " " $ map show v

quote :: String -> String
quote s
    | any (== ' ') s = "\"" ++ s ++ "\""
    | otherwise      = s
