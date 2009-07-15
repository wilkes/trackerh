module Tracker.Types where

import Data.List
import Data.Time
import System.Locale

data Token = Token { tkGuid :: String
                   , tkID   :: String
                   }
           deriving (Eq, Show, Ord)

type Projects = [Project]
data Project =
    Project { prjID              :: String
            , prjName            :: String
            , prjIterationLength :: Int
            , prjWeekStartDay    :: String
            , prjPointScale      :: PointScale
            }
    deriving (Eq, Show, Ord)

data PointScale = PointScale [Int] deriving (Eq, Ord)

instance Read PointScale where
    readsPrec _ = readParen False (\s -> [(PointScale (read $ "[" ++ s ++ "]"), "")])

instance Show PointScale where
    show (PointScale s) = filter (\c -> all (/=c) "[]") $ show s
        

type Stories = [Story]
data Story = 
    Story { stID           :: Maybe String
          , stType         :: Maybe StoryType
          , stURL          :: Maybe String
          , stEstimate     :: Maybe Int
          , stCurrentState :: Maybe StoryState
          , stDescription  :: Maybe String
          , stName         :: Maybe String
          , stRequestedBy  :: Maybe String
          , stOwnedBy      :: Maybe String
          , stCreatedAt    :: Maybe TrackerTime
          , stAcceptedAt   :: Maybe TrackerTime
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
              , itrNumber    :: Int
              , itrStartDate :: Maybe TrackerTime
              , itrEndDate   :: Maybe TrackerTime
              , itrStories   :: Stories
              }
    deriving (Eq, Show, Ord)

emptyIteration :: Iteration
emptyIteration =
    Iteration { itrID        = Nothing
              , itrNumber    = 0
              , itrStartDate = Nothing
              , itrEndDate   = Nothing
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
                 , ntNotedAt :: Maybe TrackerTime
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


data TrackerTime = TrackerTime ZonedTime

instance Eq TrackerTime where
    (TrackerTime t1) == (TrackerTime t2) = t1' == t2'
        where t1' = zonedTimeToUTC t1
              t2' = zonedTimeToUTC t2

instance Ord TrackerTime where
    compare (TrackerTime t1) (TrackerTime t2) = compare t1' t2'
        where t1' = zonedTimeToUTC t1
              t2' = zonedTimeToUTC t2

instance ParseTime TrackerTime where
    buildTime locale vs = TrackerTime $ ((buildTime locale vs)::ZonedTime)

instance Read TrackerTime where
    readsPrec _ = readParen False $ readsTime defaultTimeLocale datetimeFormat

instance Show TrackerTime where
    show (TrackerTime zt) = formatTime defaultTimeLocale datetimeFormat zt

datetimeFormat :: String
datetimeFormat = "%Y/%m/%d %X %Z"
