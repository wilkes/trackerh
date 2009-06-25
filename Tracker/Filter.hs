module Tracker.Filter
    ( StoryType(..)
    , StoryState(..)
    , SearchTerm(..)
    ) where

import Data.List

data StoryType = Feature
               | Bug
               | Chore
               | Release
                 deriving (Show,Eq)

data StoryState = Unstarted
                | Started
                | Finished
                | Delivered
                | Accepted
                | Rejected
                  deriving (Show,Eq)

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

