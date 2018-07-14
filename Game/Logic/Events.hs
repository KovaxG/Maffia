module Logic.Events where

import Logic.Common

data Event = AddPlayer PlayerName
           | StartGame Salt
           | EndDay
           | EndNight
           | QueryRole PlayerName
           | MaffiaHit PlayerName
           | DoctorSave PlayerName
           | Investigate PlayerName
           | Vote PlayerName PlayerName
           | CancelVote PlayerName
           deriving (Show, Read)
