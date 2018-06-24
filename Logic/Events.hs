module Events where

import Common

data Event = AddPlayer PlayerName
           | StartGame Salt
           | EndDay
           | EndNight
           | QueryRole PlayerName
           | MaffiaHit PlayerName
           deriving (Show, Read)
