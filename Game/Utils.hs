-- Game Utils

module Utils where

data Role = Unassigned
          | Civilian
          | Maffia
          deriving (Show, Eq)

data Player = Player { idOf :: Int
                     , nameOf :: String
                     , roleOf :: Role
                     } deriving (Show, Eq)

data State = State { playersOf :: [Player]
                   , playerNrOf :: Int
                   , idNrOf :: Int
                   } deriving (Show)

initialState = State { playersOf = [] 
                     , playerNrOf = 0
                     , idNrOf = 0 }
                     
newPlayer id name = Player { idOf = id
                           , nameOf = name
                           , roleOf = Unassigned }

maffiaFromPlayerNr :: Int -> Int
maffiaFromPlayerNr pn = ceiling (fromIntegral pn / 3)

genList :: Int -> [Role]
genList playerNumber = maffias ++ civils
    where
        maffiaNumber = maffiaFromPlayerNr playerNumber
        civilNumber  = playerNumber - maffiaNumber
        maffias = replicate maffiaNumber Maffia
        civils  = replicate civilNumber Civilian
