-- Game Utils

module Utils where

import Text.Read (readMaybe)

data Role = Unassigned
          | Civilian
          | Maffia
          deriving (Show, Eq)

data Player = Player { idOf :: Int
                     , nameOf :: String
                     , roleOf :: Role
                     , readinessOf :: Bool
                     } deriving (Show, Eq)

data State = State { playersOf :: [Player]
                   , lobbyChatOf :: [String]
                   , idNrOf :: Int
                   } deriving (Show)

initialState = State { playersOf = [] 
                     , lobbyChatOf = []
                     , idNrOf = 0 }
                     
newPlayer id name = Player { idOf = id
                           , nameOf = name
                           , roleOf = Unassigned 
                           , readinessOf = False }

maffiaFromPlayerNr :: Int -> Int
maffiaFromPlayerNr pn = ceiling (fromIntegral pn / 3)

genList :: Int -> [Role]
genList playerNumber = maffias ++ civils
    where
        maffiaNumber = maffiaFromPlayerNr playerNumber
        civilNumber  = playerNumber - maffiaNumber
        maffias = replicate maffiaNumber Maffia
        civils  = replicate civilNumber Civilian


data Message = Say String
             | GetMessages Int
             | Rename String
             | Unvote String
             | Vote String
             | Unready
             | Ready
             deriving (Show, Read, Eq)
                    
                    
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map $ \a -> if p(a) == True then f(a) else a