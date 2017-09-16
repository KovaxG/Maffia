-- Game Utils

data Role = Civilian 
          | Maffia 
          deriving (Show, Eq)

data Player = Player { id :: Int
                     , name :: String
                     , role :: Role
                     } deriving (Show, Eq)

data State = State { players :: [Player]
                   } deriving (Show)
                   
maffiaFromPlayerNr :: Int -> Int
maffiaFromPlayerNr pn = ceiling (fromIntegral pn / 3)

genList :: Int -> [Role] 
genList playerNumber = maffias ++ civils
    where
        maffiaNumber   = maffiaFromPlayerNr playerNumber
        civilNumber = playerNumber - maffiaNumber
        maffias = replicate maffiaNumber Maffia
        civils  = replicate civilNumber Civilian