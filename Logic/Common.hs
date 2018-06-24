module Common where

import Data.Sort
import Data.List

type PlayerName = String
type Salt = Int

data Player = Player {
  pName :: PlayerName,
  pRole :: Role,
  pEffect :: [Effect]
} deriving (Show, Eq)

data Role = Maffia | Civilian deriving (Show, Eq)

data Effect = MaffiaTarget deriving (Show, Eq)

generateRoles :: Salt -> Int -> [Role]
generateRoles salt nr = map snd sorted
  where evilRoles = round $ fromIntegral nr * 0.4
        goodRoles = nr - evilRoles
        roles = (replicate evilRoles Maffia) ++ (replicate goodRoles Civilian)
        hashed = zip (hash salt <$> [1 ..]) roles
        hash :: Int -> Int -> Int
        hash mag x = round $ sin (fromIntegral x * fromIntegral mag) * 100
        sorted = sortOn fst hashed

replacePlayer :: Player -> [Player] -> [Player]
replacePlayer player players =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName (pName player) players
    removeOldPlayer player  = filterNot (== player)
    playerNotFound = players
    playerFound oldPlayer = player : removeOldPlayer oldPlayer players

findPlayerByName :: PlayerName -> [Player] -> Maybe Player
findPlayerByName name players = find (\p -> pName p == name) players

removeMaffiaHits :: Player -> Player
removeMaffiaHits player = player { pEffect = noMaffiaHits }
  where noMaffiaHits = filterNot (== MaffiaTarget) (pEffect player)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

maffiaWin :: [Player] -> Bool
maffiaWin players = maffiaNr >= townNr
  where maffiaNr = count (\p -> pRole p == Maffia) players
        townNr = length players - maffiaNr
