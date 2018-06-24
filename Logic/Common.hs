module Common where

import Data.Sort
import Data.List

type PlayerName = String
type Salt = Int

data Player = Player {
  pName :: PlayerName,
  pRole :: Role,
  pEffects :: [Effect]
} deriving (Eq)

instance Show Player where
  show player =
    mconcat ["(",
      show $ pName player, ": ",
      show $ pRole player, "; ",
      show $ pEffects player,
      ")"]

data Role = Maffia
          | Civilian
          | Doctor
          | Detective
          deriving (Show, Eq)

isMaffia :: Role -> Bool
isMaffia role = case role of
  Maffia -> True
  _ -> False

isTown :: Role -> Bool
isTown = not . isMaffia

data Effect = MaffiaTarget
            | DoctorTarget
            | Investigated
            | VotedBy PlayerName
            | Lynched
            deriving (Show, Eq)

generateRoles :: Salt -> Int -> [Role]
generateRoles salt nr = map snd sorted
  where
    maffiaRoles = round $ fromIntegral nr * 0.25
    doctorRoles = 1
    detectiveRoles = 1
    civilianRoles =
      nr - maffiaRoles - doctorRoles - detectiveRoles
    roles =
      (replicate maffiaRoles Maffia) ++
      (replicate doctorRoles Doctor) ++
      (replicate detectiveRoles Detective) ++
      (replicate civilianRoles Civilian)
    hashed = zip (hash salt <$> [1 ..]) roles
    hash :: Int -> Int -> Int
    hash salt x = round $ sin (fromIntegral x * fromIntegral salt) * 100
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

removeEffect :: Effect -> Player -> Player
removeEffect effect player = player { pEffects = noEffects }
  where
    noEffects = filterNot (== effect) (pEffects player)

addEffect :: Effect -> Player -> Player
addEffect effect player = player {
  pEffects = effect : pEffects player
}

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

maffiaWin :: [Player] -> Bool
maffiaWin players = maffiaNr >= townNr
  where maffiaNr = count (\p -> pRole p == Maffia) players
        townNr = length players - maffiaNr

(+:) :: [a] -> a -> [a]
as +: a = as ++ [a]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead as = Just $ head as

safeRead :: (Read a) => String -> Maybe a
safeRead s = fst <$> (safeHead $ reads s)
