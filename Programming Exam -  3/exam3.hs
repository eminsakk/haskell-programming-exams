module PE3 where

data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Read, Show)

isInGrid :: Grid -> Coordinate -> Bool
isInGrid _ coor
    | fst coor < 0 || snd coor < 0 = False
isInGrid grid coor = if snd coor < (length grid) && fst coor < (length (grid !! 0)) then True else False


totalCount :: Grid -> Int
rockValue (Rock x) = x
rockValue _ = 0

cellLstTraverser:: [Cell] ->Int
cellLstTraverser cellLst
    | cellLst == [] = 0
    | otherwise  = rockValue (head cellLst) + cellLstTraverser (tail cellLst)
totalCount grid 
    | grid == [] = 0
    | otherwise = (cellLstTraverser (head grid)) + (totalCount (tail grid))


coordinatesOfPits :: Grid -> [Coordinate]

isPit::Cell ->Bool
isPit Pit = True
isPit _ = False
coordinatesOfPits grid = [(x,y) | x <- [0.. ((length (head grid)) - 1)] , y <- [0..((length grid) - 1)], (isPit ((grid !! y) !! x)) == True,(isInGrid grid (x,y)) == True] 


tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePathHelper::Grid ->Int ->Int ->Int-> Int -> Int->[Move]-> [Coordinate]

tracePathHelper _ _ _ _ _ _ [] = []
tracePathHelper grid xCoordinate yCoordinate robotCapacity robotEnergy robotStorage moves
  
    | (head moves) == West = if ((isInGrid grid (xCoordinate - 1, yCoordinate)) && (robotEnergy - 1 >= 0) ) && (not (elem (xCoordinate,yCoordinate) (coordinatesOfPits grid)))then (xCoordinate - 1, yCoordinate):tracePathHelper grid (xCoordinate-1) yCoordinate robotCapacity (robotEnergy-1) robotStorage (tail moves)
                             else (xCoordinate, yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity (robotEnergy-1) robotStorage (tail moves)

    | (head moves) == East = if ((isInGrid grid (xCoordinate + 1, yCoordinate)) && (robotEnergy - 1 >= 0)) && (not (elem (xCoordinate,yCoordinate) (coordinatesOfPits grid))) then  (xCoordinate + 1, yCoordinate):tracePathHelper grid (xCoordinate+1) yCoordinate robotCapacity (robotEnergy-1) robotStorage (tail moves)
                             else (xCoordinate, yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity (robotEnergy-1) robotStorage (tail moves)

    | (head moves) == North = if ((isInGrid grid (xCoordinate, yCoordinate - 1)) && (robotEnergy - 1 >= 0)) && (not (elem (xCoordinate,yCoordinate) (coordinatesOfPits grid))) then (xCoordinate, yCoordinate - 1):tracePathHelper grid xCoordinate (yCoordinate-1) robotCapacity (robotEnergy-1) robotStorage (tail moves) 
                              else (xCoordinate, yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity (robotEnergy-1) robotStorage (tail moves)

    | (head moves) == South = if ((isInGrid grid (xCoordinate, yCoordinate + 1)) && (robotEnergy - 1 >= 0)) && (not (elem (xCoordinate,yCoordinate) (coordinatesOfPits grid))) then (xCoordinate, yCoordinate + 1):tracePathHelper grid xCoordinate (yCoordinate+1) robotCapacity (robotEnergy-1) robotStorage (tail moves) 
                              else (xCoordinate, yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity (robotEnergy-1) robotStorage (tail moves)

    |((head moves) == PickUp) = if(robotEnergy - 5 >= 0) then (xCoordinate,yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity (robotEnergy-5) robotStorage (tail moves)
                                else (xCoordinate,yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity 0 robotStorage (tail moves)

    |((head moves) == PutDown) = if(robotEnergy - 3 >= 0) then (xCoordinate,yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity (robotEnergy-5) robotStorage (tail moves)
                                 else (xCoordinate,yCoordinate):tracePathHelper grid xCoordinate yCoordinate robotCapacity 0 robotStorage (tail moves)
    

tracePath grid robot moves = tracePathHelper grid (fst (location robot)) (snd (location robot)) (capacity robot) (energy robot) (storage robot) moves




energiseRobots :: Grid -> [Robot] -> [Robot]

isCraft::Cell ->Bool
isCraft (SpaceCraft x) = True
isCraft _ = False

finderIndex::Grid -> [Coordinate]
finderIndex grid = [(x,y) | x <- [0.. ((length (head grid)) - 1)] , y <- [0..((length grid) - 1)], (isCraft ((grid !! y) !! x)) == True]

manhattanDist::Int -> Int ->Int -> Int-> Int
manhattanDist x y z t = (max 0 (100 - ((abs (x - z)) + abs (y - t)) * 20))

robotGiver::Grid -> Robot -> Robot

robotGiver grid tmpRbt = Robot { name = name tmpRbt,
                                                      location = location tmpRbt,
                                                      capacity = capacity tmpRbt,
                                                      energy = if (manhattanDist (fst (location tmpRbt)) (snd (location tmpRbt)) (fst (head (finderIndex grid))) (snd (head (finderIndex grid)))) + (energy tmpRbt) >= 100 then 100 else (manhattanDist (fst (location tmpRbt)) (snd (location tmpRbt)) (fst (head (finderIndex grid))) (snd (head (finderIndex grid)))) + (energy tmpRbt),
                                                      storage = storage tmpRbt }


energiseRobots grid [] = []
energiseRobots grid robotList = [robotGiver grid (head robotList)] ++ (energiseRobots grid (tail robotList))



