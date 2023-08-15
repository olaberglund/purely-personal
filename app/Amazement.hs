module Amazement where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Data.Function (on)
import Data.List
  ( group,
    groupBy,
    iterate',
    sort,
    sortBy,
    sortOn,
    (\\),
  )
import Data.List.Split (chunksOf)
import qualified Data.Ord as Ord (Down (Down))
import Data.Sequence (iterateN, mapWithIndex)
import Data.Set (Set, insert, notMember)
import qualified Data.Set as S
import Debug.Trace (trace)
import System.Random (StdGen)
import qualified System.Random as R
import Prelude hiding (Left, Right)

type Location = (Int, Int)

data MazeState = MazeState
  { maze :: Maze,
    location :: Location,
    visited :: Set Location,
    gen :: StdGen
  }

type Maze = Graph Location

width = height

height = 16

initialMaze :: Int -> MazeState
initialMaze n =
  let startLocation = (0, 0)
   in MazeState
        { maze = G.empty,
          location = startLocation,
          visited = S.singleton startLocation,
          gen = R.mkStdGen n
        }

create :: Int -> IO ()
create = putStr . showMaze . generateMaze

generateMaze :: Int -> Maze
generateMaze = maze . iterateUntil haveVisitedAll aldousStep . initialMaze

haveVisitedAll :: MazeState -> Bool
haveVisitedAll = (width * height ==) . S.size . visited

wall = "██"

space = "  "

blockSize = length space

data Direction = N | E | S | W deriving (Show, Eq, Ord)

dir :: Location -> Location -> Direction
dir (x, y) (x', y') =
  case (x' - x, y' - y) of
    (0, -1) -> N
    (1, 0) -> E
    (0, 1) -> S
    (-1, 0) -> W

showBlockTop :: (Location, [Location]) -> String
showBlockTop = showWalls . walls
  where
    showWalls [] = wall <> space
    showWalls [N] = wall <> wall
    showWalls [W] = wall <> space
    showWalls [N, W] = wall <> wall

showBlockBottom :: (Location, [Location]) -> String
showBlockBottom = showWalls . walls
  where
    showWalls [] = space <> space
    showWalls [N] = space <> space
    showWalls [W] = wall <> space
    showWalls [N, W] = wall <> space

walls :: (Location, [Location]) -> [Direction]
walls (loc, neighbors) = [N, W] \\ map (dir loc) neighbors

showMaze :: Maze -> String
showMaze = unlines . appendFloor . map showBlockRow . mkBlocks . sortOn y . G.adjacencyList
  where
    showRow showF = (<> wall) . concatMap showF
    bottom = showRow showBlockBottom
    top = (<> wall) . concatMap showBlockTop
    showBlockRow r = top r <> "\n" <> bottom r
    mkBlocks = chunksOf width
    y = snd . fst

appendFloor :: [String] -> [String]
appendFloor = (++ [concat $ wall : replicate (blockSize * width) wall])

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = head . filter p . iterate' f

aldousStep :: MazeState -> MazeState
aldousStep m =
  if notMember loc' (visited m)
    then
      m
        { gen = gen',
          maze = overlay (edge (location m) loc') (maze m),
          location = loc',
          visited = insert loc' (visited m)
        }
    else m {gen = gen', location = loc'}
  where
    (loc', gen') = findLegalMove (gen m) (location m)

findLegalMove :: StdGen -> Location -> (Location, StdGen)
findLegalMove gen loc =
  let (dir, gen') = randomMovement gen
      loc' = move dir loc
   in if withinBounds loc' then (loc', gen') else findLegalMove gen' loc
  where
    withinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

randomMovement :: StdGen -> (Movement, StdGen)
randomMovement = first toEnum . R.randomR (0, 3)

data Movement = Up | Right | Down | Left deriving (Enum)

move :: Movement -> Location -> Location
move Up = second succ
move Right = first succ
move Down = second pred
move Left = first pred
