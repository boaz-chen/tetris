module Tetris where

import qualified Data.Set                      as Set
import           System.Random
import           Data.Time.Clock
import           Data.Time.Format

data Tetromino = Tetromino (Set.Set Block)
  deriving (Eq, Show)

type Location = (Int, Int)

data Block = Block Location Color
  deriving (Eq, Show, Ord)

type Color = String -- TODO: replace with real Color

cyanI :: Tetromino
cyanI = Tetromino $ Set.fromList
  [ Block (0, 2) "Cyan"
  , Block (1, 2) "Cyan"
  , Block (2, 2) "Cyan"
  , Block (3, 2) "Cyan"
  ]

yellowO :: Tetromino
yellowO = Tetromino $ Set.fromList
  [ Block (1, 1) "Yellow"
  , Block (2, 1) "Yellow"
  , Block (1, 2) "Yellow"
  , Block (2, 2) "Yellow"
  ]

purpleT :: Tetromino
purpleT = Tetromino $ Set.fromList
  [ Block (0, 2) "Purple"
  , Block (1, 2) "Purple"
  , Block (2, 2) "Purple"
  , Block (1, 3) "Purple"
  ]

greenS :: Tetromino
greenS = Tetromino $ Set.fromList
  [ Block (0, 2) "Green"
  , Block (1, 2) "Green"
  , Block (1, 3) "Green"
  , Block (2, 3) "Green"
  ]

redZ :: Tetromino
redZ = Tetromino $ Set.fromList
  [ Block (0, 3) "Red"
  , Block (1, 3) "Red"
  , Block (1, 2) "Red"
  , Block (2, 2) "Red"
  ]

blueJ :: Tetromino
blueJ = Tetromino $ Set.fromList
  [ Block (0, 3) "Blue"
  , Block (0, 2) "Blue"
  , Block (1, 2) "Blue"
  , Block (2, 2) "Blue"
  ]

orangeL :: Tetromino
orangeL = Tetromino $ Set.fromList
  [ Block (0, 2) "Orange"
  , Block (1, 2) "Orange"
  , Block (2, 2) "Orange"
  , Block (2, 3) "Orange"
  ]

tetrominoes :: [Tetromino]
tetrominoes = [cyanI, yellowO, purpleT, greenS, redZ, blueJ, orangeL]

type Heap = Set.Set Block

emptyHeap :: Heap
emptyHeap = Set.empty

data BoardState =
    GameOver
  | GameOn Tetromino
  deriving (Show)

data Board = Board Heap BoardState StdGen
  deriving (Show)

newBoard :: StdGen -> Board
newBoard gen =
  let (newTetromino, newGen) = randomTetromino gen
  in Board emptyHeap (GameOn newTetromino) newGen

randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen =
  let (randomIx, newGen) = randomR (0, (length tetrominoes) - 1) gen
  in  (tetrominoes !! randomIx, newGen)














getSeedFromCurrentTime :: IO Int
getSeedFromCurrentTime = do
  currentTime <- getCurrentTime
  let seconds = formatTime defaultTimeLocale "%s" currentTime
  return $ read seconds

-- main
main :: IO ()
main = do
  seed <- getSeedFromCurrentTime
  let stdGen         = mkStdGen seed
  let b = newBoard stdGen

  putStrLn $ show b
