module Tetris where

import qualified Data.Set                      as Set
import           System.Random
import           Data.Time.Clock
import           Data.Time.Format

data Tetromino = Tetromino [Block]
  deriving (Eq, Show)

type Location = (Int, Int)

data Block = Block Location Color
  deriving (Eq, Show, Ord)

type Color = String -- TODO: replace with real Color

cyanI :: Tetromino
cyanI = Tetromino
  [ Block (0, 2) "Cyan"
  , Block (1, 2) "Cyan"
  , Block (2, 2) "Cyan"
  , Block (3, 2) "Cyan"
  ]

yellowO :: Tetromino
yellowO = Tetromino
  [ Block (1, 1) "Yellow"
  , Block (2, 1) "Yellow"
  , Block (1, 2) "Yellow"
  , Block (2, 2) "Yellow"
  ]

purpleT :: Tetromino
purpleT = Tetromino
  [ Block (0, 2) "Purple"
  , Block (1, 2) "Purple"
  , Block (2, 2) "Purple"
  , Block (1, 3) "Purple"
  ]

greenS :: Tetromino
greenS = Tetromino
  [ Block (0, 2) "Green"
  , Block (1, 2) "Green"
  , Block (1, 3) "Green"
  , Block (2, 3) "Green"
  ]

redZ :: Tetromino
redZ = Tetromino
  [ Block (0, 3) "Red"
  , Block (1, 3) "Red"
  , Block (1, 2) "Red"
  , Block (2, 2) "Red"
  ]

blueJ :: Tetromino
blueJ = Tetromino
  [ Block (0, 3) "Blue"
  , Block (0, 2) "Blue"
  , Block (1, 2) "Blue"
  , Block (2, 2) "Blue"
  ]

orangeL :: Tetromino
orangeL = Tetromino
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

height :: Int
height = 20

width :: Int
width = 10

startPositionOffset :: Int
startPositionOffset = height - 3

data Direction = DirLeft | DirRight | DirDown



-----------

newBoard :: StdGen -> Board
newBoard gen =
  let (newTetromino, newGen) = randomTetromino gen
      positionedTetromino    = moveTetrominoToStartingPosition newTetromino
  in  Board emptyHeap (GameOn positionedTetromino) newGen

randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen =
  let (randomIx, newGen) = randomR (0, (length tetrominoes) - 1) gen
  in  (tetrominoes !! randomIx, newGen)

moveTetrominoToStartingPosition :: Tetromino -> Tetromino
moveTetrominoToStartingPosition (Tetromino blocks) =
  let movedBlocks =
          (\(Block (x, y) color) -> Block (x, y + startPositionOffset) color)
            <$> blocks
  in  Tetromino movedBlocks

moveTetromino :: Tetromino -> Direction -> Tetromino
moveTetromino (Tetromino blocks) dir =
  let (xOffset, yOffset) = case dir of
        DirLeft  -> (-1, 0)
        DirRight -> (1, 0)
        DirDown  -> (0, -1)
      movedBlocks =
          (\(Block (x, y) color) -> (Block (x + xOffset, y + yOffset) color))
            <$> blocks
  in  Tetromino movedBlocks


getSeedFromCurrentTime :: IO Int
getSeedFromCurrentTime = do
  currentTime <- getCurrentTime
  let seconds = formatTime defaultTimeLocale "%s" currentTime
  return $ read seconds

getGoodStdGen :: IO StdGen
getGoodStdGen = do
  seed <- getSeedFromCurrentTime
  let stdGen = mkStdGen seed
  return stdGen


-- main
main :: IO ()
main = do
  stdGen <- getGoodStdGen
  let b = newBoard stdGen

  putStrLn $ show b
