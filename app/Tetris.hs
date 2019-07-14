module Tetris where

import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           System.Random
import           Data.Time.Clock
import           Data.Time.Format
import qualified System.Console.ANSI           as Console

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

type Heap = Map.Map Location Block

emptyHeap :: Heap
emptyHeap = Map.empty

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
      positionedTetromino    = moveToStartingPosition newTetromino
  in  Board emptyHeap (GameOn positionedTetromino) newGen

randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen =
  let (randomIx, newGen) = randomR (0, (length tetrominoes) - 1) gen
  in  (tetrominoes !! randomIx, newGen)

moveToStartingPosition :: Tetromino -> Tetromino
moveToStartingPosition (Tetromino blocks) =
  let movedBlocks =
          (\(Block (x, y) color) -> Block (x, y + startPositionOffset) color)
            <$> blocks
  in  Tetromino movedBlocks

offsetTetromino :: Tetromino -> Direction -> Tetromino
offsetTetromino (Tetromino blocks) dir =
  let (xOffset, yOffset) = case dir of
        DirLeft  -> (-1, 0)
        DirRight -> (1, 0)
        DirDown  -> (0, -1)
      movedBlocks =
          (\(Block (x, y) color) -> (Block (x + xOffset, y + yOffset) color))
            <$> blocks
  in  Tetromino movedBlocks

moveTetromino :: Board -> Direction -> Board
moveTetromino board@(Board _ GameOver _) _ = board
moveTetromino board@(Board heap (GameOn tetromino) gen) dir =
  let movedTetromino@(Tetromino blocks) = offsetTetromino tetromino dir
      outOfBounds = List.any (\(Block (x, _) _) -> x < 0 || x >= width) blocks
      collidesWithHeap =
          List.any (\(Block location _) -> Map.member location heap) blocks
      landed = List.any (\(Block (_, y) _) -> y == 0) blocks
  in  if outOfBounds
        then board
        else if landed || collidesWithHeap
          then
            let (newTetromino, newGen) = randomTetromino gen
            in  (Board (addToHeap tetromino heap)
                       (GameOn $ moveToStartingPosition newTetromino)
                       newGen
                )
          else (Board heap (GameOn movedTetromino) gen)

addToHeap :: Tetromino -> Heap -> Heap
addToHeap (Tetromino blocks) heap = List.foldl'
  (\acc block@(Block location _) -> Map.insert location block acc)
  heap
  blocks

handleBoard :: Board -> Board
handleBoard b@(Board _    GameOver _  ) = b
handleBoard (  Board heap state    gen) = undefined


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



-- main --
log' s = putStrLn $ ">>> " <> s

main :: IO ()
-- main = do
--   stdGen <- getGoodStdGen
--   let b = newBoard stdGen
--
--   log' $ "Initial board: " <> show b
--
--   let b' = moveTetromino b DirDown

--  log' $ "Move down: " <> show b'
main = do
  gen <- getGoodStdGen
  loop $ (Board emptyHeap GameOver gen)
  putStrLn "\nGoodbye"

log'' s = s <> "\n"

loop :: Board -> IO Board
loop board@(Board _ GameOver gen) = do
  log' "Game over. n to start a new game or q to quit."
  c <- getChar

  case c of
    'n' -> getGoodStdGen >>= (\newGen -> loop $ newBoard newGen)
    'q' -> return board
    _   -> loop board

loop board@(Board heap (GameOn tetromino@(Tetromino blocks)) gen) = do
  -- putStrLn $ show board
  render board
  log' "r for right. l for left. d for down. q to quit. n to start a new game."
  c <- getChar

  case c of
    'n' -> getGoodStdGen >>= (\newGen -> loop $ newBoard newGen)
    'q' -> return board
    'l' -> loop $ moveTetromino board DirLeft
    'r' -> loop $ moveTetromino board DirRight
    'd' -> loop $ moveTetromino board DirDown
    _   -> loop board

render :: Board -> IO ()
render (Board _    GameOver                    _) = putStrLn ""
render (Board heap (GameOn (Tetromino blocks)) _) = do
  Console.clearScreen
  Console.setCursorPosition 30 0
  let heapBlocks = snd <$> Map.toList heap

  mapM_
    (\(Block (x, y) _) -> Console.setCursorPosition (height - y) x >> putStr "*"
    )
    heapBlocks
  mapM_
    (\(Block (x, y) _) -> Console.setCursorPosition (height - y) x >> putStr "*"
    )
    blocks

  Console.setCursorPosition 25 0
