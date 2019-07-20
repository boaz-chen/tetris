module Tetris where

import           Types
import           Config
import           Util
import           Tetromino
import qualified Heap
import qualified Data.List                     as List
import qualified Data.Vector                   as Vec
import           System.Random
import qualified System.Console.ANSI           as Console
import           Data.Functor
import qualified Graphics.Gloss                as Gloss
import qualified Graphics.Gloss.Data.Color     as Color
import qualified Graphics.Gloss.Data.Picture   as Picture
import qualified Graphics.Gloss.Interface.IO.Interact
                                               as Interact

data BoardState =
    GameOver
  | GameOn Tetromino
  deriving (Show)

type Score = Int

landingScore = 10
fullLineScore = 50

data Board = Board Heap.Heap BoardState StdGen Score
  deriving (Show)

newBoard :: StdGen -> Board
newBoard gen =
  let (newTetromino, newGen) = randomTetromino gen
      positionedTetromino    = newTetromino
  in  Board Heap.empty (GameOn positionedTetromino) newGen 0

moveTetromino :: Board -> Direction -> Board
moveTetromino board@(Board _ GameOver _ _) _ = board
moveTetromino board@(Board heap (GameOn tetromino) gen score) dir =
  let movedTetromino = offsetTetromino tetromino dir
      blocks = absBlocks movedTetromino
      outOfBounds = List.any (\(Block (x, _) _) -> x < 0 || x >= width) blocks
      collidesWithHeap =
          List.any (\(Block location _) -> Heap.member location heap) blocks
      gameOver = collidesWithHeap
        && List.any (\(Block (_, y) _) -> y >= height) (absBlocks tetromino)
      landed = List.any (\(Block (_, y) _) -> y < 0) blocks
  in  if gameOver
        then Board heap GameOver gen score
        else
          if outOfBounds
             || (collidesWithHeap && List.elem dir [DirLeft, DirRight])
          then
            board
          else
            if landed || collidesWithHeap -- tetromino landed
              then
                let
                  (newTetromino, newGen) = randomTetromino gen
                  boardWithUpdatedHeap   = Board (addToHeap tetromino heap)
                                                 (GameOn newTetromino)
                                                 newGen
                                                 score
                  boardWithoutFullLines = removeFullLines boardWithUpdatedHeap
                in
                  boardWithoutFullLines
              else Board heap (GameOn movedTetromino) gen score

removeFullLines :: Board -> Board
removeFullLines board@(Board _ GameOver _ _) = board
removeFullLines (Board heap state gen score) =
  let (fullLinesHeap, partialLinesHeap) =
          Vec.partition (\blocks -> length blocks == width) heap
      fullLinesCount = length fullLinesHeap
      completionHeap = Vec.replicate fullLinesCount Heap.emptyRow
      newHeap        = Vec.concat [partialLinesHeap, completionHeap]
      calibratedHeap = compressBlocks newHeap
      newScore       = score + landingScore + fullLineScore * fullLinesCount
  in  Board calibratedHeap state gen newScore

compressBlocks :: Heap.Heap -> Heap.Heap    -- modify each row blocks to have the correct row index
compressBlocks = Vec.imap modifyRow
 where
  modifyRow :: Int -> Heap.Row -> Heap.Row
  modifyRow rowIndex blocks =
    (\(Block (x, y) color) -> Block (x, rowIndex) color) <$> blocks

addToHeap :: Tetromino -> Heap.Heap -> Heap.Heap
addToHeap tetromino heap = List.foldl' (flip Heap.insert) heap blocks
  where blocks = absBlocks tetromino

rotate :: Tetromino -> Tetromino
rotate (Tetromino blocks offset size) =
  let correctedOffset | rightOverrun > 0 = rightOverrun
                      | leftOverrun < 0  = leftOverrun
                      | otherwise        = 0
      rotateBlock :: Block -> Block
      rotateBlock (Block (x, y) color) = Block (size - y - 1, x) color
      rotatedTetromino@(Tetromino rotatedBlocks (xOffset, yOffset) size') =
          Tetromino (rotateBlock <$> blocks) offset size
      absRotatedBlocks = absBlocks rotatedTetromino
      rightmost = List.maximum $ fmap (\(Block (x, _) _) -> x) absRotatedBlocks
      leftmost = List.minimum $ fmap (\(Block (x, _) _) -> x) absRotatedBlocks
      rightOverrun = rightmost - width + 1
      leftOverrun = leftmost
  in  Tetromino rotatedBlocks (xOffset - correctedOffset, yOffset) size'

-- main --
log' s = putStrLn $ ">>> " <> s

terminalMain :: IO ()
terminalMain = do
  gen <- getGoodStdGen
  loop $ newBoard gen
  putStrLn "\nGoodbye"

log'' s = s <> "\n"

loop :: Board -> IO Board
loop board@(Board _ GameOver gen score) = do
  log'
    $  "Game over. Your score is "
    ++ show score
    ++ ". n to start a new game or q to quit."
  c <- getChar

  case c of
    'n' -> getGoodStdGen >>= (loop . newBoard)
    'q' -> return board
    _   -> loop board

loop board@(Board heap (GameOn tetromino) gen score) = do
  render board
--  log' "Tetromino: "
--  print tetromino
--  log' "Heap: "
--  print heap
  log' $ "Score: " ++ show score
  log'
    ". for right. , for left. d for down. space to rotate. q to quit. n to start a new game."
  c <- getChar

  case c of
    'n' -> getGoodStdGen >>= (loop . newBoard)
    'q' -> return board
    ',' -> loop $ moveTetromino board DirLeft
    '.' -> loop $ moveTetromino board DirRight
    'd' -> loop $ moveTetromino board DirDown
    ' ' -> loop $ Board heap (GameOn $ rotate tetromino) gen score
    _   -> loop board

render :: Board -> IO ()
render (Board _    GameOver           _ _) = putStrLn ""
render (Board heap (GameOn tetromino) _ _) = do
  Console.clearScreen
  Console.setCursorPosition 25 0
  let blocks = absBlocks tetromino
  let blocksInBounds = List.filter
        (\(Block (x, y) _) -> x >= 0 && x < width && y >= 0 && y < height)
        blocks

  (mapM_ . mapM_)
    (\(Block (x, y) _) -> Console.setCursorPosition (height - y) x >> putStr "■"
    )
    heap
  mapM_
    (\(Block (x, y) _) -> Console.setCursorPosition (height - y) x >> putStr "■"
    )
    blocksInBounds

  Console.setCursorPosition 21 0
  putStrLn $ replicate width '▬'
  Console.setCursorPosition 25 0


----------------

main :: IO ()
main = do
  gen <- getGoodStdGen
  Gloss.play (Gloss.InWindow "Tetris" (500, 500) (500, 500))
             Color.black
             60
             (newBoard gen)
             renderBoard
             handleInput
             stepBoard

renderBoard :: Board -> Gloss.Picture
renderBoard _ = Picture.color Color.azure $ Picture.circle 100

handleInput :: Interact.Event -> Board -> Board
handleInput _ b = b

stepBoard :: Float -> Board -> Board
stepBoard _ b = b
