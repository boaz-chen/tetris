import qualified Data.List                     as List
import qualified Data.Vector                   as Vec
import           System.Random
import           Data.Time.Clock
import           Data.Time.Format
import qualified System.Console.ANSI           as Console
import           Data.Functor
import           Data.Maybe                               ( fromMaybe )

data Tetromino = Tetromino [Block] Offset Size
  deriving (Eq)

instance Show Tetromino where
  show tetromino@(Tetromino blocks offset size) =
    "Relative blocks: "
      <> (show blocks)
      <> "\n"
      <> "Offset: "
      <> (show offset)
      <> "\n"
      <> "Absolute blocks: "
      <> (show $ absBlocks tetromino)

type Location = (Int, Int)
type Offset = (Int, Int)
type Size = Int

data Block = Block Location Color
  deriving (Eq, Show, Ord)

type Color = String -- TODO: replace with real Color

cyanI :: Tetromino
cyanI = fourSizedTetromino
  [ Block (0, 2) "Cyan"
  , Block (1, 2) "Cyan"
  , Block (2, 2) "Cyan"
  , Block (3, 2) "Cyan"
  ]

yellowO :: Tetromino
yellowO = twoSizedTetromino
  [ Block (0, 0) "Yellow"
  , Block (1, 0) "Yellow"
  , Block (0, 1) "Yellow"
  , Block (1, 1) "Yellow"
  ]

purpleT :: Tetromino
purpleT = threeSizedTetromino
  [ Block (0, 1) "Purple"
  , Block (1, 1) "Purple"
  , Block (2, 1) "Purple"
  , Block (1, 2) "Purple"
  ]

greenS :: Tetromino
greenS = threeSizedTetromino
  [ Block (0, 1) "Green"
  , Block (1, 1) "Green"
  , Block (1, 2) "Green"
  , Block (2, 2) "Green"
  ]

redZ :: Tetromino
redZ = threeSizedTetromino
  [ Block (0, 2) "Red"
  , Block (1, 2) "Red"
  , Block (1, 1) "Red"
  , Block (2, 1) "Red"
  ]

blueJ :: Tetromino
blueJ = threeSizedTetromino
  [ Block (0, 2) "Blue"
  , Block (0, 1) "Blue"
  , Block (1, 1) "Blue"
  , Block (2, 1) "Blue"
  ]

orangeL :: Tetromino
orangeL = threeSizedTetromino
  [ Block (0, 1) "Orange"
  , Block (1, 1) "Orange"
  , Block (2, 1) "Orange"
  , Block (2, 2) "Orange"
  ]

tetrominoes :: [Tetromino]
tetrominoes = [cyanI, yellowO, purpleT, greenS, redZ, blueJ, orangeL]

type Row = [Block]

type Heap = Vec.Vector Row

emptyRow :: Row
emptyRow = []

emptyHeap :: Heap
emptyHeap = Vec.replicate height emptyRow

data BoardState =
    GameOver
  | GameOn Tetromino
  deriving (Show)

data Board = Board Heap BoardState StdGen
  deriving (Show)

height = 20
width = 10
tetrominoHeight = 4
tetrominoWidth = 4

data Direction = DirLeft | DirRight | DirDown
  deriving (Eq, Show)

-----------

mkTetromino :: Size -> [Block] -> Tetromino
mkTetromino size blocks = Tetromino blocks (0, 0) size

heapMember :: Location -> Heap -> Bool
heapMember (col, row) heap =
  let heapRow   = heap Vec.!? row
      columns   = (fmap . fmap) (\(Block (col', _) _) -> col') heapRow
      colExists = fmap (List.elem col) columns
                              --List.elem col $ fmap (\(Block (col', _) _) -> col') $ heap Vec.! row
  in  fromMaybe False colExists

heapInsert :: Block -> Heap -> Heap
heapInsert block@(Block (_, row) _) heap = heap Vec.// [(row, newRow)]
  where newRow = block : (heap Vec.! row)

twoSizedTetromino = mkTetromino 2
threeSizedTetromino = mkTetromino 3
fourSizedTetromino = mkTetromino 4

absBlocks :: Tetromino -> [Block]
absBlocks (Tetromino blocks (xOffset, yOffset) _) =
  (\(Block (x, y) color) -> (Block (x + xOffset, y + yOffset) color)) <$> blocks

newBoard :: StdGen -> Board
newBoard gen =
  let (newTetromino, newGen) = randomTetromino gen
      positionedTetromino    = newTetromino
  in  Board emptyHeap (GameOn positionedTetromino) newGen

randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen =
  let (randomIx, gen0)               = randomR (0, (length tetrominoes) - 1) gen
      (Tetromino blocks (_, y) size) = tetrominoes !! randomIx
      (randomX, gen1)                = randomR (0, width - size - 1) gen
  in  (Tetromino blocks (randomX, y + height - 1) size, gen1)

offsetTetromino :: Tetromino -> Direction -> Tetromino
offsetTetromino (Tetromino blocks (x, y) size) dir =
  let (xOffset, yOffset) = case dir of
        DirLeft  -> (-1, 0)
        DirRight -> (1, 0)
        DirDown  -> (0, -1)
  in  Tetromino blocks (x + xOffset, y + yOffset) size

moveTetromino :: Board -> Direction -> Board
moveTetromino board@(Board _ GameOver _) _ = board
moveTetromino board@(Board heap (GameOn tetromino) gen) dir =
  let
    movedTetromino = offsetTetromino tetromino dir
    blocks         = absBlocks movedTetromino
    outOfBounds    = List.any (\(Block (x, _) _) -> x < 0 || x >= width) blocks
    collidesWithHeap =
      List.any (\(Block location _) -> heapMember location heap) blocks
    gameOver =
      collidesWithHeap
        && (List.any (\(Block (_, y) _) -> y >= height) $ absBlocks tetromino)
    landed = List.any (\(Block (_, y) _) -> y < 0) blocks
  in
    if gameOver
      then (Board heap GameOver gen)
      else
        if outOfBounds
           || (collidesWithHeap && List.elem dir [DirLeft, DirRight])
        then
          board
        else
          if landed || collidesWithHeap
            then
              let (newTetromino, newGen) = randomTetromino gen
              in  (Board (addToHeap tetromino heap)
                         (GameOn $ newTetromino)
                         newGen
                  )
            else (Board heap (GameOn movedTetromino) gen)

removeFullLines :: Board -> Board
removeFullLines board@(Board _    GameOver _) = board
removeFullLines board@(Board heap _        _) = undefined

addToHeap :: Tetromino -> Heap -> Heap
addToHeap tetromino heap = List.foldl' (\acc block -> heapInsert block acc)
                                       heap
                                       blocks
  where blocks = absBlocks tetromino

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

rotate :: Tetromino -> Tetromino
rotate (Tetromino blocks offset size) = Tetromino (rotateBlock <$> blocks)
                                                  offset
                                                  size
 where
  rotateBlock :: Block -> Block
  rotateBlock (Block (x, y) color) = (Block (size - y - 1, x) color)

-- main --
log' s = putStrLn $ ">>> " <> s

main :: IO ()
main = do
  gen <- getGoodStdGen
  loop $ newBoard gen
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

loop board@(Board heap (GameOn tetromino) gen) = do
  render board
  log' "Tetromino: "
  putStrLn $ show tetromino
  log' $ "Heap: "
  putStrLn $ show heap
  log'
    ". for right. , for left. d for down. space to rotate. q to quit. n to start a new game."
  c <- getChar

  case c of
    'n' -> getGoodStdGen >>= (\newGen -> loop $ newBoard newGen)
    'q' -> return board
    ',' -> loop $ moveTetromino board DirLeft
    '.' -> loop $ moveTetromino board DirRight
    'd' -> loop $ moveTetromino board DirDown
    ' ' -> loop $ Board heap (GameOn $ rotate tetromino) gen
    _   -> loop board

render :: Board -> IO ()
render (Board _    GameOver           _) = putStrLn ""
render (Board heap (GameOn tetromino) _) = do
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
  putStrLn $ take width $ repeat '▬'
  Console.setCursorPosition 25 0
