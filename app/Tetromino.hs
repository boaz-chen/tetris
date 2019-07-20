module Tetromino where

import           System.Random
import Types
import Config

data Tetromino = Tetromino [Block] Offset Size
  deriving (Eq)

instance Show Tetromino where
  show tetromino@(Tetromino blocks offset size) =
    "Relative blocks: "
      <> show blocks
      <> "\n"
      <> "Offset: "
      <> show offset
      <> "\n"
      <> "Absolute blocks: "
      <> show (absBlocks tetromino)

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

mkTetromino :: Size -> [Block] -> Tetromino
mkTetromino size blocks = Tetromino blocks (0, 0) size

twoSizedTetromino = mkTetromino 2
threeSizedTetromino = mkTetromino 3
fourSizedTetromino = mkTetromino 4

absBlocks :: Tetromino -> [Block]
absBlocks (Tetromino blocks (xOffset, yOffset) _) =
  (\(Block (x, y) color) -> Block (x + xOffset, y + yOffset) color) <$> blocks

randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen =
  let (randomIx, gen0)               = randomR (0, length tetrominoes - 1) gen
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

