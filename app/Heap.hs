module Heap where

import           Config
import           Tetromino
import qualified Data.Vector                   as Vec
import qualified Data.List                     as List
import           Data.Maybe                               ( fromMaybe )

type Row = [Block]

type Heap = Vec.Vector Row

emptyRow :: Row
emptyRow = []

empty :: Heap
empty = Vec.replicate height emptyRow

member :: Location -> Heap -> Bool
member (col, row) heap =
  let heapRow   = heap Vec.!? row
      columns   = (fmap . fmap) (\(Block (col', _) _) -> col') heapRow
      colExists = fmap (List.elem col) columns
  in  fromMaybe False colExists

insert :: Block -> Heap -> Heap
insert block@(Block (_, row) _) heap = heap Vec.// [(row, newRow)]
  where newRow = block : (heap Vec.! row)

