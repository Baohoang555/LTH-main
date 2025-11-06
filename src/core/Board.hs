module Core.Board
  ( Player(..)
  , Board
  , rows, cols
  , emptyBoard
  , cellAt
  , lowestEmptyRow
  , dropPiece
  , legalMoves
  , isColumnFull
  , isFull
  , winner
  , isDraw
  , isOver
  , successors
  , render
  , countSeq
  , opponent  -- THÊM
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.List (tails)

-- 1. Đảo lượt
opponent :: Player -> Player
opponent X = O
opponent O = X
-- Kích thước
rows, cols :: Int
rows = 6
cols = 7

-- Người chơi
data Player = X | O
  deriving (Eq, Show)

-- Board: outer = rows (0 = đáy), inner = cols (0..6)
type Board = Vector (Vector (Maybe Player))

-- Board rỗng
emptyBoard :: Board
emptyBoard = V.replicate rows (V.replicate cols Nothing)

-- Kiểm in-bounds
inBounds :: Int -> Int -> Bool
inBounds r c = r >= 0 && r < rows && c >= 0 && c < cols

-- Lấy cell (Nothing nếu ngoài hoặc trống)
cellAt :: Board -> Int -> Int -> Maybe Player
cellAt b r c
  | inBounds r c = (b V.! r) V.! c
  | otherwise    = Nothing

-- Tìm hàng trống thấp nhất trong cột (row index), Nothing nếu cột đầy/invalid
lowestEmptyRow :: Board -> Int -> Maybe Int
lowestEmptyRow b c
  | c < 0 || c >= cols = Nothing
  | otherwise = go 0
  where
    go r | r >= rows = Nothing
         | isNothing (cellAt b r c) = Just r
         | otherwise = go (r + 1)

-- Thả quân (trả Board mới hoặc Nothing)
dropPiece :: Board -> Player -> Int -> Maybe Board
dropPiece b p c = do
  r <- lowestEmptyRow b c
  let oldRow = b V.! r
      newRow = oldRow V.// [(c, Just p)]
  return (b V.// [(r, newRow)])

-- Cột có thể đánh
legalMoves :: Board -> [Int]
legalMoves b = [ c | c <- [0..cols-1], isJust (lowestEmptyRow b c) ]

isColumnFull :: Board -> Int -> Bool
isColumnFull b c = isNothing (lowestEmptyRow b c)

isFull :: Board -> Bool
isFull b = null (legalMoves b)

-- Kiểm 4 in a row từ (r,c) theo hướng (dr,dc)
hasFourFrom :: Board -> Player -> Int -> Int -> Int -> Int -> Bool
hasFourFrom b p r c dr dc =
  all (== Just p) [ cellAt b (r + i*dr) (c + i*dc) | i <- [0..3] ]

-- Tìm winner (scan positions + 4 hướng)
winner :: Board -> Maybe Player
winner b = findFor X `orElse` findFor O
  where
    findFor ply = if any (\(r,c) -> any (\(dr,dc) -> hasFourFrom b ply r c dr dc) directions) positions
              then Just ply else Nothing
    positions = [ (r,c) | r <- [0..rows-1], c <- [0..cols-1] ]
    directions = [(0,1),(1,0),(1,1),(1,-1)]
    orElse (Just x) _ = Just x
    orElse Nothing y = y

isDraw :: Board -> Bool
isDraw b = isFull b && winner b == Nothing

isOver :: Board -> Bool
isOver b = isJust (winner b) || isDraw b

-- Generate successors an toàn
successors :: Board -> Player -> [(Int, Board)]
successors b p = mapMaybe (\c -> fmap (\nb -> (c, nb)) (dropPiece b p c)) (legalMoves b)

-- Render ASCII: in top -> bottom để nhìn giống bàn thật
render :: Board -> String
render b =
  let header = " 0 1 2 3 4 5 6 "
      rowStr r = concat [ "|" ++ [cellChar (cellAt b r c)] | c <- [0..cols-1] ] ++ "|"
      rowsStrs = [ rowStr r | r <- [rows-1, rows-2 .. 0] ]
      footer = replicate (cols * 2 + 1) '-'
  in unlines (header : rowsStrs ++ [footer])
  where
    cellChar Nothing  = '.'
    cellChar (Just X) = 'X'
    cellChar (Just O) = 'O'

-- =========== helpers cho heuristic ===========

-- Sliding windows length n
windows :: Int -> [a] -> [[a]]
windows n xs
  | n <= 0 = []
  | otherwise = [ take n t | t <- tails xs, length t >= n ]

-- Count windows all == Just player
countInLine :: Int -> Player -> [Maybe Player] -> Int
countInLine n p lst = length [ () | w <- windows n lst, all (== Just p) w ]

-- Horizontal lines (rows)
horizontals :: Board -> [[Maybe Player]]
horizontals b = [ V.toList (b V.! r) | r <- [0..rows-1] ]

-- Vertical lines (columns bottom->top)
verticals :: Board -> [[Maybe Player]]
verticals b = [ [ cellAt b r c | r <- [0..rows-1] ] | c <- [0..cols-1] ]

-- Helper: generate diagonal from start (r,c) with direction (dr,dc)
generateDiag :: Board -> (Int, Int) -> Int -> Int -> [Maybe Player]
generateDiag b (sr, sc) dr dc = go 0
  where
    go i =
      let r = sr + i * dr
          c = sc + i * dc
      in if inBounds r c
         then cellAt b r c : go (i + 1)
         else []

-- Diagonals up-right: /
diagonalsUR :: Board -> [[Maybe Player]]
diagonalsUR b = [ generateDiag b (r, 0) 1 1 | r <- [0..rows-1] ] ++
                [ generateDiag b (0, c) 1 1 | c <- [1..cols-1] ]

-- Diagonals up-left: \
diagonalsUL :: Board -> [[Maybe Player]]
diagonalsUL b = [ generateDiag b (r, cols-1) 1 (-1) | r <- [0..rows-1] ] ++
                [ generateDiag b (0, c) 1 (-1) | c <- [0..cols-2] ]

-- Note: above diagonals use a simple pattern to generate lists of Maybe Player.
-- If you prefer, we can replace with more explicit loops.

-- Count sequences length n for a player across all directions
countSeq :: Int -> Player -> Board -> Int
countSeq n p b
  | n < 1 || n > 4 = 0
  | otherwise = sum $ map (countInLine n p) allLines
  where
    allLines = horizontals b ++ verticals b ++ longDiagsUR ++ longDiagsUL
    longDiagsUR = filter (\l -> length l >= n) (diagonalsUR b)
    longDiagsUL = filter (\l -> length l >= n) (diagonalsUL b)