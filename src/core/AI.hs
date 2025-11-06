module Core.AI
  ( evaluateBoard
  , minimax
  , bestMove
  , aiMove
  ) where

import Core.Board
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Đánh giá bàn cờ: X - O
evaluateBoard :: Board -> Player -> Int
evaluateBoard board player =
  score player - score (opponent player)
  where
    score p = countSeq 4 p board * 1000   -- 4 liên tiếp = gần thắng
           + countSeq 3 p board * 100    -- 3 liên tiếp = nguy hiểm
           + countSeq 2 p board * 10     -- 2 liên tiếp = tiềm năng

-- 3. Minimax đơn giản: thử hết nước đi, chọn nước tốt nhất
minimax :: Int -> Board -> Player -> Int
minimax 0 board _ = evaluateBoard board X - evaluateBoard board O  -- Điểm X trừ điểm O
minimax depth board player =
  case winner board of
    Just p  -> if p == X then 99999 else -99999
    Nothing -> if isDraw board then 0 else bestScore
  where
    moves = successors board player
    nextPlayer = opponent player
    scores = [ minimax (depth - 1) newBoard nextPlayer | (_, newBoard) <- moves ]
    bestScore = if player == X then maximum scores else minimum scores

-- 4. Tìm nước đi tốt nhất: thử từng cột, chọn cột có điểm cao nhất
bestMove :: Int -> Board -> Player -> Int
bestMove depth board player =
  let moves = successors board player
      scored = [ (col, minimax (depth - 1) newBoard (opponent player)) | (col, newBoard) <- moves ]
  in if null scored
     then error "Không còn nước đi!"
     else fst $ maximumBy (comparing snd) scored

-- 5. AI chọn nước đi + in ra console
aiMove :: Int -> Board -> Player -> IO Int
aiMove depth board player = do
  putStrLn $ "AI (" ++ show player ++ ") đang nghĩ... (độ sâu: " ++ show depth ++ ")"
  let col = bestMove depth board player
  putStrLn $ "AI chọn cột: " ++ show col
  return col