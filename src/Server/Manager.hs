module Server.Manager
  ( GameConfig(..)
  , runGame
  ) where

import Core.Board
  ( Board
  , Player(..)
  , opponent
  , render
  , isOver
  , winner
  , dropPiece
  , lowestEmptyRow
  , emptyBoard
  )
import Core.AI (bestMove)
import Data.Maybe (isJust)
import System.IO (hSetEncoding, utf8, stdout)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

-- | Cấu hình ván chơi
data GameConfig = GameConfig
  { cfgDepth       :: Int          -- Độ sâu AI
  , cfgHumanFirst  :: Bool         -- Người đi trước?
  , cfgHumanPlayer :: Player       -- Người chơi là X hay O
  }

-- | Chạy một ván chơi hoàn chỉnh
runGame :: GameConfig -> IO ()
runGame cfg = do
  hSetEncoding stdout utf8
  printWelcome
  printConfig cfg
  let startPlayer = if cfgHumanFirst cfg then cfgHumanPlayer cfg else opponent (cfgHumanPlayer cfg)
  gameLoop emptyBoard startPlayer cfg

-- Vòng lặp chính
gameLoop :: Board -> Player -> GameConfig -> IO ()
gameLoop board currentPlayer cfg = do
  putStrLn $ render board
  putStrLn $ "Lượt: " ++ show currentPlayer ++ playerLabel currentPlayer cfg

  if isOver board
    then do
      showGameResult board
      askPlayAgain cfg  -- THÊM: Hỏi chơi lại
    else do
      move <- getMove board currentPlayer cfg
      case dropPiece board currentPlayer move of
        Nothing -> do
          putStrLn "Nước đi không hợp lệ!"
          gameLoop board currentPlayer cfg
        Just newBoard ->
          gameLoop newBoard (opponent currentPlayer) cfg

-- Lấy nước đi: từ người chơi hoặc AI
getMove :: Board -> Player -> GameConfig -> IO Int
getMove board player cfg
  | isHumanTurn player cfg = humanMove board
  | otherwise              = aiMove' (cfgDepth cfg) board player

-- Kiểm tra có phải lượt người chơi?
isHumanTurn :: Player -> GameConfig -> Bool
isHumanTurn player cfg = player == cfgHumanPlayer cfg

-- Nhãn người chơi
playerLabel :: Player -> GameConfig -> String
playerLabel player cfg
  | player == cfgHumanPlayer cfg = " (bạn)"
  | otherwise                    = " (AI)"

-- === GIAO TIẾP NGƯỜI CHƠI ===

humanMove :: Board -> IO Int
humanMove board = do
  putStrLn "Nhập cột (0-6) hoặc 'q' để thoát:"
  input <- getLine
  case input of
    "q" -> putStrLn "Tạm biệt!" >> exitSuccess
    _   -> case readMaybe input of
      Just col | col >= 0 && col <= 6 && isJust (lowestEmptyRow board col) ->
        return col
      Just _ ->
        putStrLn "Cột đầy! Chọn lại." >> humanMove board
      Nothing ->
        putStrLn "Sai định dạng! Nhập số 0-6." >> humanMove board

-- === GIAO TIẾP AI ===

aiMove' :: Int -> Board -> Player -> IO Int
aiMove' depth board player = do
  putStrLn $ "AI (" ++ show player ++ ") đang suy nghĩ... (depth = " ++ show depth ++ ")"
  let col = bestMove depth board player
  putStrLn $ "AI chọn cột: " ++ show col
  return col

-- === HIỂN THỊ KẾT QUẢ + CHƠI LẠI ===

showGameResult :: Board -> IO ()
showGameResult board = do
  putStrLn "=================================="
  case winner board of
    Just p  -> putStrLn $ "   " ++ show p ++ " THẮNG! Chúc mừng!"
    Nothing -> putStrLn "   HÒA! Không ai thắng."
  putStrLn "=================================="

askPlayAgain :: GameConfig -> IO ()
askPlayAgain cfg = do
  putStrLn "Bạn có muốn chơi lại? (y/n): "
  input <- getLine
  if input == "y" || input == "Y"
    then runGame cfg  -- Chơi lại với cùng config
    else putStrLn "Cảm ơn bạn đã chơi! Tạm biệt!"

-- === THÔNG BÁO KHAI MẠC ===

printWelcome :: IO ()
printWelcome = do
  putStrLn "=================================="
  putStrLn "     CONNECT FOUR - HASKELL       "
  putStrLn "==================================\n"

printConfig :: GameConfig -> IO ()
printConfig cfg = do
  putStrLn $ "Bạn là: " ++ show (cfgHumanPlayer cfg)
  putStrLn $ "Đi " ++ if cfgHumanFirst cfg then "trước" else "sau"
  putStrLn $ "Độ khó AI: depth = " ++ show (cfgDepth cfg) ++ "\n"