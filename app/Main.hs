import Game
import Core.Board (Player(..))  -- THÊM DÒNG NÀY

main :: IO ()
main = do
  putStrLn "Chọn độ khó AI:"
  putStrLn "1. Dễ (depth 3)"
  putStrLn "2. Trung bình (depth 5)"
  putStrLn "3. Khó (depth 7)"
  diff <- getLine
  let depth = case diff of "1" -> 3; "3" -> 7; _ -> 5

  putStrLn "Bạn muốn đi trước (X)? (y/n):"
  first <- getLine
  let humanFirst = first == "y" || first == "Y"

  let cfg = GameConfig
        { cfgDepth = depth
        , cfgHumanFirst = humanFirst
        , cfgHumanPlayer = X  -- BÂY GIỜ ĐÃ ĐÚNG
        }

  runGame cfg