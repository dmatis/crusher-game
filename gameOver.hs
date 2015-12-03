gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n = boardSeen board history
                          || countPieces board 0 0 n


boardSeen :: Board -> [Board] -> Bool
boardSeen current_board boards 
    | boards == [] = false  -- if the board is empty return false
    | current_board == (head boards) = true  --  if the current board is in list of boards return true
    | otherwise = boardSeen (current_board) (tail boards) -- else recursively call boardSeen with the rest of the history and the current board


countPieces :: Board -> Int -> Int -> Int
countPieces board whitecount blackcount n
    | board == [] = if (whitecount < n) then true  -- if the board is empty and count of white pieces < 0, return true
                    else if (blackcount < n) then true -- else if count of black pieces < 0, return true
                    else false -- return false
    | (head board) == "D" = countPieces (tail board) whitecount blackcount n -- if the piece is "D", recursively call countPieces with the rest of the board
    | (head board) == "W" = countPieces (tail board) (whitecount + 1) blackcount n -- if the piece is "W", recursively call countPieces with the rest of the board and increment the count of white pieces
    | (head board) == "B" = countPieces (tail board) whitecount (blackcount + 1) n -- if the piece is "B", recursively call countPieces with the rest of the board and increment the count of black pieces
 