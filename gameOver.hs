gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n = boardSeen board history
                          || countPieces board 0 0 n


boardSeen :: Board -> [Board] -> Bool
boardSeen current_board (b:boards) 
    | boards == [] = false  -- if the board is empty return false
    | current_board == b = true  --  if the current board is in list of boards return true
    | otherwise = boardSeen (current_board) (boards)


countPieces :: Board -> Int -> Int -> Int
countPieces (b:board) whitecount blackcount n
    | board == [] = if (whitecount < n) then true
                    else if (blackcount < n) then true
                    else false
    | b == "D" = countPieces board whitecount blackcount n
    | b == "W" = countPieces board (whitecount + 1) blackcount n
    | b == "B" = countPieces board whitecount (blackcount + 1) n
 