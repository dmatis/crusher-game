crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) player d size =
    let optimalBoard = (stateSearch board history grid slides leaps) (convert_char_to_piece d) size
    in ((boardToStr optimalBoard):(current:old))
        where
            grid = generateGrid size (size - 1) (2 * (size - 1)) []
            slides = generateSlides grid size
            leaps = generateLeaps grid size
            board = sTrToBoard current
            history = map sTrToBoard old


convertCharToPiece :: Char -> Piece
convertCharToPiece player
  | player == 'W' = W
  | player == 'B' = B
  | otherwise = D