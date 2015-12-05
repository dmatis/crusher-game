minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic
    | null children = b
    | otherwise =
        let listvals = [ (minimax' x heuristic False) | x <- children]
            valindex     = (itemfinder (listvals) (maximum listvals) 0)
        in board (children!!valindex)


minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
minimax' (Node _ b children) heuristic maxPlayer
    | null children = heuristic b maxPlayer
    | otherwise =
        let minmaxlist = if maxPlayer then maximum else minimum
        in minmaxlist [ (minimax' x heuristic (not maxPlayer)) | x <- children ]


itemfinder' :: [Int] Int Int -> Int
itemfinder' (a:ax) elem counter
    | a == elem = counter
    | otherwise = itemfinder' ax elem (counter + 1)