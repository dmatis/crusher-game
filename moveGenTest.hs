
data Piece = D | W | B deriving (Eq, Show)

type Point = (Int, Int)

type Tile  = (Piece, Point)

type Board = [Piece]

type Grid = [Point]

type State = [Tile]

type Slide = (Point,Point)

type Jump = (Point,Point,Point)

type Move = (Point,Point)

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n = ( (boardSeen board (tail history))
                          || (countBoardPieces board 0 0 n)
                          || (lessThanHalfPieces board 0 0 n) )


boardSeen :: Board -> [Board] -> Bool
boardSeen current_board boards 
    | boards == [] = False  -- if the board is empty return false
    | current_board == (head boards) = True  --  if the current board is in list of boards return true
    | otherwise = boardSeen (current_board) (tail boards) -- else recursively call boardSeen with the rest of the history and the current board


countBoardPieces :: Board -> Int -> Int -> Int -> Bool
countBoardPieces board whitecount blackcount n
    | board == [] = if (whitecount < n) then True  -- if the board is empty and count of white pieces < 0, return true
                    else if (blackcount < n) then True -- else if count of black pieces < 0, return true
                    else False -- return false
    | (head board) == D = countBoardPieces (tail board) whitecount blackcount n -- if the piece is "D", recursively call countPieces with the rest of the board
    | (head board) == W = countBoardPieces (tail board) (whitecount + 1) blackcount n -- if the piece is "W", recursively call countPieces with the rest of the board and increment the count of white pieces
    | (head board) == B = countBoardPieces (tail board) whitecount (blackcount + 1) n -- if the piece is "B", recursively call countPieces with the rest of the board and increment the count of black pieces
 
lessThanHalfPieces :: Board -> Int -> Int -> Int -> Bool
lessThanHalfPieces board whitecount blackcount n
    | board == [] = if (whitecount < (round cond)) then True  -- if the board is empty and count of white pieces < 0, return true
                    else if (blackcount < (round cond)) then True -- else if count of black pieces < 0, return true
                    else False -- return false
    | (head board) == D = countBoardPieces (tail board) whitecount blackcount n -- if the piece is "D", recursively call countPieces with the rest of the board
    | (head board) == W = countBoardPieces (tail board) (whitecount + 1) blackcount n -- if the piece is "W", recursively call countPieces with the rest of the board and increment the count of white pieces
    | (head board) == B = countBoardPieces (tail board) whitecount (blackcount + 1) n -- if the piece is "B", recursively call countPieces with the rest of the board and increment the count of black pieces
        where 
            cond = (((2 * (fromIntegral n)) - 1) / 2) 

generateSlides :: Grid -> Int -> [Slide]
generateSlides b n = concat [genSlidesHelper b pt (genSlidePointsHelper pt n) | pt <- b ]

genSlidesHelper :: Grid -> Point -> [Point] -> [Slide]
genSlidesHelper b oldPt points = [(oldPt, newPt) | newPt <- points, newPt `elem` b]

genSlidePointsHelper :: Point -> Int -> [Point]
genSlidePointsHelper pt n
    | ((snd pt) < n-1)     = ((fst pt)+1,(snd pt)) : ((fst pt)-1,(snd pt)) :
                             ((fst pt), (snd pt) + 1) : ((fst pt),(snd pt)-1) :
                             ((fst pt)-1,(snd pt)-1) : ((fst pt)+1,(snd pt)+1) : []

    | ((snd pt) == n-1)    = ((fst pt)+1,(snd pt)) : ((fst pt)-1,(snd pt)) :
                             ((fst pt), (snd pt) + 1) : ((fst pt),(snd pt)-1) : 
                             ((fst pt)-1,(snd pt)-1) : ((fst pt)-1,(snd pt)+1) : []
                             
    | otherwise            = ((fst pt)+1,(snd pt)) : ((fst pt)-1,(snd pt)) :
                             ((fst pt), (snd pt) + 1) : ((fst pt),(snd pt)-1) :
                             ((fst pt)+1,(snd pt)-1) : ((fst pt)-1,(snd pt)+1) : []

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = concat [genLeapsHelper b (genPointsHelper pt n) | pt <- b ]

--generates a list of all valid moves from a given point
genPointsHelper :: Point -> Int -> [Jump]
genPointsHelper pt n

    | ((snd pt) == n-2)    = (pt, ((fst pt)+1,(snd pt)), ((fst pt)+2,(snd pt))) : 
                             (pt, ((fst pt)-1,(snd pt)), ((fst pt)-2,(snd pt))) :
                             (pt, ((fst pt), (snd pt) + 1), ((fst pt)-1, (snd pt) + 2)) : 
                             (pt, ((fst pt),(snd pt)-1) , ((fst pt),(snd pt)-2)) :
                             (pt, ((fst pt)-1,(snd pt)-1), ((fst pt)-2,(snd pt)-2)) : 
                             (pt, ((fst pt)+1,(snd pt)+1), ((fst pt)+1,(snd pt)+2)) : []

    | ((snd pt) < n-2)     = (pt, ((fst pt)+1,(snd pt)), ((fst pt)+2,(snd pt))) : 
                             (pt, ((fst pt)-1,(snd pt)), ((fst pt)-2,(snd pt))) :
                             (pt, ((fst pt), (snd pt) + 1), ((fst pt), (snd pt) + 2)) : 
                             (pt, ((fst pt),(snd pt)-1) , ((fst pt),(snd pt)-2)) :
                             (pt, ((fst pt)-1,(snd pt)-1), ((fst pt)-2,(snd pt)-2)) : 
                             (pt, ((fst pt)+1,(snd pt)+1), ((fst pt)+2,(snd pt)+2)) : []

    | ((snd pt) == n-1)    = (pt, ((fst pt)+1,(snd pt)), ((fst pt)+2,(snd pt))) : 
                             (pt, ((fst pt)-1,(snd pt)), ((fst pt)-2,(snd pt))) :
                             (pt, ((fst pt), (snd pt) + 1), ((fst pt), (snd pt) + 2)) : 
                             (pt, ((fst pt),(snd pt)-1), ((fst pt),(snd pt)-2)) : 
                             (pt, ((fst pt)-1,(snd pt)-1), ((fst pt)-2,(snd pt)-2)) : 
                             (pt, ((fst pt)-1,(snd pt)+1), ((fst pt)-2,(snd pt)+2)) : []

    | ((snd pt) == n)      = (pt, ((fst pt)+1,(snd pt)), ((fst pt)+2,(snd pt))) : 
                             (pt, ((fst pt)-1,(snd pt)), ((fst pt)-2,(snd pt))) :
                             (pt, ((fst pt), (snd pt)-1), ((fst pt)-1, (snd pt)-2)) : 
                             (pt, ((fst pt),(snd pt)+1) , ((fst pt),(snd pt)+2)) :
                             (pt, ((fst pt)+1,(snd pt)-1), ((fst pt)+1,(snd pt)-2)) : 
                             (pt, ((fst pt)-1,(snd pt)+1), ((fst pt)-2,(snd pt)+2)) : []
                             
    | otherwise            = (pt, ((fst pt)+1,(snd pt)), ((fst pt)+2,(snd pt))) : 
                             (pt, ((fst pt)-1,(snd pt)), ((fst pt)-2,(snd pt))) :
                             (pt, ((fst pt), (snd pt) + 1), ((fst pt), (snd pt) + 2)) : 
                             (pt, ((fst pt),(snd pt)-1), ((fst pt),(snd pt)-2)) :
                             (pt, ((fst pt)+1,(snd pt)-1), ((fst pt)+2,(snd pt)-2)) : 
                             (pt, ((fst pt)-1,(snd pt)+1), ((fst pt)-2,(snd pt)+2)) : []

genLeapsHelper :: Grid -> [Jump] -> [Jump]
genLeapsHelper b jumps = [((fstPt jump),(sndPt jump), (thdPt jump)) | jump <- jumps, (sndPt jump) `elem` b, (thdPt jump) `elem` b]

--get first point in the jump
fstPt :: Jump -> Point
fstPt (j,_,_) = j

--get second point in the jump
sndPt :: Jump -> Point
sndPt (_,j,_) = j

--get third point in the jump
thdPt :: Jump -> Point
thdPt (_,_,j) = j

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
    where 
        check 'W' = W
        check 'B' = B
        check '-' = D

--test arguments
grid0 = generateGrid 3 2 4 []
grid4 = generateGrid 4 2 4 []
slides0 = generateSlides grid0 3
slides4 = generateSlides grid4 4
jumps0 = generateLeaps grid0 3
jumps4 = generateLeaps grid4 4
board0 = sTrToBoard "WWW-WW-------BB-BBB"
gameOverBoard1 = sTrToBoard "WWW-WW---------B--B"
gameOverBoard2 = sTrToBoard "WWW-WW------------B"
notGameOverBoard3 = sTrToBoard "WW---W----------BBB"
board4 = sTrToBoard "WWWW-WWW---------------------BBB-BBBB"
state0 = getState board0 grid0
state4 = getState board4 grid4
moves0W = moveGenerator state0 slides0 jumps0 W
moves0B = moveGenerator state0 slides0 jumps0 B
history0W = [sTrToBoard "-WWWWW-------BB-BBB",sTrToBoard "WWW-WW-------BB-BBB"]
boards0W = createBoards state0 moves0W W
board0History = [sTrToBoard "WWW-WW-------BB-BBB"]
gameOverHistory1 = [sTrToBoard "WWW-WW---------B--B"]
gameOverHistory2 = [sTrToBoard "WWW-WW------------B"]
notGameOverHistory3 = [sTrToBoard "WW---W----------BBB", sTrToBoard "-WW--W----------BBB", sTrToBoard "-W-W-W----------BBB"]


-- getState :: Board -> Grid -> State
-- getState b grid = [(zip' piece pt) | piece <- b | pt <- grid ]

-- Zips together piece and point to create Tile
getState :: Board -> Grid -> State
getState xs     []     = []
getState []     ys     = []
getState (x:xs) (y:ys) = (x, y) : getState xs ys

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player =
    filterBoards (createBoards state moves player) history
    where
        state = (getState board grid)
        moves = (moveGenerator state slides jumps player)


createBoards :: State -> [Move] -> Piece -> [Board]
createBoards state moves p = [(updateBoard state origin dest p) | m@(origin, dest) <- moves]

updateBoard :: State -> Point -> Point -> Piece -> Board
updateBoard state origin dest p = [updateBoardHelper origin dest point piece p | s@(piece,point) <- state]

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where 
        check W = 'W'
        check B = 'B'
        check D = '-'

updateBoardHelper :: Point -> Point -> Point -> Piece -> Piece -> Piece
updateBoardHelper origin dest point piece p 
    | (origin == point)  = D
    | (dest == point)    = p
    | otherwise          = piece

--filters out the boards that have already occurred in history
filterBoards :: [Board] -> [Board] -> [Board]
filterBoards boards history = [b | b <- boards, (not(b `elem` history))]




genTreeHelper :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> BoardTree
genTreeHelper board history grid slides jumps player depth n height
    | (depth == height)                                                 = (Node height board [])
    | (generateNewStates board history grid slides jumps player == [])  = (Node height board [])
    | (gameOver board history n)                                        = (Node height board [])
    | otherwise                          = (Node height board [(genTreeHelper cBoard (cBoard:history) grid slides jumps nextPlayer depth n (height+1)) |cBoard <- childBoards])
        where
            childBoards = (generateNewStates board history grid slides jumps player)
            nextPlayer = if player == W then B else W




moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player
    | player == D                       = []
    | otherwise                         = getMoves player state slides jumps [(snd tile) | tile <- state, (fst tile) == player]

--Generates a list of valid moves from a given state for Player Piece
getMoves :: Piece -> State -> [Slide] -> [Jump] -> [Point] -> [Move]
getMoves p state slides jumps points = (getSlides state slides points) ++ (getJumps p state jumps points)

--Generates all valid slides on the board (the tile being moved to is empty)
getSlides :: State -> [Slide] -> [Point] -> [Move]
getSlides state slides points = [sl | pt <- points, sl@(x,y) <- slides, pt == x, (blankTile state y)]

--Generates all valid jumps on the board (where it matches the piece color being jumped over and lands on a blank OR opponent piece)
getJumps :: Piece -> State -> [Jump] -> [Point] -> [Move]
getJumps p state jumps points = [(x,z) | pt <- points, j@(x,y,z) <- jumps, pt == x, (matchesPiece p state y), (notPlayerTile p state z)]

--Returns true if a given point does not contain a player's piece
notPlayerTile :: Piece -> State -> Point -> Bool
notPlayerTile p state point = True `elem` [True | tile <- state, (snd tile) == point, (fst tile) /= p ]

 --Returns true if empty tile at a given point
blankTile :: State -> Point -> Bool 
blankTile state point =  True `elem` [True | tile <- state, (snd tile) == point, (fst tile) == D ]

--Returns true if the given piece (that we will jump over) is the same color as Player Piece
matchesPiece :: Piece -> State -> Point -> Bool
matchesPiece p state point = True `elem` [True | tile <- state, (snd tile) == point, (fst tile) == p ]