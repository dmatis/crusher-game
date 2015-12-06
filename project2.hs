-- CPSC 312 - Project 2
-- 
import Debug.Trace
-- Name: Darren Matis
-- Student Number: 94897071
-- ugrad ID: f3w8

-- Name: Abrar Musa
-- Student Number: 48915086
-- ugrad ID: i1u9a

-- Name: Cunn Yong Goh
-- Student Number: 58362138
-- ugrad ID: f8f9

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- ///////////////////////////////////////////////////////////////////////////////////
-- NOTE : ALL SECTIONS DONE INCLUDE A COMMENTED HEADER AS SUCH:

-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --
-- ==================     GAMEOVER FUNCTION   ========================== --
-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --


-- //////////////////////////////////////////////////////////////////////////////////
-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--       W is a piece of the White player
--       B is a piece of the Black player
--

data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
-- 
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--       newBoard is the next board to add to the tree
--       seenBoards is the updated history to avoid possible future trouble boards
--       cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
--       board is the game state at that node
--       nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move over
--       the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
--       the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--       is that a jump can be reduced to a move as in effect 
--       nothing happens the point moved over in a jump
--

type Move = (Point,Point)


-- A simple function in order to "play" the game

play :: [String] -> Char -> Int -> Int -> IO ()
play history@(current:old) player depth n
  | gameOver (sTrToBoard current) (map sTrToBoard history) n = putStrLn "Game over."
  | otherwise = do 
       let history'@(new:_) = crusher history player depth n
       putStrLn $ player:" played: " ++ new
       play history' (if player == 'W' then 'B' else 'W') depth n

--
-- Some test results to see what functions are producing 
--
grid0 = generateGrid 3 2 4 []
grid4 = generateGrid 4 2 4 []
slides0 = generateSlides grid0 3
slides4 = generateSlides grid4 4
jumps0 = generateLeaps grid0 3
jumps4 = generateLeaps grid4 4
board0 = sTrToBoard "WWW-WW-------BB-BBB"
board1 = sTrToBoard "-WWWWW-------BB-BBB"
boardCrush = sTrToBoard "WWW-WW--B-----B-BBB"
board4 = sTrToBoard "WWWW-WWW---------------------BBB-BBBB"
state0 = getState board0 grid0
stateCrush = getState boardCrush grid0
state4 = getState board4 grid4
moves0W = moveGenerator state0 slides0 jumps0 W
moves0B = moveGenerator state0 slides0 jumps0 B
history0W = [sTrToBoard "-WWWWW-------BB-BBB",sTrToBoard "WWW-WW-------BB-BBB"]
history1W = ["-WWWWW-------BB-BBB","WWW-WW-------BB-BBB"]
boards0W = createBoards state0 moves0W W

crushBoard = "WWW-WW---B---B----B"
crushBoardHist = ["WWW--WW--B---B----B", "WWW-WW---B---B----B"]

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) player d size =
    let optimalBoard = (stateSearch board history grid slides leaps (convertCharToPiece player) d size) -- get the optimal board using stateSearcg
    in ((boardToStr optimalBoard):(current:old))
        where
            grid = generateGrid size (size - 1) (2 * (size - 1)) []
            slides = generateSlides grid size
            leaps = generateLeaps grid size
            board = sTrToBoard current
            history = map sTrToBoard old

--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --
-- ==================     GAMEOVER FUNCTION   ========================== --
-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n = ( (boardSeen board (tail history))  -- checks if the board was previously observed
                          || (countBoardPieces board 0 0 n) -- checks if B or W pieces total to less than n
                          || (lessThanHalfPieces board 0 0 n) ) -- checks if B or W has less than half the pieces 


-- convers a Char To a Piece
convertCharToPiece :: Char -> Piece
convertCharToPiece player
  | player == 'W' = W -- if player is white return W
  | player == 'B' = B -- if player is black return B
  | otherwise = D -- else return D

-- Checks to see if a board is present in history
boardSeen :: Board -> [Board] -> Bool
boardSeen current_board boards 
    | boards == [] = False  -- if the board is empty return false
    | current_board == (head boards) = True  --  if the current board is in list of boards return true
    | otherwise = boardSeen (current_board) (tail boards) -- else recursively call boardSeen with the rest of the history and the current board


-- Checks to see if the number of either black or white pieces is less than n
countBoardPieces :: Board -> Int -> Int -> Int -> Bool
countBoardPieces board whitecount blackcount n
    | board == [] = if (whitecount < n) then True  -- if the board is empty and count of white pieces < 0, return true
                    else if (blackcount < n) then True -- else if count of black pieces < 0, return true
                    else False -- return false
    | (head board) == D = countBoardPieces (tail board) whitecount blackcount n -- if the piece is "D", recursively call countPieces with the rest of the board
    | (head board) == W = countBoardPieces (tail board) (whitecount + 1) blackcount n -- if the piece is "W", recursively call countPieces with the rest of the board and increment the count of white pieces
    | (head board) == B = countBoardPieces (tail board) whitecount (blackcount + 1) n -- if the piece is "B", recursively call countPieces with the rest of the board and increment the count of black pieces

-- checks to see if either black or white pieces are less than half the initial amount
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

-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
--       [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
    where 
        check 'W' = W
        check 'B' = B
        check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B] 
--       to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board 
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where 
        check W = 'W'
        check B = 'B'
        check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--         initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--       [(0,0),(1,0),(2,0)
--        (0,1),(1,1),(2,1),(3,1)
--        (0,2),(1,2),(2,2),(3,2),(4,2)
--        (0,3),(1,3),(2,3),(3,3)
--        (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
--       it is a part of the internal representation of the game, this 
--       list of all possible slides is only generated once; and when 
--       generating next moves, the program decides which slides out of 
--       all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--

generateSlides :: Grid -> Int -> [Slide]
generateSlides b n = concat [genSlidesHelper b pt (genSlidePointsHelper pt n) | pt <- b ]

--Generates the list of possible slides from a given point in the board (pt)
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

--generates a list of slides from a given starting point (oldPt)
genSlidesHelper :: Grid -> Point -> [Point] -> [Slide]
genSlidesHelper b oldPt points = [(oldPt, newPt) | newPt <- points, newPt `elem` b]

--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
--       it is a part of the internal representation of the game, this 
--       list of all possible leaps is only generated once; and when 
--       generating next moves, the program decides which leaps out of 
--       all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

--Jump = (Point,Point,Point)

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = concat [genLeapsHelper b (genPointsHelper pt n) | pt <- b ]

--generates a list of all valid jumps from a given point
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

--Filters the list of jumps to return only the valid ones
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

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--

stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth num
    | (gameOver board history num)                                          = board
    | ((generateNewStates board history grid slides jumps player) == [])    = board
    | otherwise                       = minimax (generateTree board history grid slides jumps player depth num) (boardEvaluator player board history num) player history num
--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n = genTreeHelper board history grid slides jumps player depth n 0

--generateTree helper function to add additional height argument
genTreeHelper :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> BoardTree
genTreeHelper board history grid slides jumps player depth n height
    | (depth == height)                                                 = (Node height board [])
    | (generateNewStates board history grid slides jumps player == [])  = (Node height board [])
    | (gameOver board history n)                                        = (Node height board [])
    | otherwise                          = (Node height board [genTreeHelper b (board:history) grid slides jumps nextPlayer depth n (height+1) |b <- childBoards])
        where
            childBoards = (generateNewStates board history grid slides jumps player)
            nextPlayer = if player == W then B else W

--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player =
    filterBoards (createBoards state moves player) history
    where
        state = (getState board grid)
        moves = (moveGenerator state slides jumps player)

--Creates the list of all possible boards from a list of valid moves in a given state
createBoards :: State -> [Move] -> Piece -> [Board]
createBoards state moves p = [(updateBoard state origin dest p) | m@(origin, dest) <- moves]

--For each piece in a given state, updates it to contain the correct piece after a move has completed
updateBoard :: State -> Point -> Point -> Piece -> Board
updateBoard state origin dest p = [updateBoardHelper origin dest point piece p | s@(piece,point) <- state]

--Updates points in a board, origin of moved piece becomes blank, destination becomes the piece
updateBoardHelper :: Point -> Point -> Point -> Piece -> Piece -> Piece
updateBoardHelper origin dest point piece p 
    | (origin == point)  = D
    | (dest == point)    = p
    | otherwise          = piece

--Filters out the boards that have already occurred in history
filterBoards :: [Board] -> [Board] -> [Board]
filterBoards boards history = [b | b <- boards, (not(b `elem` history))]

--Zips together piece and point to create State
getState :: Board -> Grid -> State
getState xs     []     = []
getState []     ys     = []
getState (x:xs) (y:ys) = (x, y) : getState xs ys

--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--       type State, for our purposes it is zipping the board and the
--       grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--
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

--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

-- Goodness value determined by: The difference between black/white pieces on the board

-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --
-- ==================     BOARDEVALUATOR   ============================= --
-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --

boardEvaluator :: Piece -> Board -> [Board] -> Int -> Int
boardEvaluator player board history n
    | (gameOver board history n) == True = getFinalScore (whoWon (countPlayerPieces player board 0) player) player -- if its gameOver, get the Final Score for the player
    | otherwise = countPlayerPieces player board 0 -- else return the heuristic calculation

-- Checks who won based on wheather the result of countPlayerPieces is less than 0 or greater than 0
whoWon :: Int -> Piece -> Piece
whoWon num player
    | (num < 0) = (getOtherPlayer player) -- if its less than 0, return opponent
    | (num > 0) = player -- else return the player

-- Gets the opponents Piece depending on the players piece
getOtherPlayer :: Piece -> Piece
getOtherPlayer player
    | player == B = W -- if the player is B return W
    | otherwise = B -- if the player is W return B

-- Outputs 10 if the player won or -10 if the player lost
getFinalScore :: Piece -> Piece -> Int
getFinalScore winner player
    | winner == player = 10 -- if the player is the winner, return 10
    | otherwise = (-10) -- if the player is not the winner, return -10
  

-- Count the pieces on the board and return the difference in black/white pieces from the perspective of the player
countPlayerPieces :: Piece -> Board -> Int  -> Int
countPlayerPieces player board counter
    | player == B = (countPieces B board 0) - (countPieces W board 0) -- if player is B, return B - W pieces
    | otherwise     = (countPieces W board 0) - (countPieces B board 0) -- if player is W, return B - W pieces


-- Counts the number of pieces for a player on the board
countPieces :: Piece -> Board -> Int -> Int
countPieces player board counter 
  | (null board)           = counter     
  | (head board) == player = countPieces player (tail board) (counter + 1)
  | otherwise              = countPieces player (tail board) counter



--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--               appropriate heuristic to apply based on the size of the board,
--               who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--
-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --
-- ==================     MINIMAX             ========================== --
-- ===================================================================== --
-- ===================================================================== --
-- ===================================================================== --

minimax :: BoardTree -> Int -> Piece -> [Board] -> Int ->Board
minimax (Node _ b children) heuristic player history n
    | null children = b
    | otherwise =
        let listvals = [ (minimax' x heuristic False player history n) | x <- children] -- create a list of minimax values
            valindex = (itemfinder' (listvals) (maximum listvals) 0) -- find item with the max value
        in board (children!!valindex) -- return board

--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes 
-- a search tree, an appropriate heuristic to apply to the leaf nodes of 
-- the tree, and based on whether it would have been the maximizing 
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--               appropriate heuristic to apply based on the size of the board,
--               who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
--               or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--
-- 
minimax' :: BoardTree -> Int -> Bool -> Piece -> [Board] -> Int -> Int
minimax' (Node _ b children) heuristic maxPlayer player history n
    | null children = (boardEvaluator player b history n) -- If the list of children is null, return the boardevaluator result of the variables
    | otherwise =
        let minmaxlist = if maxPlayer then maximum else minimum  -- return max/min value from list based on maxPlayer
        in minmaxlist [ (minimax' x heuristic (not maxPlayer) player history n) | x <- children ] -- build list of minimax values for TRUE/FALSE maxPlayer


-- Finds the index of an item in a list

itemfinder' :: [Int] -> Int -> Int -> Int
itemfinder' (a:ax) el counter
    | a == el = counter
    | otherwise = itemfinder' ax el (counter + 1)

