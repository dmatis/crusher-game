data Piece = D | W | B deriving (Eq, Show)
type Point = (Int, Int)
type Tile  = (Piece, Point)
type Board = [Piece]
type Grid = [Point]
type State = [Tile]

--boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
--boardEvaluator player history n board myTurn = -- To Be Completed

boardEvaluator :: Piece -> Board -> Int
boardEvaluator player board = countPieces player board 0
  
  
countPieces :: Piece -> Board -> Int -> Int
countPieces player board counter 
  | (null board)             = counter     
  | (head board) == player = countPieces player (tail board) (counter + 1)
  | otherwise              = countPieces player (tail board) counter

