--files that I copied over in order to test
type Point = (Int, Int)
type Slide = (Point,Point)
type Grid = [Point]

grid0 = generateGrid 3 2 4 []

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1
--end of files that I copied over


--creates a list of possible slides from every position on the board

--To Test, call:
--generateSlides grid0 sizeGrid
--ie generateSlides grid0 3 
generateSlides :: Grid -> Int -> [Slide]
generateSlides b n = concat [genSlidesHelper b pt (genPointsHelper pt n) | pt <- b ]


--generates a list of all valid moves from a given point
genPointsHelper :: Point -> Int -> [Point]
genPointsHelper pt n
    | ((snd pt) < n-1)     = ((fst pt)+1,(snd pt)) : ((fst pt)-1,(snd pt)) :
                             ((fst pt), (snd pt) + 1) : ((fst pt),(snd pt)-1) :
                             ((fst pt)-1,(snd pt)-1) : ((fst pt)+1,(snd pt)+1) : []

    | ((snd pt) == n-1)    = ((fst pt)+1,(snd pt)) : ((fst pt)-1,(snd pt)) :
                             ((fst pt), (snd pt) + 1) : ((fst pt),(snd pt)-1) : 
                             ((fst pt)-1,(snd pt)-1) : ((fst pt)-1,(snd pt)+1) : []
                             
    | otherwise            = ((fst pt)+1,(snd pt)) : ((fst pt)-1,(snd pt)) :
                             ((fst pt), (snd pt) + 1) : ((fst pt),(snd pt)-1) :
                             ((fst pt)+1,(snd pt)-1) : ((fst pt)-1,(snd pt)+1) : []

genSlidesHelper :: Grid -> Point -> [Point] -> [Slide]
genSlidesHelper b oldPt points = [(oldPt, newPt) | newPt <- points, newPt `elem` b]
                            

