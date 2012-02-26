import Data.List
import Data.Char

data Cell = X | O | Empty
    deriving (Eq, Ord)

instance Show Cell where
    show X = "X"
    show O = "O"
    show Empty = " "

opponent :: Cell -> Cell
opponent X = O
opponent O = X
opponent _ = undefined

data Board = Board [[Cell]]
    deriving (Eq)

instance Show Board where
    show (Board b) = '\n' : (intercalate "\n-+-+-\n" [intercalate "|" (map show row) | row <- b] ++ "\n")

newBoard = Board (replicate 3 (replicate 3 Empty))

spans :: Board -> [[Cell]]
spans (Board b) = b ++ transpose b ++ diags
                    where diags = [[b!!i!!i | i <- [0..(length b) - 1]],[b!!i!!((length b)-i-1) | i <- [0..(length b) - 1]]]

type Score = (Int,Int,Int)

score :: Board -> Score
score b = tsum $ map (scoreSpan . sort) (spans b)
                    where scoreSpan (X:X:X:[]) = (1,0,0)
                          scoreSpan (O:O:O:[]) = (-1,0,0)
                          scoreSpan (X:X:Empty:[]) = (0,1,0)
                          scoreSpan (O:O:Empty:[]) = (0,-1,0)
                          scoreSpan (X:Empty:Empty:[]) = (0,0,1)
                          scoreSpan (O:Empty:Empty:[]) = (0,0,-1)
                          scoreSpan (_:_:_:[]) = (0,0,0)
                          scoreSpan _ = error "not a span"
                          tsum = foldl1 tadd
                          tadd (a,b,c) (d,e,f) = (a+d,b+e,c+f)

movesFor :: Cell -> Board -> [Board]
movesFor cell (Board b) = let movesInRow cell []         = []
                              movesInRow cell (Empty:cs) = (cell:cs) : map (Empty:) (movesInRow cell cs)
                              movesInRow cell (c:cs)     = map (c:) (movesInRow cell cs)
                              movesInRows cell []         = []
                              movesInRows cell (row:rows) = (map (:rows) (movesInRow cell row)) ++ (map (row:) (movesInRows cell rows))
                          in map Board (movesInRows cell b)

compareScores :: Cell -> Score -> Score -> Ordering
compareScores X x y = compare x y
compareScores O x y = compare y x

bestMoveFor :: Cell -> Board -> (Board,Score)
bestMoveFor player b = let candidates = movesFor player b
                           compareOutcomes X x y = score x `compare` score y
                           compareOutcomes O x y = score y `compare` score x
                           best = maximumBy (compareOutcomes player) candidates
                       in (best, score best)

minimax ::  Int -> Cell -> Board -> (Board,Score)
minimax ply player b | ply == 1  = bestMoveFor player b
                     | otherwise = let candidates = movesFor player b
                                       for = flip map
                                       eventualCandidates = for candidates (\c ->
                                            if isGameOver c then (c, score c)
                                                            else (c, snd $ minimax (ply-1) (opponent player) c))
                                       compareOutcomes x y = compareScores player (snd x) (snd y)
                                       best = maximumBy compareOutcomes eventualCandidates
                                   in best

replaceAt :: (Int,Int) -> a -> [[a]] -> [[a]]
replaceAt (x,y) replacement list = (take y list) ++ replacementRow : (drop (y+1) list)
                                   where replacementRow = (take x row) ++ replacement : (drop (x+1) row)
                                         row = list!!y

applyInput :: Board -> Cell -> Char -> Either String Board
applyInput (Board b) player input = if input `elem` ['1'..'9']
                                    then let digit = (ord input) - (ord '1')
                                             height = length b
                                             width = length $ head b
                                             x = digit `mod` width
                                             y = (height - 1) - (digit `div` height)
                                         in if (b!!y!!x) == Empty
                                            then Right $ Board $ replaceAt (x,y) player b
                                            else Left "You can only play in an empty square"
                                    else Left "Position to play must be one of the digits 1..9"

isGameOver :: Board -> Bool
isGameOver b = (hasPlayerWon b /= Empty) || Empty `notElem` (concat b')
               where Board b' = b

hasPlayerWon :: Board -> Cell
hasPlayerWon b = let (rowsofthree,_,_) = score b
                 in case (rowsofthree `compare` 0) of
                        LT -> O
                        GT -> X
                        EQ -> Empty

ifGameContinues b f = if (isGameOver b) then
                            if (hasPlayerWon b) /= Empty then
                                "A glorious win for " ++ (show $ hasPlayerWon b) ++ "!"
                                else "A strange game. The only way to win is not to play."
                        else f b

computerFirst :: Board -> String -> String
computerFirst b input = ifGameContinues b (\x -> let b' = fst $ minimax 2 X x
                                                 in humanFirst b' input)

humanFirst :: Board -> String -> String
humanFirst b input = show b ++ "> " ++ ifGameContinues b (\x -> case applyInput b O (head input) of
                                                  Left errorMsg -> '\n':errorMsg ++ "\n" ++ humanFirst x (tail input)
                                                  Right b' -> (show b') ++ (computerFirst b' (tail input)))

ioLoop :: String -> String
ioLoop inputs = humanFirst newBoard inputs ++ "\n"

