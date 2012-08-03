{-# LANGUAGE TemplateHaskell #-}

import Data.List
import Data.Char
import System.IO
import Test.QuickCheck
import Test.QuickCheck.All


-- | A cell in the board.
data Cell = X | O | Empty
    deriving (Eq, Ord)

instance Show Cell where
    show X = "X"
    show O = "O"
    show Empty = " "

prop_cell_show_length :: Cell -> Bool
prop_cell_show_length c = length (show c) == 1

instance Arbitrary Cell where
    arbitrary = elements [X, O, Empty]

-- | The opposing player of a cell: only for occupied cells.
opponent :: Cell -> Cell
opponent X = O
opponent O = X
opponent _ = undefined

-- | A board is a list of lists of cells, 3x3 in size.
data Board = Board [[Cell]]
    deriving (Eq)

-- Print out a board in the traditional line-drawing style.
instance Show Board where
    show (Board b) = '\n' : (intercalate "\n-+-+-\n" [intercalate "|" (map show row) | row <- b] ++ "\n")

instance Arbitrary Board where
    arbitrary = do a <- vector 3
                   b <- vector 3
                   c <- vector 3
                   return $ Board [a, b, c]

-- | The list of all cells, in no particular order, in a board.
cells :: Board -> [Cell]
cells (Board b) = concat b

prop_cells b = length (cells b) == 9

-- | Generate a fresh, empty board.
newBoard = Board (replicate 3 (replicate 3 Empty))

-- | A list containing all the spans of a board; that is, each row, column, and diagonal crossing the whole board.
spans :: Board -> [[Cell]]
spans (Board b) = b ++ transpose b ++ diags
                    where diags = [[b!!i!!i | i <- [0..(length b) - 1]],[b!!i!!((length b)-i-1) | i <- [0..(length b) - 1]]]

prop_spans_length b = length (spans b) == 8

prop_spans_row_length b@(Board b') = all (\r -> length r == rowlength) (spans b)
                                     where rowlength = length b'

-- | A score representing the desirability of the board situation. The first
-- element gives the number of winning lines (i.e. three of the same symbol);
-- the second element is the number of lines containing two of the same symbol
-- and a space; the third, one of the same symbol and two spaces. Wins for X
-- are positive and wins for O negative.
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

prop_movesFor_length player b = length (movesFor player b) == length (filter (== Empty) (cells b))

compareScores :: Cell -> Score -> Score -> Ordering
compareScores X x y = compare x y
compareScores O x y = compare y x

prop_compareScores_antisymmetric s1 s2 = compareScores X s1 s2 == compareScores O s2 s1
prop_compareScores_reflexive s = (compareScores X s s == EQ) && (compareScores O s s == EQ)
prop_compareScores_transitive s1 s2 s3 = case (compareScores X s1 s2) of
                                            EQ -> (compareScores X s1 s3) == (compareScores X s2 s3)
                                            LT -> ((compareScores X s2 s3) == GT) || ((compareScores X s1 s3) == LT)
                                            GT -> ((compareScores X s2 s3) == LT) || ((compareScores X s1 s3) == GT)

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

runtests :: IO ()
runtests = do
    $quickCheckAll
    return ()

main :: IO ()
main = hSetBuffering stdin NoBuffering >> interact ioLoop
