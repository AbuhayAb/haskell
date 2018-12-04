--- file: tar_2.hs
--- abuhay abune

-------------------------------sudoku--------------------------------------------------------------------
type Matrix a = [Row a]
type Row a    = [a]
type Choices  = [Digit]
type Grid    = Matrix Digit
type Digit    = Char
digits :: [Digit]
digits = ['1' .. '9']
blank :: Digit -> Bool
blank = (=='0')
boardsize = 9
boxsize = 3

nodups       :: (Eq a) => [a] -> Bool
nodups []     = True
nodups (x:xs) = all (/=x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols          :: Matrix a -> Matrix a
cols [xs]     = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . 
       map cols . 
       group . map group

group    :: [a] -> [[a]]
group [] = []
group xs = take boxsize xs : group (drop boxsize xs)

ungroup :: [[a]] -> [a]
ungroup = concat

choices  :: Grid -> Matrix [Digit]
choices   = map (map choice)
            
choice d = if blank d then digits else [d] 


single :: [a] -> Bool
single [_] = True
single _   = False

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

expand :: Matrix [Digit] -> [Matrix [Digit]]
expand rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2| c <- cs]
               where 
                (rows1, row:rows2) =  break (any smallest) rows
                (row1, cs:row2)    =  break smallest row
                smallest cs        = length cs == n
                n                  = minimum (counts rows)
                counts             = filter (/= 1) . map length . concat

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
        where pruneBy f = f . map pruneRow . f

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]

complete :: Matrix Choices -> Bool
complete = all (all single)


valid :: Matrix Choices -> Bool
valid cm = all ok (rows cm) &&
           all ok (cols cm) &&
           all ok (boxs cm)
ok row   = nodups [x | [x] <- row]

extract :: Matrix [Digit] -> Grid
extract = map (map head)

solve :: Grid -> [Grid]
solve = search . choices
search :: Matrix Choices -> [Grid]
search cm
  | not (valid pm)  = []
  | complete pm    = [extract pm]
  | otherwise      = concat (map search (expand pm))
  where pm = prune cm

mysolve :: Grid -> IO()
mysolve a = putStrLn $ unlines $ head $ solve a

sudoku = ["200001038",
          "000000005",
          "070006000",
          "000000013",
          "098100257",
          "310000800",
          "900800020",
          "050069784",
          "400250000"]


--------------------------------------Make a tree that describes mathematical expressions ---------------

data ETree a = CONSTANT a
             | X 
             | PLUS (ETree a) (ETree a) 
             | MINUS (ETree a) (ETree a) 
             | TIMES (ETree a) (ETree a) 
             | OBELUS (ETree a) (ETree a) 
             | POW (ETree a) (ETree a) 
             | NEGATE (ETree a) 
             | SIN (ETree a) 
             | COS (ETree a) 
             | Empty
             deriving (Show, Read)


eval :: (Floating a, Eq a) => ETree a -> a -> Maybe a
eval X            x = if x /= 0 then Just x else Nothing
eval (CONSTANT a) _ = if a /= 0 then Just a else Nothing
eval (PLUS a b)   x = (+)   <$> (eval a x) <*> (eval b x)
eval (TIMES a b)  x = (*)   <$> (eval a x) <*> (eval b x)
eval (MINUS a b)  x = (-)   <$> (eval a x) <*> (eval b x)
eval (OBELUS a b) x = (/)   <$> (eval a x) <*> (eval b x)
eval (POW a b)    x = (**)  <$> (eval a x) <*> (eval b x)
eval (SIN a)      x = (sin) <$> (eval a x)
eval (COS a)      x = (cos) <$> (eval a x)

diff ::(Floating a) => ETree a -> ETree a
diff (CONSTANT a) = (CONSTANT 0)
diff X            = (CONSTANT 1)
diff (PLUS a b)   = PLUS (diff a) (diff b)
diff (MINUS a b)  = MINUS (diff a) (diff b)
diff (TIMES a b)  = PLUS (TIMES (diff a) b) (TIMES a (diff b))
diff (OBELUS a b) = OBELUS (MINUS (TIMES (diff a) b) (TIMES a (diff b))) (POW b (CONSTANT 2))
diff (POW a b)    = TIMES (TIMES b (POW a (MINUS b (CONSTANT 1) ))) (diff a)
diff (SIN a)      = COS a
diff (COS a)      = NEGATE (SIN a)



