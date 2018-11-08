--- file: tar_2.hs
--- abuhay abune 319311759

-------------------------------sudoku-----------------------------------------------------
type Matrix a = [[a]]
type Board    = Matrix Char
blank :: Char -> Bool
blank = (='.')

correct   :: Board -> Bool
correct b = all nodups (rows b) &&
            all nodups (cols b) &&
            all nodups (boxs b)

nodups       :: (Eq a) => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs


rows :: Matrix a -> Matrix a
rows = id


cols          :: Matrix a -> Matrix a
cols [xs]     = [[x] | x <-xs]
cols (xs:xss) = zipWith (:) xs (cols xss)


boxs :: Matrix a -> Matrix a
boxs = map ungroup.ungroup.map cols.group.map group

group   :: [a] -> [[a]]
group   = groupBy boxsize
ungroup :: [[a]] -> [a]
ungroup = concat

 
