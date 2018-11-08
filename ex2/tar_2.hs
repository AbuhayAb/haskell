--- file: tar_2.hs
--- 

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

choice   :: Board -> Matrix Choices
choice   = map (map choose)

choose e = if blank e then cellvals else [e] 

mcp :: Matrix [a] -> [Matrix a]
mcp = cp.map cp

cp          :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [x : ys | x <-xs, ys <- cp xss]

sudoko :: Board -> [Board]
sudoko = filter correct.mcp.choices

fixed  :: [Choices] -> Choices
fixed  = concat.filter single
single :: [a] ->Bool

reduce       :: [Choices] -> Choices
reduce css   = map (remove.fixed css) css
reduce fx cs = if single cs then cs else delete fs cs


