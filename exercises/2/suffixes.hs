suffixes [] = [[]]
suffixes s@(x:xs) = [s] ++ suffixes(xs)

