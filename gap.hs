gap :: (Char, Char) -> Int -> String -> Int
gap = gap' 0
        where gap' r (c,h) n s 
                | length s < n  + 2 = r
                | otherwise         = gap' (r + if head s == c && s !! (n + 1) == h then 1 else 0) (c, h) n (tail s) 