commonSubstring :: String -> String -> String
commonSubstring s1 s2 = css s1 s2 ""

css "" _ r = r
css _ "" r = r
css x@(c1:s1) y@(c2:s2) r
    | elem c1 y = css s1 (tail $ dropWhile (/=c1) y) (r ++ [c1])
    | elem c2 x = css (tail $ dropWhile (/= c2 ) x) s2 (r ++[c2])
    | otherwise = css s1 s2 r