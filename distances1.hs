distance1 :: String -> String -> Float
distance1 s1 s2
    | s1 == "" || s2 == ""  = 0
    | otherwise             =  (s1_s2 + s2_s1) / s1s2
        where   s1_s2 = fromIntegral $ length [c | c <- s1, notElem c s2]
                s2_s1 = fromIntegral $ length [c | c <- s2, notElem c s1]
                s1s2  = fromIntegral $ length s1 + length s2

distance2 :: String -> String -> Float
distance2 s1 s2
    | s1 == "" || s2 == ""  = 0
    | otherwise             =  (s1_char + s2_char) / s1s2
        where   digits = ['0' .. '9']
                s1_char = fromIntegral $ length [c | c <- s1, notElem c digits]
                s2_char = fromIntegral $ length [c | c <- s2, notElem c digits]
                s1s2  = fromIntegral $ length s1 + length s2