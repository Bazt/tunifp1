distance1 :: String -> String -> Float
distance1 s1 s2
    | s1 == "" && s2 == ""  = 0
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

d::Float
d=0.2
s_=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"]
tt = [[""],["a","aa"],["a","aa","aaabc"],["aa","aaabc","aabdd","bcbcb"],["aaabc","aabdd","abdd"],["aaabc","bcbcb"],["aabdd","abdd"],["abcdefghij"]] 
    

s_2 =["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"] 
tt2= [[],[],[],["123a","45","456789b","a12"],["123a","45","456789b","a12"],["45","456789b"],["45","456789b"]]

clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]

clusters r r0 ss = [ [t | t <- ss, r s t <= r0] | s <- ss ]
