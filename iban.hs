validate :: String -> Bool
validate s
    | length s /= 18 = False
    | "FI" /= take 2 s = False
    | any (\c -> notElem c ['0'..'9']) $ drop 2 s  = False
    | otherwise = mod ibanToNum 97 == 1
        where
            prefix = take 2 s
            digits = drop 2 s
            digits1 = take 2 digits
            digits2 = drop 2 digits
            charToNumStr c = show $ fromEnum c - fromEnum 'A' + 10
            prefix_s = concat $ map charToNumStr prefix
            ibanToNum = read (digits2 ++ prefix_s ++ digits1) :: Integer