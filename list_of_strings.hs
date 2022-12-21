-- evaluates to a list with all the strings of the input list that either begin or end with the input character.
headOrLast :: [String] -> Char -> [String]
headOrLast ss c = [s | s <- ss, head s == c || last s == c]