nextIsGreater :: [Int] -> [Int]
nextIsGreater xs = [x | (x, next_x) <- zip xs (tail xs), x < next_x]