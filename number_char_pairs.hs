charsDivisibleBy :: Int -> [Char]
charsDivisibleBy n = [c | (c, i) <- zip ['a' .. 'z'] [1 ..], mod i n == 0]

charsProductOf :: [Int] -> [Char]
charsProductOf ns = [c | (c, i) <- zip ['a' .. 'z'] [1 ..], elem i [x * y | x <- ns, y <- ns, x /= y]]

