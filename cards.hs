-- ‘s’ means spades, 
-- ‘h’ hearts, 
-- ‘c’ clubs’ and 
-- ‘d’ diamonds, 
-- with number values going from 2 to 14 (Ace being 14).
-- Consider a game, where a player is dealt two cards and wins credits based on the following rules:

-- If the player has the Ace of Spades (‘s’, 14), then the player wins 14 credits.
-- Otherwise if the player has two consecutive numbers of the same suit, then the player wins 8 credits.
-- Otherwise if the player has a pair (same number values), then the player wins 6 credits.
-- Otherwise if the player has to consecutive numbers, then the player wins 4 credits.
-- Otherwise if the player has two cards of the same suit, then the player wins 2 credits.
-- Otherwise, the player wins 0 credits.

credits :: (Char, Int) -> (Char, Int) -> Int 
credits f@(c, n) s@(h, m) 
    | ('s', 14) `elem` [f, s]       = 14
    | c == h && abs (n-m) == 1  = 8
    | n == m                    = 6
    | abs (n-m) == 1            = 4
    | c == h                    = 2
    | otherwise                 = 0 