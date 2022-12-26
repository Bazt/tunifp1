distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter r r0 s = filter (\t -> r s t <= r0)