onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits s = all (\c -> elem c ['0'..'9']) s