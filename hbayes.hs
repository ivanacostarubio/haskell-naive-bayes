sProb :: String -> [String] -> Float
sProb x xs = fromIntegral(length(filter(==x) xs)) / fromIntegral (length xs)
