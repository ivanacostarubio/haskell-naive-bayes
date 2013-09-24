-- TODO: Being able to read categories from a file
data Category = Vegetables | Proteins
    deriving (Show)

data TrainedCategory = TrainedCategory Category [String]
    deriving (Show)

dataFromTrainedCategory :: TrainedCategory -> [String]
dataFromTrainedCategory (TrainedCategory cat xs) = xs

sProb :: (String, [String]) -> Float
sProb (x, xs)= fromIntegral(length(filter(==x) xs)) / fromIntegral (length xs)

categoryProb :: (String, TrainedCategory) -> Float
categoryProb (x, t) = sProb(x, dataFromTrainedCategory t)
