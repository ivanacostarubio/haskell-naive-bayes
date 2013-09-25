-- TODO: Being able to read categories from a file
data Category = Vegetables | Proteins
    deriving (Show)

data TrainedCategory = TrainedCategory Category [String]
    deriving (Show)

dataFromTrainedCategory :: TrainedCategory -> [String]
dataFromTrainedCategory (TrainedCategory cat xs) = xs

categoryFromTRainedCategory :: TrainedCategory -> Category
categoryFromTRainedCategory (TrainedCategory c xs) = c

sProb :: (String, [String]) -> Float
sProb (x, xs)= fromIntegral(length(filter(==x) xs)) / fromIntegral (length xs)

categoryProb :: (String, TrainedCategory) -> (Category, Float)
categoryProb (x, t) = (categoryFromTRainedCategory(t), sProb(x, dataFromTrainedCategory t))

probDist :: (String, [TrainedCategory]) -> [ (Category, Float) ]
probDist(x, y:ys)  =  categoryProb(x, y)  : probDist(x, ys)
probDist(x,[]) = []


totalTrainingSet :: [TrainedCategory] -> Int
totalTrainingSet (x:xs) = length(dataFromTrainedCategory(x)) + totalTrainingSet(xs)
totalTrainingSet [] = 0

prior :: (TrainedCategory, [TrainedCategory] ) -> Float
prior (t,ts) = fromIntegral( length(dataFromTrainedCategory(t))) / fromIntegral(totalTrainingSet(ts))

-- TODO: add laplace Smoothing
--
likelihood :: String -> TrainedCategory -> Float
likelihood w tc = sProb(w, dataFromTrainedCategory(tc))


--------------------------
-- Naive Bayes Classifier
--------------------------
-- p(d|c) p(c)
-- -----------------------
-- p(c) prior
-- p(d|c) likelihood
--------------------------
