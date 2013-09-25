-- TODO:
-- * Being able to read categories by listing files names
-- * Validate that [TrainedCategory] does not contain a duplicate category?
-- * Being able to train a category from a file

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

totalTrainingSet :: [TrainedCategory] -> Int
totalTrainingSet (x:xs) = length(dataFromTrainedCategory(x)) + totalTrainingSet(xs)
totalTrainingSet [] = 0

prior :: (TrainedCategory, [TrainedCategory] ) -> Float
prior (t,ts) = fromIntegral( length(dataFromTrainedCategory(t))) / fromIntegral(totalTrainingSet(ts))

-- TODO: add laplace Smoothing
--
likelihood :: String -> TrainedCategory -> Float
likelihood w tc = sProb(w, dataFromTrainedCategory(tc))

cprob :: String -> TrainedCategory -> [TrainedCategory] -> (Category, Float)
cprob w tc tcs = ( categoryFromTRainedCategory(tc), prior(tc, tcs) * likelihood w tc)

reducingNBayes :: String -> [TrainedCategory] -> [TrainedCategory] -> [(Category, Float)]
reducingNBayes w [] tcs' = []
reducingNBayes w tcs tcs' = cprob w (head tcs) tcs' : reducingNBayes w (tail tcs) tcs'

nBayes :: String -> [TrainedCategory] -> [(Category, Float)]
nBayes s tcs = reducingNBayes s tcs tcs

--------------------------
-- Naive Bayes Classifier
--------------------------
-- p(d|c) p(c)
-- -----------------------
-- p(c) prior
-- p(d|c) likelihood
--------------------------
