stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

nouns :: [String]
nouns = ["man", "woman", "mutton", "beef", "trout"]

verbs :: [String]
verbs = ["eats", "thinks", "is"]

ngrams :: [[a]] -> [[a]]
ngrams [] = [[]]
ngrams (xs : xss) = [y : ys | y <- xs, ys <- ngrams xss]

trigramWords :: [Char] -> [Char] -> [String]
trigramWords ss vs = ngrams [ss, vs, ss]

trigramWordsP :: [Char] -> [Char] -> [String]
trigramWordsP ss vs = [w | w <- trigramWords ss vs, head w == 'p']

trigramSentences :: [String] -> [String] -> [[String]]
trigramSentences ns vs = ngrams [ns, vs, ns]

averageWordLength :: String -> Double
averageWordLength x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))