myWords :: String -> [String]
myWords [] = []
myWords (' ' : cs) = myWords cs
myWords cs = (takeWhile (/= ' ') cs) : myWords (dropWhile (/= ' ') cs)