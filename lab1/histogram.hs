import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Ord
import System.IO
import System.Environment

--a substitute for the standard unix pipe operator
infixl 9 |>
(|>) :: a -> (a -> b) -> b
(|>) r g = g r

--splits a string on whitespace just like the standard function words,
--but also strips punctionation from the beginning and end of the
--words
myWords :: String -> [String]
myWords s = words s
            |> L.map stripPunctuation
            |> L.filter (\w -> length w > 0)
              where
                stripPunctuation s = reverse . loop . reverse $ loop s
                loop [] = []
                loop w@(l:ls)
                  | not $ isAlpha l = loop ls
                  | isAlpha l = w

--reads a string and produces a map from each word to the number of
--times that word appeared in the string
makeHistogram :: String -> M.Map String Int
makeHistogram s = myWords s |> loop M.empty
  where
    loop m [] = m
    loop m (x:xs)
      | M.member x m = loop (M.insertWith' (+) x 1 m) xs
      | otherwise = loop (M.insert x 1 m) xs

--takes a map of words to counts and creates a histogram
makePicture :: M.Map String Int -> String
makePicture m = L.sortBy (comparing snd) l |> L.map (makeLine max_len max_cnt) |> join "\n"
  where
    l = M.toList m
    max_len = length $ fst $ L.maximumBy (comparing $ length . fst) l
    max_cnt = snd $ L.maximumBy (comparing snd) l
    
--concatenates the strings in the list, separated by sep
join :: String -> [String] -> String
join sep = foldl (\t h -> h ++ sep ++ t) ""
        
--makes a line for a single word and its count
makeLine :: Int -> Int -> (String, Int) -> String
makeLine i max (word, cnt)
  | cnt <= 0 = error "count should never be less than 1"
  | cnt > 0 = 
    word ++ replicate numspaces ' ' ++ replicate (numhashes cnt max (79 - i)) '#'
      where 
        numspaces = i + 1 - length word
        
--finds the number of hashes necessary in a line for a word with cnt
--appearances, given that the most frequest word appeared maxCnt times
--and space is the maximum number of characters remaining on the line
numhashes :: Int -> Int -> Int -> Int
numhashes cnt maxCnt space
  | maxCnt <= space = cnt
  | cnt * space <= maxCnt = 1
  | otherwise = round $ fromIntegral space * fromIntegral cnt / fromIntegral maxCnt
        
main = do
  paths <- getArgs
  s <-if length paths == 0
      then do
        putStrLn "type text all in one line, then hit return:"
        getLine
      else do
        loop paths ""
  map toLower s |> makeHistogram |> makePicture |> putStr
    where
      loop :: [String] -> String -> IO String
      loop (p:paths) s = do
        text <- readFile p
        loop paths (s ++ "\n" ++ text)
      loop [] s = return s