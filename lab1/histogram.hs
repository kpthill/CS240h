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

--reads a string and produces a map from each word to the number of
--times that word appeared in the string
makeHistogram :: String -> M.Map String Int
makeHistogram s = words s |> loop M.empty
  where
    loop m [] = m
    loop m (x:xs)
      | M.member x m = loop (M.insertWith' (+) x 1 m) xs
      | otherwise = loop (M.insert x 1 m) xs

--takes a map of words to counts and creates a histogram
makePicture :: M.Map String Int -> String
makePicture m =
  M.toList m |> L.sortBy (comparing snd) |> L.map makeLine |> join "\n"
    where
      join sep = foldl (\t h -> h ++ sep ++ t) ""
      makeLine (word, cnt) =
        replicate (10 - length word) ' ' ++ word ++ ": " ++ replicate cnt '*'
        
main = do
  (fpath:_) <- getArgs
  s <- readFile fpath
  map toLower s |> makeHistogram |> makePicture |> putStr