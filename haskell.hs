module Binary where 
import Data.Char

data Tree a = Empty | Node a ( Tree a) (Tree a)
search :: Ord a => Eq  a => a -> Tree a -> Bool 
search x tree  = case tree of 
  Empty -> False 
  Node z ( yyy ) ( zzz) -> if z == x 
    then True 
    else if z < x then search x yyy else search x zzz

elim :: String -> String -> String 
elim str acc = case str of 
  []  -> []
  x:[] -> [x]
  x:(xx:xs) -> if (toUpper x) == xx then elim xs acc else elim xs (xx : (x : acc))

final_elim :: String -> String 
final_elim str = reverse (elim str []) 

 
main :: IO ()
main = do 
  let res = final_elim ['A', 'a', 'c']
  print res

