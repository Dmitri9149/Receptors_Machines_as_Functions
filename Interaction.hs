{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interaction where

-- the configuration : there is 'gas' of particles of type 'a' 
-- particles may interact : there is function 'interact' which 
-- take two particles of type 'a' and may be produce a new particle 
-- of type 'a' -> pt1 'interact' pt2 (similar to evaluate function)
-- we have a list ['a'] ('gas') paticles -> describe the 
-- evolution of the 'gas' : two particles are consumed and 
-- one particle is produced 

interact :: a -> a -> Maybe a
interact = undefined

-- helper function for one_elem_with_list function 
one_elem_with_list_helper:: a -> (a -> a -> Maybe a) -> [a] -> [a] -> ([a], [a])
one_elem_with_list_helper x interact lst acc =
  case lst of
    [] -> ([x], reverse acc)
    [y] -> case interact x y of
     Nothing -> ([x], reverse $ y : acc)
     Just z -> ([], reverse $ z:acc)
    y:xs -> case interact x y of
      Just k -> ([] , reverse $ reverse xs ++ [k] ++ acc)
      Nothing -> one_elem_with_list_helper x interact xs (y:acc)
-- take element xx of type a ; interacion function , list of type [a]
-- find the first element in list which can interact with the xx 
-- res = interact xx elt /= Nothing , change the elt to res at the element position 
-- and return the result 
-- return tuple : fst = one element list [xx] , snd = original lst with elt changed to 
-- res (result of interaction) 
-- if there was no interaction -> the original list lst is returned unchanged 
one_elem_with_list :: a -> (a -> a -> Maybe a) -> [a] -> ([a], [a])
one_elem_with_list x interact lst = one_elem_with_list_helper x interact lst []

-- example 
interact_Ord :: Int -> Int -> Maybe Int
interact_Ord x y = if x < y then Just $ x + y else Nothing

ex1 :: ([Int], [Int])
ex1 = one_elem_with_list 2 interact_Ord [-5, -6, 7 , -8, -10, 100]

ex2 :: ([Int], [Int])
ex2 = one_elem_with_list 2 interact_Ord [-5, -6, - 7 , -8, -10, -100, 0,1]


evolution_helper :: (a -> a -> Maybe a) -> [a] -> [a] -> [a]
evolution_helper interact lst acc = case lst of
  [] -> acc
  [x] -> x:acc
  [x, y] -> case interact x y of
    Nothing -> x:y:acc
    Just z -> z:acc
  x:y:xs -> case one_elem_with_list x interact (y:xs) of
    ([], res1) -> reverse $ reverse res1 ++ acc
    (r,res2) -> evolution_helper interact res2 $ head r:acc

evolution :: (a -> a -> Maybe a) -> [a] -> [a]
evolution interact lst = evolution_helper interact lst []

ex3 :: [Int]
ex3 = evolution interact_Ord  [-5, -6, - 7 , -8, -10, -100, 0, 1]


ex4 :: [Int]
ex4 = evolution interact_Ord  [ -6, -11, - 7 , -8, -10, -100, 30, 1, 8]

ex5 :: ([Int], [Int])
ex5 = one_elem_with_list_helper 2 interact_Ord [-5, -6, 7 , -8, -10, 100] []

ex6 :: ([Int], [Int])
ex6 = one_elem_with_list_helper 2 interact_Ord [-5, 7, -6] []

ex7 :: [Int]
ex7 = evolution interact_Ord  [ 32, -11, - 7 , -8, -10, -100, 30]

ex8 :: [Int]
ex8 = (evolution interact_Ord . reverse . evolution interact_Ord)  [ 32, -11, - 7 , -8, -10, -100, 30]

main :: IO ()
main = do
  let res = ex8
  print res

-- ex1 -> output : ([],[100,-10,9,-8,-6,-5])
-- ex2 -> output : ([2],[1,0,-100,-10,-8,-7,-6,-5])





