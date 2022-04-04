module Configuration_1 where
-- import Receptors 
-- try to get an intuition with working with more and more complex 
-- topological configurations 
-- the simplest configuration is used here : see picture 10 in README.md
-- there are values (list of length 1) and functions (list of length > 1)
-- we firstly number all the recetors (or molecules) by Int number and include it 
-- into our enumeration (spin system) 
-- and we are in need of two spins (both are Int , even Nat is enought)
-- let us say we have a value [(7,10)] 
-- it will means the value is numbered as 7 and will be consumed by function
-- numbered as 10 
-- [(7,10), (5,10), (3,10), (10,11)]
-- the function numbered as 10 will consume values numbered as 7, 5, 3 
-- (in this order) values and will send it result from it ( from 10) to 
-- function with number 11 
-- note the change of order at the last element 
-- first elemnts of the list are consumers 
-- the last element is like an origin of a value 
-- if we will concider all the receptors (molecules) as a set 
-- there is a big constrain on the enumeration !
-- so, there are laws in our Universe : laws of configuration 
-- How we can be assure that we will follow the laws when 
-- we work with the configurations ? It is not clear yet. This is 
-- a question for the future. Something like a structural programming 
-- may help. 
-----------------------------------------------------
-- spin from to to model the relations 
-- the meaning is clear : 
-- if particle has mass 0 : list length = 1 it works as a source 
-- of data : we send from fst x (current particle) to snd x , where fst x is current 
-- particle and snd x to which we send 
-- if particle has mass > 0 (list length is > 1) we receive from 
-- fst y and we are currently a particle with snd y 
-- the last element in the list is to be with 'reversed' configuration 
-- because it is not a 'consumer' but a 'producer' : it is a values 
--  
type Spin_FT = (Int, Int)

-- it seems we can simplify the spin system : 
-- for value (source) we can mention only at which position is has to be consumed 
-- for sinks (arguments) of a function : just mention the position 
-- so it will be [10] for a value and [10,10,10,10,25] for a function : 
-- the value [10] will ne consumed at function with number 10 
-- and the function after of evaluation will become a value 
-- which will be consumed at function with number [25]
-- so, we can simplify our spin system 
-- S from space : it just the particle numbering 
-- ops ! it is not enought : we have to keep the numbering of the sinks at every function !! 
-- like [(1,10), (2,10), (3,10), (4,10), (25,2)] 
-- the (1,10) - correspond to the first argument of the function 
-- the (2,10) - to the second urgument etc ...
-- (25,2) the resulting value will be consumed at second argument of the function 
-- with position 25  
-- PXT from coordinate : P and X from coordinate and impulse and T from tensor 
type Spin_PXT = (Int,Int)

data Receptors_1 = Receptor_1 { pxt :: [Spin_PXT] } deriving Show

val1 :: Receptors_1
val1 = Receptor_1 { pxt = [(1,10), (2,10), (3,10),(4,10),(25,2)]}

val2 :: Receptors_1
val2 = Receptor_1 { pxt = [(10,1)] }

val3 :: Receptors_1
val3 = Receptor_1 { pxt = [(10,2)]}

val4 :: Receptors_1
val4 = Receptor_1 { pxt = [(10,3)]}

val5 :: Receptors_1
val5 = Receptor_1 { pxt = [(10,4)]}


mass :: Receptors_1 -> Int
mass p = length (pxt p) - 1

lawsOfUniverse_1 :: Receptors_1 -> Receptors_1 -> Bool
lawsOfUniverse_1
  tv@ Receptor_1 { pxt = tv_pxt }
  tf@Receptor_1 {pxt = tf_pxt} =
-- mass of value is 0, like generalised 'photons' in String Theory 
-- the interaction is transmitted by massless particles 
-- the functions are massive particles , we do not allow linking of 
-- two functions , we have the eager evaluation 
    (mass tv == 0 && mass tf > 0)
    &&
-- correspondence between source and sinks : the value is targeted to a particle 
-- at some position (sink space position) and the position of the argument at the 
-- function 
    let head_tv = head $ pxt tv
        head_tf = head $ pxt tf in (fst head_tv == snd head_tf && snd head_tv == fst head_tf)

-- general pattern for interaction : if laws of Universe are valid 
-- between interacting particles : the particles may interact : 
-- it means function evaluation with the value 
-- if not: the interaction will generate nothing 
evaluate :: Receptors_1 -> Receptors_1 -> Maybe Receptors_1
evaluate
  tv@ Receptor_1 { pxt = tv_pxt }
  tf@Receptor_1 {pxt = tf_pxt}
  | lawsOfUniverse_1 tv tf = Just Receptor_1 {pxt = drop 1 tf_pxt }
  | otherwise = Nothing

main :: IO ()
main = do
  let res = evaluate val2 val1
  putStrLn (show res)

