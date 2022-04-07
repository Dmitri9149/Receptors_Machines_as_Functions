{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Receptors where

-- thise are 'draft' thoughts , computationally the 'particle 
-- physics below' and spins are low level computations with 
-- registers : many simple registers -> complex connections 
-- between registers 
-- 'spins' are digit like structures which target the evaluations 
-- in the 'gas' of molecules ('receptors' or registers) 
-- such indexes have to arise from a DSL language 
--(not yet known) : but see my experiments with 
-- 2-Dimentional Lambda calculus in Two_D_Lambda.hs 
-- (simple examples of values of Session Types)

-- we will do 'particle physics' below 
-- we will make calculation with 'clever indexing' 
-- I name the indexes as spins as in quantum fields theory   
-- where spin numbers are used actually for the same reasons : 
-- to target interactions between particles 

-- in the receptors computations the basic receptors types are very 
-- simple like (), Bool ( 2 valued set) etc.. 
-- this is the same simplicity as a machine register simplicity : 
-- the rigisters  are very simple : they keeps only 0 or 1 value.
-- but there are billions on such registers with very complex linking topology 
-- between them 
-- same way in cell biology our simple receptors are linked by 'clever shaping' 
-- which we will transform in 'clever' indexing -> the indexing will arise from the 
-- topology of interaction 


-- We will start with model of Session machines : machines which implement 
-- Session Types. These are bundles of receptors. Such machines are responsible 
-- for interaction between two (or even more) cells and correspond to 'human made' 
-- values of session Types. 
-- Haskell is used for the modelling ( but later most probably I will switch to Idris)

-- we will construct particles (receptors) and Laws of Univerce : which will be some predicates 
-- to interact (every interaction will be evaluation of some function) the particles 
-- are to correspond to Laws of Universe ; otherwise an interaction is not possible 

-- there will be Action function and principle : collection of results of 
-- interactions at every step, glued by && between each other -> so, in case of an valid 
-- interaction the total result of the collection is to be True 

-- Some basic types 

-- the name is from Birth / Death (or Rise / Low ) 
-- there are sources and sinks which correspond to value generation
-- and value substitution to functions
-- second elt in (Int, Int) part correspond to a Type (like () or Bool or Int)
-- there is to be function from Int to types ( with kind *) to enumerate Types 
-- such function exists because there only a few basic Types which are used in 
-- cell machines, so such function may exist 
-- but in Haskell we can not make a function from Int to Types 
-- (it is possible in Idris)
-- we will just keep the function in our mind : 
-- 0 -> ()
-- 1 -> Bool 
-- 2 -> Int 
-- 3 -> Char
-- 4 -> String 
-- ..... yet undefined 

-- we will do it as function from Int to String 
-- and out type checking for type correspondence will use 
-- checking for equality of strings 
int_to_types :: Int -> String 
int_to_types 0 = "()"
int_to_types 1 = "Bool"
int_to_types 2 = "Int"
int_to_types 3 = "Char"
int_to_types 4 = "String"
int_to_types _ = undefined

-- from Source to Sink 
-- the length (mass) of the list is to be >= 1
-- the list of length = 1 correspond to a value (totally evaluated function)
-- in this case the fst elt in the pair is the 'coordinate' of the 
-- function which will consume the value
-- if length > 1 the fst elt correspond to 'coordinate' of value 
-- which will be substituted into the function 
-- the snd elt in the pair correspont to the Type of data 
-- here it is represented by Int number 
-- by coordinate we mean that the values and functions will be collected 
-- to bundles (lists) for session types evaluation 
-- the coordinate is position in the list and there will be 
-- send / receive configuration , so at every step there will be 
-- possibility for evaluation 
type Spin_SS = (Int,Int)


-- ST is from Space / Time : it is Nat valued position in the 
-- list of receptors in our Session machine ( see later)
type Spin_ST = Int

-- we may have receptors which are sticked in the membrane from Outside 
-- to Inside (will name as Down) and from Inside to Outside 
-- (will name it Up)
-- we actually are in need of a set {0,1}  by modulo 2 ... 
-- but this is OK ...
data Spin_UD = Up | Down deriving Eq

up_down :: Spin_UD -> Spin_UD
up_down Up = Down
up_down Down = Up

-- Our particle is some clever collection of spins 
-- which actually correspond to our 'clever enumeration' 
-- which arise from out configutation of interacting compartments 
-- from topology of our configuration of interactions (our geometry) 
data Receptors = Receptor { ud :: Spin_UD,
                            st :: Spin_ST,
                            sss :: [Spin_SS] }

-- mass of a receptor : the mass is to be >= 0 
-- mass of a value is 0 -> the value corresoind to a list of length 1 
-- remind as we take the final result as a value from stack of lenght 1 
-- the mass of functions is > 0
-- so we use the eager evaluation; we will not glue together unevaluated 
-- functions 
-- the interaction is transmitted by 
-- particles of mass 0 (values, or 'generalised photons') between particles of mass > 0 which are 
-- functions 
-- we do the 'downshift' from the lenght of the list by 1 to get the mass of a particle 
mass :: Receptors  -> Int
mass p = length (sss p) -1

val1 :: Receptors
val1 = Receptor {ud = Up, st = 3, sss = [(1,3),(2,3),(3,3)]}

val2 :: Receptors
val2 = Receptor {ud = Down, st = 1, sss = [(3,3)]}

-- this predicate correspond to our Laws of Universe 
-- not all particles can interact 
-- there are to be some correspondence between out spins 
-- this laws are valid for the 'horizontal' computations 
-- between particles at the same cell compartment  
lawsOfUniverse_1 :: Receptors -> Receptors -> Bool
lawsOfUniverse_1 tv@Receptor{ud = tv_ud, st = tv_st, sss =tv_sss }
  tf@Receptor{ud = tf_ud, st = tf_st, sss =tf_sss } =
  -- the left particle is to be a value -> partile of mass = 0
  mass tv == 0
  &&
  -- 'source' label in  tf has to be the same as 'sink' label in tv
  -- the Types of interaction have to be the same 
  -- in out case it means equality between snd elements  
  (tv_st == fst (head tf_sss)) && snd (head tv_sss) == snd (head tf_sss)
  &&
  -- mass of tf is to be at least 1 ( it is function)
  mass tf >= 1
  &&
  -- st is to be >= 0 , in the bundle (list) of receptors we enumerate 
  -- position starting from 0 
  (tf_st >= 0) && (tv_st >= 0)
  &&
  -- ud spin of value is to be Down and ud spin of tf is to be Up
  tv_ud == Down && tf_ud == Up
  -- some other laws may be added 

-- laws for interaction between particles of different 
-- compartments : only a value at one side may interact 
-- with receptor (actually id function) of another side (ud spins are to be opposite)
-- ud spin of value is to be Up 
-- first argument is value / second is id function 
lawsOfUniverse_2 :: Receptors -> Receptors -> Bool
lawsOfUniverse_2 tv@Receptor{ud = tv_ud, st = tv_st, sss =tv_sss }
  tf@Receptor{ud = tf_ud, st = tf_st, sss = tf_sss } =
-- spin of tv is Up , tf is Down 
  tv_ud == Up && tf_ud == Down
  &&
-- particles are to be in the same positions 
  tv_st == tf_st
  &&
-- masses 
  (mass tv == 0) && (mass tf == 1)
-- many other laws are to be added 
-- it forces again to use Idris with its dependent types 

--- to be continued :) 









