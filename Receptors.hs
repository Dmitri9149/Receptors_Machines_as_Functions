{-# LANGUAGE ScopedTypeVariables #-}
module Receptors where
import Control.Arrow
import Data.Aeson (Value(Bool))

--  we will make calculation with 'clerer indexing' 
-- of course if we rely on indexing 'we do not understand something ...' 
-- and in the tasks we really do not understand somethig 
-- but the clerer indexing is a good and unserstandable exercise 
-- we will name our digits 'spins' in relation with physics 
-- where spin numbers are used actually for the same reasons : 
-- to target interacting particles 

-- in the receptors computations the basic receptors types are very 
-- simple like (), Bool ( 2 valued set) etc.. 
-- this is the same simplicity as a machine register is also very simple : 
-- it keeps only 0 or 1 value.
-- but there are billions on such registers with clever linking between each 
-- other
-- in cell biology our somple receptors are linked by 'clever shaping' 
-- which we will transform in 'clever' indexing 


-- We will mostly model Session machines : machines which implement 
-- Session Types. Such machines are responsible for interaction between 
-- two (or even more) cells. 

-- Some basic types 

-- from Birth / Death (or Rise / Low ) 
-- there are sources and sinks which correspond to value generation
-- and value substitution to functions
-- second elt in (Int, Int) part correspond to reall type (like () or Bool)
-- there is to be function from Int to types 

-- function from Int to Types : there only a few basic Types which are used in 
-- cell machines, so such function may exist 
-- but in Haskell we can not make a function from Int to Types 
-- may be we have to go to Idris 
-- now we will just keep the function in our mind : 
-- 0 -> ()
-- 1 -> Bool 
-- 2 -> Int 
-- ..... yet undefined 

-- from Source to Sink 
-- the length (mass) of the list is to be >= 1
-- the list of length = 1 correspond to a value (totally evaluated function)
-- in this case the fst elt in the pair is the 'coordinate' of the 
-- function which will consume the value
-- if length > 1 the fst elt correspond to 'coordinate' of value 
-- which will be substituted into the function 
-- the snd elt in the pair correspont to the Type of used data 
-- here it is represented by Int number 
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
-- from topology of our configuration (our geometry) 
data Receptors = Receptor { ud :: Spin_UD,
                            st :: Spin_ST,
                            sss :: [Spin_SS] }

-- mass of receptor 
mass :: Receptors  -> Int
mass p = length $ sss p

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
  -- the left particle is to be a value -> partile of mass = 1 
  not (mass tv == 1)
  &&
  -- 'source' label in  tf has to be the same as 'sink' label in tv
  -- the Types of interaction have to be the same 
  -- in out case it means equality between snd elements  
  (tv_st == fst (head tf_sss)) && snd (head tv_sss) == snd (head tf_sss)
  &&
  -- mass of tf is to be at least 2 ( it is function)
  mass tf >= 2
  &&
  -- st is to be >= 0 
  (tf_st >= 0) && (tv_st >= 0)
  &&
  -- ud spin of value is to be Down and ud spin of tf is to be Up
  tv_ud == Down && tf_ud == Up 
  -- some other laws may be added 

-- laws for interaction between particles of different 
-- compartments : only a value at one side may interact 
-- with id - receptor of another side (ud spins are to be opposite)
-- ud spin of value is to be Up 
-- first is value / second is id function 
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
  (mass tv == 1) && (mass tf == 2)
-- many other laws are to be added 
-- all of it may be force us to use Idris with its universal types 
-- possibilities 

--- to be continued :) 









