{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Genes_Example where 
import Control.Arrow ( (>>>) )
import Control.Monad.Trans.State ( state, State )
import GHC.Exts (prefetchValue2#)

-- see Genes_as_Code.hs for the general genes description 
-- in many cases we are in need of more concrete description of 
-- DNA, RNA, genes, proteins , less abstruct and more 'human oriented' 
-- below we will use the description which use 'human understandable' strings
-- this will make some complex constructs less abstruct and more close to 
-- what happens 'biologycally' in spririt 
-- this is more dedious style , but much more readable and it is much more 
-- clear how DNA , RNA and Protein Machines are linked with each other 
-- (at least I hope so )


-- the 'data base units' which we can quiry from DNA data base are 'packages' : 
-- subsets of genes, because usually to construct a protein machine : 
-- active building block of cells at the level we are making the cell description 
-- as processes , several proteins are used ( protein roughly correspond to single gene)
-- several values at every data type is enought to make some interesting examples 
data DNA_Packages = DNA_package1 | DNA_package2 | DNA_package3 | DNA_package4

data RNA_Packages = RNA_package1 | RNA_package2 | RNA_package3 | RNA_package4

-- promoters are special DNA elements which are targeted by transcription factors 
-- when the factor attachs to the element the transcription of corresponging 
-- DNA package begins -> the it transforms to RNA package and then to 
-- Proteins packege (Machine)
data Promoters = PM1 | PM2 | PM3 | PM4 
data Machines = Pt_machine1 | Pt_machine2 |Pt_machine3 | Pt_machine4

-- by promoters we can make quiry to DNA (to invoke the DNA Code packages)
-- this is 'real' biological function : by it we can quiry the DNA data base 
from_Promoters_to_DNA :: Promoters -> DNA_Packages 
from_Promoters_to_DNA pm = case pm of 
  PM1 -> DNA_package1
  PM2 -> DNA_package2
  PM3 -> DNA_package3
  PM4 -> DNA_package4

-- this is just mathematical description , there is no 'real biological' 
-- data flow in the direction
from_DNA_to_Promoters :: DNA_Packages -> Promoters 
from_DNA_to_Promoters  pm = case pm of 
  DNA_package1 -> PM1
  DNA_package2 -> PM2 
  DNA_package3 -> PM3
  DNA_package4 -> PM4 

-- DNA is like a main memory of uncompiled code packages 
-- this is 'real biological' function 
-- the transformation from DNA to RNA is named 'transcription' 
from_DNA_to_RNA :: DNA_Packages -> RNA_Packages
from_DNA_to_RNA dna = case dna of 
  DNA_package1 -> RNA_package1 
  DNA_package2 -> RNA_package2 
  DNA_package3 -> RNA_package3 
  DNA_package4 -> RNA_package4 

-- this is just mathematical description , there is no 'real biological' 
-- data flow in the direction ( as exceptions are some viruses !! they can produce DNA from RNA)
from_RNA_to_DNA :: RNA_Packages -> DNA_Packages 
from_RNA_to_DNA rna = case rna of 
  RNA_package1 -> DNA_package1
  RNA_package2 -> DNA_package2 
  RNA_package3 -> DNA_package3
  RNA_package4 -> DNA_package4  

-- RNA is like a cache memory of compiled Code Packages (compiled from DNA code)
-- this is 'real biological' function 
-- the process of production of a protein from a rna molecule is named 'translation'
-- Protein Machines (Protein Packages),  are produced / spawned from packages of RNA molecules 
-- in Goldgi apparatue . This apparatus is also responsible for delivery of the machines to 
-- theis compartment of action (machines are exported to their name spaces)
from_RNA_to_Machines :: RNA_Packages -> Machines 
from_RNA_to_Machines rna = case rna of 
  RNA_package1 -> Pt_machine1
  RNA_package2 -> Pt_machine2
  RNA_package3 -> Pt_machine3
  RNA_package4 -> Pt_machine4
-- this is just mathematical function 
from_Machines_to_RNA :: Machines -> RNA_Packages
from_Machines_to_RNA m = case m of 
  Pt_machine1 -> RNA_package1
  Pt_machine2 -> RNA_package2
  Pt_machine3 -> RNA_package3 
  Pt_machine4 -> RNA_package4

-- this is 'real biological' function, but from Proteins to Machines there are 
-- three separate stages (this fucntion is factored to three steps)
-- first step : from outer membrane to nucleus of a cell
-- second step : happens in nucleus an is anded at he row reticulum of Golgi apparatus 
-- third step : happens in Golgi apparatus and is finished as export to the compartment 
-- of the machine action 
from_Promoters_to_Machines :: Promoters -> Machines 
from_Promoters_to_Machines = from_Promoters_to_DNA >>> from_DNA_to_RNA >>> from_RNA_to_Machines

-- this is just mathematical function : machines and promoters are in one to one 
-- relation (at least at the model)
-- if we know value of a machine we can determine the value of corresponding promoter 
from_Machines_to_Promoters :: Machines -> Promoters 
from_Machines_to_Promoters = from_Machines_to_RNA >>> from_RNA_to_DNA >>> from_DNA_to_Promoters

-- promoters represent a state of cell (agent) 
-- using a value of Promoter type  ('biologically' it is transcription factor ) we 
-- spawn the machine which represent the state functionality: machines <-> promoters <-> states 

-- we can use State (Promoters, Promoters) (Machines, Machines) to make some examples 
-- in monadic style 
-- we assume there is a function which describe the cells interaction : 
-- (Promoters, Promoters) -> (Promoters, Promoters) 
-- if there is no interaction between a concrete pairs :  we have the identity 
-- (no change in states) , or we will get a new changed pair of states 

-- we are going to represent 'biologically' the system which we 
-- can describe abstractly as mathematical function 
-- where the abstact state of a cell (agent) is modeled as 
-- 'promoter' 
pairs_p_interaction :: (Promoters,Promoters) -> (Promoters,Promoters) 
pairs_p_interaction = undefined 

-- same for the Machines this is a mathematical function , 
-- because we 'identify' state of cell, promoters, machines 
-- machines here correspond to values of Session Types 
pairs_m_interaction :: (Machines,Machines) -> (Machines,Machines) 
pairs_m_interaction = undefined 

pairs_m_to_promoters :: (Machines,Machines) -> (Promoters,Promoters)
pairs_m_to_promoters (m1,m2) = 
  let (next_m1,next_m2) = pairs_m_interaction (m1,m2) in 
    (from_Machines_to_Promoters next_m1,from_Machines_to_Promoters next_m2) 

-- this description is more 'biological' 
-- newtype DNA_packages dna = DNA_packages { as_DNA :: dna  } 
-- newtype RNA_packages rna = RNA_packages { as_RNA :: rna }
-- newtype Machines m = Machines { as_Machines :: m}
-- Prs from Promoters : intermediate type between Machines and DNA_code 
-- dna , rna , m , p types are isomorphic to each other 
-- newtype Prs p = Prs { as_Promoters :: p }

-- and we can repeat mostly one to one 
-- we combine everything to pairs because only the pairs determine 
-- result of interaction : the next pairs 
from_code'  :: (Promoters,Promoters) -> ((Machines,Machines), (Promoters,Promoters)) 
from_code' = undefined

code_to_machine' :: State (Promoters,Promoters) (Machines, Machines) 
code_to_machine'  = state from_code' 

-- the biological meaning : if we have pair (m1,m2) we can tranform it 
-- to the pair (pr1, pr2) -> which mathematically correspond to 
-- two promoters -> which correspond to the current state 
-- the function of type (Promoters, Promoters) -> ((Machines,Machines), (Promoters,Promoters))
-- corrspond to the interaction of two particles : from current pair (pr1,pr2) -> we can get 
-- the next pair (next_pr1, next_pr2) of promoters, from which we can 'quiry' DNA data base 
-- to get (next_m1,next_m2) 
machine_interaction' :: (Machines,Machines) -> 
  ((Promoters, Promoters) -> ((Machines,Machines), (Promoters,Promoters))) 
machine_interaction' = undefined 

monadic_interaction' :: (Machines,Machines) -> 
  State (Promoters,Promoters) (Machines,Machines)
monadic_interaction' pair = state (machine_interaction' pair)

next_pair_1' :: State (Promoters, Promoters) (Machines,Machines)
next_pair_1' = code_to_machine' >>= monadic_interaction' 

next_pair' :: State (Promoters, Promoters) (Machines,Machines)
next_pair' = do 
-- get pair of machines
  (x,y) <- code_to_machine' 
-- use the pair of machines to get next_pair of promoters and machines 
-- the actual biological flow is: 
----------------------
-- (m1, m2) => then within the interaction generate => (next_pr1, next_pr2) => 
-- then produce from DNA data base => (next_m1, next_m2) => 
-- then within the interaction generate => (next_next_pr1, next_next_pr2) => 
-- then produce from DNA data base => (next_next_m1, next_next_m2) => ....... 
---------------------
  monadic_interaction' (x,y)

---let us make an example 
pairs_m_interaction' :: (Machines,Machines) -> (Machines,Machines)
pairs_m_interaction' (Pt_machine1,Pt_machine4) = (Pt_machine2,Pt_machine4)
pairs_m_interaction' (Pt_machine2,Pt_machine4) = (Pt_machine3,Pt_machine4)
pairs_m_interaction' (Pt_machine3,Pt_machine4) = (Pt_machine2,Pt_machine4)
pairs_m_interaction' (Pt_machine4,Pt_machine1) = (Pt_machine4,Pt_machine2)
pairs_m_interaction' (Pt_machine4,Pt_machine2) = (Pt_machine4,Pt_machine3)
pairs_m_interaction' (Pt_machine4,Pt_machine3) = (Pt_machine4,Pt_machine1)

pairs_m_interaction' (Pt_machine3,Pt_machine2) = (Pt_machine1,Pt_machine4)

pairs_m_interaction' (x,y) = (x,y)

from_code'  :: (Promoters,Promoters) -> ((Machines,Machines), (Promoters,Promoters)) 
from_code' (pr1,pr2) = 
  ((from_Promoters_to_Machines pr1, from_Promoters_to_Machines pr2),(pr1,pr2)

machine_interaction' :: (Machines,Machines) -> 
  ((Promoters, Promoters) -> ((Machines,Machines), (Promoters,Promoters))) 
machine_interaction' (m1,m2) = 
  \ pr1, pr2 -> let (next_pr1,next_pr2) = pairs_m_to_promoters (m1,m2) in 
    let (next_m1, next_m2) = (from_Promoters_to_Machines next_p1, from_Promoters_to_Machines next_p2) in 
      ((next_m1,next_m2), (next_p1,next_p2))















