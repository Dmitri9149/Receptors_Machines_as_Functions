{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Genes_Example where
import Control.Arrow ( (>>>) )
import Control.Monad.Trans.State
import qualified Data.Bifunctor as BiF

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
data DNA_Packages = DNA_package1 | DNA_package2 | DNA_package3 | DNA_package4 deriving Show

data RNA_Packages = RNA_package1 | RNA_package2 | RNA_package3 | RNA_package4 deriving Show

-- we also add the data "Protein_Packages" : biologically these are the lists of strings in 
-- alphabet of amino acids 
-- ## see Genes_as_Code.hs 
-- for more details 
-- if DNA is main memory and works actually as "block chain" (but it is more then list , it is tree)
-- it keeps records about the very agents and is present at the nucleus of every cell
-- if RNA is like a cache memory , or compiled code : the only currently needed code is compiled 
-- from DNA and stored at endoplasmic reticulum of Goldgi apparatus 
-- the protein strings listed in a protein package are not stored for a long time but 
-- are immediatelly transformed to 3-D mature proteins , which are immediatelly collected in 
-- together in a protein machine : the mature process (protein machine) is spawned from the 
-- protein packages (list of strings) just the package is translater from the cache : RNA package
-- so DNA -> long term / main memory (it is code)
--    RNA -> cache memory (it is code)
--    Protein strings -> short term memory (it is still code)
--    Protein Machines : 3-D folded (here folded is used biologically,
--    to reflect 3-D structure formation) proteins which are additionally collected to higher 
--    level structure ( something like 'tensors' of 3-D proteins , or list of 3-D folded proteins)

-- promoters are the elements in DNA alphabet which are 'tighting the knot'
-- these are DNA elements (strings is DNA alphabet, like genes) and interact 
-- with Protein Machines : at this point Code interact with Spawned Processes
-- it makes possible the loop : read from Code -> Spawn Process -> the process 
-- query the Code data base 
-- (by using the Promoters -> DNA_Packages function ) -> reads new code -> 
-- Spawn Process -> ... 
-- Promoters are special DNA elements which are targeted by transcription factors: 
-- which are proteins machines in our classification 
-- when the factor attachs to the promoter element the transcription of corresponging 
-- DNA package begins -> then it transforms to RNA package and then to 
-- Proteins package (Machine)

data Promoters = PM1 | PM2 | PM3 | PM4 deriving Show
data Machines = Pt_machine1 | Pt_machine2 |Pt_machine3 | Pt_machine4 deriving Show

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
pairs_m_interaction = id --- just to define something , not to keep undefined 

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

-- let us consider an example 
-- biologycally within the interaction of the machines 
-- which are now values of session types pairs of values of Promoter type are 
-- firstly produced 
interaction_machines_to_promoters :: (Machines, Machines) -> (Promoters,Promoters)
interaction_machines_to_promoters (Pt_machine1,Pt_machine4) = (PM2,PM4)
interaction_machines_to_promoters (Pt_machine2,Pt_machine4) = (PM3,PM4)
interaction_machines_to_promoters (Pt_machine3,Pt_machine4) = (PM2,PM4)
interaction_machines_to_promoters (Pt_machine4,Pt_machine1) = (PM4,PM2)
interaction_machines_to_promoters (Pt_machine4,Pt_machine2) = (PM4,PM3)
interaction_machines_to_promoters (Pt_machine4,Pt_machine3) = (PM4,PM1)
interaction_machines_to_promoters (Pt_machine2,Pt_machine3) = (PM3,PM2)
interaction_machines_to_promoters (Pt_machine3,Pt_machine2) = (PM2,PM3)
interaction_machines_to_promoters (x,y) = (from_Machines_to_Promoters x, from_Machines_to_Promoters y)

-- from the function , using the structural map from_Promoters_to_Machines 
-- we can find how the pair of machines generate a new pair of machines 
interaction_machines_to_machines :: (Machines, Machines) -> (Machines,Machines)
interaction_machines_to_machines (x,y) =
  BiF.bimap from_Promoters_to_Machines from_Promoters_to_Machines (interaction_machines_to_promoters (x,y))

main :: IO ()
main = do
  print
  $
  (interaction_machines_to_machines
  .
  interaction_machines_to_machines
  .
  interaction_machines_to_machines
  .
  interaction_machines_to_machines
  .
  interaction_machines_to_machines
  .
  interaction_machines_to_machines
  )
  (Pt_machine4,Pt_machine1)




-- so in real 'biological' situation when machines interact firstly the 
-- values of type Promoters will 
-- be produced (the name of the molecule is transcription factor)
-- after that the transcription factors will 'query' the genes data base 
-- x :: Promoters => then using the from_Promoters_to_DNA => then the DNA packege will be generated 
-- => then the RNA package will be generated => then the Protein package will be generated => 
-- then the x :: Machine value of type Machine will be generated 























