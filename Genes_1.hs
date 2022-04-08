-- one more trial to 'idiomatically' describe the Genes
-- interplay between the code and processes ( machines) 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Control.Arrow ( (>>>) )
import Data.Functor.Contravariant ( Contravariant(contramap) )
import Control.Monad.Trans.State ( state, State )
import qualified Control.Monad.Identity as Data.Functor.Identity
{-# HLINT ignore "Use camelCase" #-} 

-- aplhabet of DNA 
-- every gene is string in A, T, C, G (nucleotides) alphabet
data DNA_Alphabet = A | T | C | G

-- DNA is long string in DNA alphabet 
-- some of it substrings are 'genes' 
-- this substrings code proteins : elementary processes 
-- of the system 
-- the Genes is just a type synonim for DNA_Code 
-- we Genes link us with biology and DNA_Code is referense to 
-- Computer science
type DNA_Code = [DNA_Alphabet]
type Genes = DNA_Code 


-- a working 'unit' of Genome is not a gene , but some subset of genes 
-- it is same way as we usually never use only one module in programming, 
-- but we use a package of modules to get the functionality we are in need. 
-- 
-- promoters are the regulatory elements, substrings of DNA, which are used to 
-- transcribe subsets of genes 
-- a promotor is also list in DNA_Alphabet 
-- special proteins : transcription factors recognize the promoters and 
-- work as quiry in a data base : get a subset of records
type Promoters = [DNA_Alphabet]

-- we are dealing not with a single gene, but with subsets of 
-- genes : DNA_Packages 
-- this is code level : we can think about the subsets as 
-- as modules written in some programming language and collected 
-- to a package 
type DNA_Packages = [DNA_Code]

-- Genome is actually the map from promoters to 
-- collection (here list) of genes 
-- Genome is like main memory which has complex organization 
-- 
-- the function is to be injective !
newtype Genome = Genome { from_enum_to_packages:: Promoters  -> DNA_Packages }

-- RNA is like cache memory , intermediate memory 
-- but we also can think about it as a compiled code , 
-- where genes correspond to uncompiked code 
-- only that part of code which is in big need now for cell is 
-- transcribed from DNA to RNA and saved as cache memory 
-- from which we will spawn protein machines (run compiled packages)
-- gene in DNA alphabet is transcribed firstly to RNA alphadet 
-- A' is a biochemical modification to A , C' to C , G' to G and U' 
-- correpond to T ( at DNA level)
data RNA_Alphabet = A' | C' | G' | U'
type RNA_Code = [RNA_Alphabet]

-- this are functions from 'biology' 
from_DNA_to_RNA :: DNA_Alphabet -> RNA_Alphabet
from_DNA_to_RNA x = case x of
  A -> A'
  C -> C'
  G -> G'
  T -> U'
-- this is well known from biology 
from_RNA_to_DNA :: RNA_Alphabet -> DNA_Alphabet
from_RNA_to_DNA r = case r of
  A' -> A
  C' -> C
  G' -> G
  U' -> T

-- elements of RNA_Code are in one to one correspondence with DNA_Code
code_transformer_DNA_to_RNA :: DNA_Code -> RNA_Code
code_transformer_DNA_to_RNA = map from_DNA_to_RNA


-- there is isomorphism between RNA_Packages and DNA_Packages
-- to every DNA_Package correspond RNA_Package 
-- and every 'gene' in a DNA_Package compiles to some RNA element in the 
-- RNA_Package 
type RNA_Packages = [RNA_Code]

-- this function transform from DNA_Packge to RNA_Package 
package_transformer_DNA_RNA :: DNA_Packages -> RNA_Packages
package_transformer_DNA_RNA = map code_transformer_DNA_to_RNA

-- RNA_Packages correspond to cache memory / compiled code 
-- from this code packages we can run (spawn) mature processes : 
-- protein machines 
-- firstly we translate every RNA 'gene element' to string 
-- in amino acids alphabet 
-- then we fold the strings in 3-D structured molecules 
-- proteins / protein subunits 
-- then we combine the folded element in even more high ordered 
-- structure : protein machine : actuall process units of the system 

-- the letters in the alphabet correspond to 
-- amino acids which are the monomers that make up proteins
-- A´´ -> Alanine , G´´ -> Glycine , ... etc .
-- I add ´´ here not to mix with DNA / RNA aplhabet 
-- every rna :: RNAs is translated one to one to protein code (string in the Proteins Alphabet)
data ProteinsAlphabet = A'' | C'' | D'' | E'' | F'' | G'' | H'' | I'' | K''
                        | L'' | M'' | N'' | P'' | Q'' | R'' | S'' | T'' | V'' | W'' |Y''

-- we have the same one to one structure as for RNA_Code and DNA_Code 
-- we have the chain of isomorphisms 
type ProteinCode = [ProteinsAlphabet]

-- there is function which can transform RNA_Code ([RNAs]) to 
-- string in ProteinCode (string in [ProteinsAlphabet])
-- it is well known transformation from 'biology'
code_transformer_RNA_Protein :: RNA_Code -> ProteinCode
code_transformer_RNA_Protein = undefined

-- the proteins are folded to the final 3-D structure from amino asid strings  
-- and delivered to their compartment of action by the Golgi apparatus which is special 
-- fabric to make mature protein machines from their code.
-- It is similat to spawning a process or starting a programm from compiled file. 
-- Every protein machine has its own compartment of action : scope of the protein action.
-- we use newtype to wrap the Protein string to reflects the folding / spawning
-- and we reflect the fact the active unit is not a single protein 
-- but it is usually a list of proteins collected together to make a 'machine'
-- Now we will use mostly machines which work as values of Session Types

newtype ProteinPackages = ProteinMachines {fromCode :: [ProteinCode]}
type ProteinMachines = ProteinPackages

-- this function transform from RNA_Packages to Protein_Packages (or ProteinMachines)
package_transformer_RNA_Proteins :: RNA_Packages -> ProteinMachines
package_transformer_RNA_Proteins rna = ProteinMachines {fromCode = map code_transformer_RNA_Protein rna }


-- we have a concrete Genome 
our_Genome :: Genome
our_Genome = undefined


state_to_machines :: a -> ProteinMachines
state_to_machines =
  package_transformer_RNA_Proteins
  .
  package_transformer_DNA_RNA
  .
  from_enum_to_packages our_Genome
  .
  states_to_promoters


-- if we write from left to right :  from state a to the promoters to the ... ProteinsPackages 
state_to_machines' :: a -> ProteinMachines
state_to_machines' = 
  states_to_promoters 
  >>> 
  from_enum_to_packages our_Genome
  >>>
  package_transformer_DNA_RNA
  >>>
  package_transformer_RNA_Proteins

  -- As a very important examle we will consider Session types protein machines 
-- Our machines will implement values of this types 
-- Such machines reside on cell membrane and are used for intercellular communication : 
-- Similar to server / client pair in Computer science
-- Let us take some enumerable type 'a' for states and injective function from 'a' to Promoters
-- The idea is : we will model states and state transitions and every p from Promoters 
-- is a state : by making quiry with 'p' we will get the DNA and RNA code for 'p' and 
-- the Session type protein machine for the state 'p'
-- When two cells (agents)  in states p and q will interact it is actually the 
-- interaction between two protein machines 
-- THE IMPLEMENTATION OF INTERACTION AS SESSION TYPE INTERACTION BETWEEN THE TWO MACHINES 
-- WILL BE DONE IN SPECIAL FILE : NOW IT IS 'TO DO '

states_to_promoters :: a -> Promoters
states_to_promoters = undefined

-- let us consider the scenarion : we have agents which are in some state from some 
-- Enumerable a ; for every i from a there is unique code which coding the 
-- unique protein machine i 
-- this machine is value of Sesion Type 
-- val of Session Type of agent i can interact with val of Session Type of agent j 
-- the values (proteins machines) implement the interaction 
-- the implementation will be done in separate module in future 
-- TODO this 
-- in simplest form : interaction between (protein machine i ) and (protein machine j ) 
-- is given just by function : interaction :: (a , a) -> (a , a) 

pair_to_pair :: (a,a) -> (a,a)
pair_to_pair = undefined

pair_to_promoters :: (a,a) -> (Promoters,Promoters)
pair_to_promoters = undefined  


-- we can forget about the DNA , RNA and the code and think about a new type 
-- ProteinMacnines' which is just 
newtype ProteinMachines' a = ProteinMachines' { stateToMachines :: a -> ProteinMachines } 
-- we see this is a Contravariant functor 
instance Contravariant ProteinMachines' where 
  contramap f pm@ProteinMachines'{stateToMachines = stm} 
    = ProteinMachines' { stateToMachines = stm . f }

-- we can easily lift the interaction_1 to ProteinMachines and to ProteinMachines'

interaction_11 :: (a,a) -> ((a,a) -> (a,a)) -> (a -> ProteinMachines) 
  -> (ProteinMachines, ProteinMachines)

interaction_11 (m1,m2) interaction state_to_machines = 
  let (res1, res2) = interaction (m1,m2) 
  in (state_to_machines res1, state_to_machines res2)

-- if we remind about 'biology' about Promoters , DAN and RNA code there are much more steps 
-- the first step is interacting machines will generate not immediatelly the 
-- next pair machines but the pair (prom1 :: Promoters , prom2 :: Promoters) 

-- Let us assume there is function from Promoters to our type a 
from_promoter_to_state :: Promoters -> a 
from_promoter_to_state = undefined 

-- because now Promoters and Protein Machines are in one to one correspondence 
-- we can think about states as just Protein machines values or just as Promoters 
-- below we use State monad to represent the interaction between two cells (agents)
-- we will use the type alianses to shorten the long type names 

type Pr = Promoters
type Pt = ProteinMachines 


from_code  :: (Pr,Pr) -> ((Pt,Pt), (Pr,Pr)) 
from_code = undefined 

code_to_machine :: State (Pr,Pr) (Pt,Pt) 
code_to_machine  = state from_code

machine_interaction :: (Pt,Pt) -> ((Pr,Pr) -> ((Pt,Pt), (Pr,Pr))) 
machine_interaction = undefined 

monadic_interaction :: (Pt,Pt) -> State (Pr,Pr) (Pt,Pt)
monadic_interaction pair = state (machine_interaction pair) 

-- res :: StateT
--  (Pr, Pr)
--  Data.Functor.Identity.Identity
--  (Pt, Pt )
next_pair_1 :: State (Pr, Pr) (Pt, Pt)
next_pair_1 = code_to_machine >>= monadic_interaction 

next_pair :: State (Pr, Pr) (Pt, Pt)
next_pair = do 
-- from the state which is determined by pair of Promoters : (pt1, pt2) we may get the 
-- pair of Protein Machines : (pt1, pt2) 
  (x,y) <- code_to_machine 
-- the interaction between the two protein machines (which are values of Session Types) 
-- we may get the next pair of Promoters : pr_next1, pr_next2 
-- this pair determine the next pair of protein machines : (pt_next1, pt_next2)
-- (pr1,pr2) => (pt1,pt2) -> (pr_next1,pr_next2) => (pt_next1,pt_next2) -> ........
-- => correspond to production of Protein Machine from DNA Code 
-- -> correspond to the interaction between two cells (agents)
  monadic_interaction (x,y)

----  Generalisation 
-- we can generalise our description 
-- actually all is very simple 
-- we have some type dna (most probably enumerable) as DNA code representation 
-- we have RNA code which we may get as applying Identity functor to the type dna
-- of we just may think that we have another type rna which is isomorphic to dna
-- there are isomorphisms dna -> rna and rna -> dna
-- there is also type Protein Machines m isomorphic to dna and rna
-- dna nd rna types represent the 'packeges' of genes which correspond to protein machines 

newtype DNA_packages dna = DNA_packages { as_DNA :: dna  } 
newtype RNA_packages rna = RNA_packages { as_RNA :: rna }
newtype Machines m = Machines { as_Machines :: m}
-- Prs from Promoters : intermediate type between Machines and DNA_code 
-- dna , rna , m , p types are isomorphic to each other 
newtype Prs p = Prs { as_Promoters :: p }

-- and we can repeat mostly one to one 
-- we combine everything to pairs because only the pairs determine 
-- result of interaction : the next pairs 
from_code'  :: (Prs p,Prs p) -> ((Machines m ,Machines m), (Prs p,Prs p )) 
from_code' = undefined

code_to_machine' :: State (Prs p,Prs p) (Machines m ,Machines m) 
code_to_machine'  = state from_code' 

machine_interaction' :: (Machines m,Machines m ) -> 
  ((Prs p , Prs p) -> ((Machines m,Machines m), (Prs p,Prs p))) 
machine_interaction' = undefined 

monadic_interaction' :: (Machines m,Machines m) -> 
  State (Prs p,Prs p) (Machines m,Machines m)
monadic_interaction' pair = state (machine_interaction' pair)

next_pair_1' :: State (Prs p, Prs p) (Machines m, Machines m)
next_pair_1' = code_to_machine' >>= monadic_interaction' 

next_pair' :: State (Prs p, Prs p) (Machines m, Machines m)
next_pair' = do 
  (x,y) <- code_to_machine' 
  monadic_interaction' (x,y)














