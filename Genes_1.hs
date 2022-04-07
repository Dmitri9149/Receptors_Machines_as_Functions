-- one more trial to 'idiomatically' describe the Genes
-- interplay between the code and processes ( machines) 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Control.Arrow ( (>>>) )
import Data.Functor.Contravariant ( Contravariant(contramap) )

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

-- the proteins are folded to the final 3-D structure in Golgi apparatus
-- this is like compilation and spawning  a program ( like runhaskell command)
-- we use newtype to wrap the Protein string to reflects the folding / spawning
-- and we reflect the fact the active unit is not a single protein 
--  but it is usually a list of proteins collected together to make a 'machine'
-- we will use mostly machines which work as values of Session Types 

newtype ProteinPackages = ProteinMachines {fromCode :: [ProteinCode]}
type ProteinMachines = ProteinPackages

-- this function transform from RNA_Packages to Protein_Packages (or ProteinMachines)

package_transformer_RNA_Proteins :: RNA_Packages -> ProteinMachines
package_transformer_RNA_Proteins rna = ProteinMachines {fromCode = map code_transformer_RNA_Protein rna }

-- let us consider only one class of Protein machines : which are a values of Session Types 
-- let us consider some enumerable state a and one to one function from a to Promoters
-- the function must be bijection !
states_to_promoters :: a -> Promoters
states_to_promoters = undefined

promoters_to_states :: Promoters -> a 
promoters_to_states = undefined 

-- in this case there is function from a to ProteinMachines
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

-- because all the maps were injections there is to be reverse function : 
machines_to_state :: ProteinMachines -> a 
machines_to_state = undefined 

-- let us consider the scenarion : we have agents which are in some state from some 
-- Enumerable a ; for every i from a there is unique code which coding the 
-- unique protein machine i ; this machine is value of Sesion Type -> actually 
-- our state from a is in one to one relation to the protein machine i 
-- val of Session Type of agent i can interact with val of Session Type of agent j 
-- the values (proteins machines) implement the interaction 
-- the implementation will be done in separate module in future 
-- insimplest form : interaction between (protein machine i ) and (protein machine j ) 
-- is given just by function : interaction :: (a , a) -> (a , a) 
pair_to_pair :: (a,a) -> (a,a)
pair_to_pair = undefined

pair_to_promoters :: (a,a) -> (Promoters,Promoters)
pair_to_promoters = undefined  



-- we can forget abou the DNA , RNA and the code and think about a new type 
-- ProteinMacnines' which is just 
newtype ProteinMachines' a = ProteinMachines' { stateToMachines :: a -> ProteinMachines } 
-- we see this is a Contravariant functor 
instance Contravariant ProteinMachines' where 
  contramap f pm@ProteinMachines'{stateToMachines = stm} 
    = ProteinMachines' { stateToMachines = stm . f }

-- we can easily lift the interaction_1 to ProteinMachines and to ProteinMachines'

interaction_11 :: (a,a) -> ((a,a) -> (a,a)) -> (a -> ProteinMachines) -> (ProteinMachines, ProteinMachines)
interaction_11 (m1,m2) interaction state_to_machines = 
  let (res1, res2) = interaction (m1,m2) in (state_to_machines res1, state_to_machines res2)

-- if we remind about 'biology' about Promoters , DAN and RNA code there are much more steps 
-- the first step is interacting machines will generate not immediatelly the 
-- next pair machines but the pair (prom1 :: Promoters , prom2 :: Promoters) 


continuation_in_promoters :: 
  (a,a) -> ((a,a) -> (a,a)) -> ProteinMachines' a -> (Promoters, Promoters)
continuation_in_promoters = undefined 


