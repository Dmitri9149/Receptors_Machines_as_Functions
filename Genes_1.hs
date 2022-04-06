-- here I try to use Functor class to clarify the transformations 
-- are actually functors, to be more specific : isomorphisms 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Control.Arrow
import Data.Functor.Contravariant
{-# HLINT ignore "Use camelCase" #-}
-- import Data.Map as Map

-- one of my attempts to make a right type system to 
-- such things are DNA / Genes ; RNA / Proteins 

-- aplhabet of DNA 
-- every gene is string in A, T, C, G (nucleotides) alphabet
data DNA_Alphabet = A | T | C | G

-- these are the regulatory elements , which are used to 
-- transcribe subsets of genes 
-- a promotor is list in DNA_Alphabet
type Promoters = [DNA_Alphabet]
-- a gene is list in DNA_Alphabet 
type DNA_Code = [DNA_Alphabet]
type Genes = DNA_Code
-- we are dealing not with a single genes , but with subsets of 
-- genes : DNA_Packages and  Genome is actually the map from promoters to 
-- collection (here list) of genes 
-- Genome is like main memory which has complex organization 
type DNA_Packages = [DNA_Code]

-- the function is to be injective !
newtype Genome = Genome { from_enum_to_packages:: Promoters  -> DNA_Packages }

-- RNA is like cache memory , intermediate memory 
-- gene in DNA alphabet is transcribed firstly to RNA alphadet 
-- A' is a biochemical modification to A , C' to C , G' to G and U' 
-- correpond to T ( at DNA level)
data RNA_Alphabet = A' | C' | G' | U'
type RNA_Code = [RNA_Alphabet]

from_DNA_to_RNA :: DNA_Alphabet -> RNA_Alphabet
from_DNA_to_RNA x = case x of
  A -> A'
  C -> C'
  G -> G'
  T -> U'

from_RNA_to_DNA :: RNA_Alphabet -> DNA_Alphabet
from_RNA_to_DNA r = case r of
  A' -> A
  C' -> C
  G' -> G
  U' -> T

code_transformer_DNA_to_RNA :: DNA_Code -> RNA_Code
code_transformer_DNA_to_RNA = map from_DNA_to_RNA


-- package of RNA_Code
type RNA_Packages = [RNA_Code]

-- this function transform from DNA_Packge to RNA_Package 
package_transformer_DNA_RNA :: DNA_Packages -> RNA_Packages
package_transformer_DNA_RNA = map code_transformer_DNA_to_RNA




-- DNA and RNA molecules are like an uncompiled code 
-- a biological machine is made from proteins , which are the 
-- processes (compiled code) 
-- for simplicity we will assume one RNA molecule correspond to one protein 

-- the letters in the alphabet correspond to 
-- amino acids which are the monomers that make up proteins
-- A´´ -> Alanine , G´´ -> Glycine , ... etc .
-- I add ´´ here not to mix with DNA / RNA aplhabet 
-- every rna :: RNAs is translated one to one to protein code (string in the Proteins Alphabet)
data ProteinsAlphabet = A'' | C'' | D'' | E'' | F'' | G'' | H'' | I'' | K''
                        | L'' | M'' | N'' | P'' | Q'' | R'' | S'' | T'' | V'' | W'' |Y''

-- there is function which can transform RNACode ([RNAs]) to 
-- string in ProteinCode (string in [ProteinsAlphabet])

type ProteinCode = [ProteinsAlphabet]

-- there is function which transform from RNA code to Protein code 
code_transformer_RNA_Protein :: RNA_Code -> ProteinCode
code_transformer_RNA_Protein = undefined

-- the proteins are folded to the final 3-D structure in Golgi apparatus
-- this is like compilation and spawning  a program ( like runhaskell command)
-- we use newtype to wrap the Protein string to reflects the compilation / folding 
-- and we reflect the fact the active unit is now a single protein 
--  but it is usually a list of proteins collected together to make a 'machine'
-- we will use mostly machines which work as values of Session Types 


newtype ProteinMachines = ProteinMachines {fromCode :: [ProteinCode]}
type ProteinPackages = ProteinMachines

-- this function transform from RNA_Packages to Protein_Packages (or ProteinMachines)

package_transformer_RNA_Proteins :: RNA_Packages -> ProteinMachines
package_transformer_RNA_Proteins rna = ProteinMachines {fromCode = map code_transformer_RNA_Protein rna }

-- let us consider only one class of Protein machines : which are a values of Session Types 
-- let us consider some enumerable state a and one to one function from a to Promoters
-- the function must be injection !
state_to_promoters :: a -> Promoters
state_to_promoters = undefined

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
  state_to_promoters


-- if we write from left to right :  from state a to the promoters to the ... ProteinsPackages 
state_to_machines' :: a -> ProteinMachines
state_to_machines' = 
  state_to_promoters 
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
interaction_1 :: (a,a) -> (a,a)
interaction_1 = undefined 

-- we can forget abou the DNA , RNA and the code and think about a new type 
-- ProteinMacnines' which is just 
newtype ProteinMachines' a = ProteinMachines' { stateToMachines :: a -> ProteinMachines } 
-- we see this is a Contravariant functor 
instance Contravariant ProteinMachines' where 
  contramap f pm@ProteinMachines'{stateToMachines = stm} 
    = ProteinMachines' { stateToMachines = stm . f }

-- we can easily lift the interaction_1 to ProteinMachines and to ProteinMachines'

interaction_11 :: (a,a) -> ((a,a) -> (a,a)) -> (a -> ProteinMachines) -> (ProteinMachines, ProteinMachines)
interaction_11 (m1,m2) interact state_to_machines = 
  let (res1, res2) = interact (m1,m2) in (state_to_machines res1, state_to_machines res2)

-- if we remind about 'biology' about Promoters , DAN and RNA code there are much more steps 
-- the first step is interacting machines will generate not immediatelly the 
-- next pair machines but the pair (prom1 :: Promoters , prom2 :: Promoters) 


-- continuation_in_promoters :: (ProteinMachines, ProteinMachines) -> (Promoters, Promoters)


