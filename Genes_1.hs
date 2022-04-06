-- here I try to use Functor class to clarify the transformations 
-- are actually functors, to be more specific : isomorphisms 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
-- genes from Genome , Genome is actually the Map from promoters to 
-- collection (here list) of genes 
-- Genome is like main memory which has complex organization 
type DNA_Packages = [DNA_Code]
type Genome = DNA_Packages

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


-- packege of RNA_Code
type RNA_Package = [RNA_Code]




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


type ProteinMachines = [ProteinCode]
type ProteinPackages = ProteinMachines

