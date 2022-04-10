{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Genes where 
import Data.Map as Map

-- there are -draft thoughts' to develop an intuition 

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
type Genes = [DNA_Alphabet]
-- we are dealing not with a single genes , but with subsets of 
-- genes from Genome , Genome is actually the Map from promoters to 
-- collection (here list) of genes 
-- Genome is like main memory which has complex organization 
newtype Genome = Genes { fromGenes :: Map Promoters  [Genes]}

-- RNA is like cache memory , intermediate memory 
-- gene in DNA alphabet is transcribed firstly to RNA alphadet 
-- A' is a biochemical modification to A , C' to C , G' to G and U' 
-- correpond to T ( at DNA level)
data RNA_Alphabet = A' | C' | G' | U' 
type RNAs = [RNA_Alphabet]

-- DNA and RNA molecules are like an uncompiled code 
-- a biological machine is made from proteins , which are the 
-- processes (compiled code) 
-- for simplicity we will assume one RNA molecule correspond to one protein 
type MachineCode = [RNAs]

-- the letters in the alphabet correspond to 
-- amino acids which are the monomers that make up proteins
-- A´´ -> Alanine , G´´ -> Glycine , ... etc .
-- I add ´´ here not to mix with DNA / RNA aplhabet 
-- every rna :: RNAs is translated one to one to protein code (string in the Proteins Alphabet)
data ProteinsAlphabet = A'' | C'' | D'' | E'' | F'' | G'' | H'' | I'' | K'' 
                        | L'' | M'' | N'' | P'' | Q'' | R'' | S'' | T'' | V'' | W'' |Y''
type ProteinCode = [ProteinsAlphabet]

-- the proteins are folded to the final 3-D structure in Golgi apparatus
-- this is like compilation and spawning  a program ( like runhaskell command)
-- we use newtype to wrap the Protein string to reflects the compilation / folding 
-- and we reflect the fact the active unit is now a single protein 
--  but it is usually a list of proteins collected together to make a 'machine'
-- we will use mostly machines which work as values of Session Types 
newtype ProteinMachines = Folded { fromFolded :: [ProteinCode]}

-- we may go from Promoters to ProteinsMachines , because most part of transformations is 
-- one to one : DNA (single gene) -> RNA (single rna molecule) -> protein code (string) -> 
-- folded protein -> specific collection of proteins are forming a machine 

-- this function is actually what we are in need : it is the function from 
-- Promoters to the ProteinMachines ; the Genome is in some way combined with 
-- Goldgi apparatus 
-- the function is to be one-to-one -> isomorphism 
cellFabric :: Promoters -> ProteinMachines
cellFabric = undefined 

-- what ProteinMachines can do ? One class of ProteinMachines are 
-- values of session types. Its organize receptors in bundles for intercellular 
-- communication. Cells in a body have different 'types'. 'Type' here is used here 
-- as biological notion : for example 'lymphocyte' or 'hepatocyte' : there 
-- are cells at different stages of differentiation : we may say at different 
-- states. 
-- We are in situation where there are agents at different states. 
-- Cells may change their stage of differentiation by interaction with 
-- another cells. 
-- We are in situation where agents are changing their state via 
-- interaction with another agents. 
-- Let as consider the set of states of cells: (will model is by Int)
newtype AgentStates = AgentStates {fromInt :: Int }

-- there is to be function : from AgentStates to Promoters
-- the meaning is : taking a state i we can quiry promoters and get 
-- some protein machin (at state i) which represent the state i 
-- we can consider interaction of cell at state i with another cell 
-- (or agent) at state j as interaction of two proteins machines 
-- at state i and j 
-- because the proteins machines are actually values of Session Types 
-- the interaction will change the cell at stage i to cell at 
-- state (interaction (i,j)) and cell (agent) at stage j to state 
-- (interaction (j,i)) : where 'interaction' is a function which determine 
-- the transition of the cells to new stages within the interaction 
agents_to_promoters :: AgentStates -> Promoters
agents_to_promoters = undefined 

-- fst of the tuple correspond to the first agent , and second to the 
-- second agent 
interaction :: (AgentStates , AgentStates) -> (AgentStates, AgentStates) 
interaction = undefined

-- concrete examples how we can make concrete protein machiens from 
-- receptors and how protein machines can work as values of Session Types 
-- are to be in file 'Receptors' (may be it is not done yet to the moment)




