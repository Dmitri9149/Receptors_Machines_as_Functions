{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds #-}
module Genes_Block_Chain where
import Data.Map as Map
import Control.Lens

--still draft thoughts about the best description of the Genes (biological)
-- let us try to think about it as a "block-chain"

-- see Genes_as_Code.hs about the DNA, RNA, Proteins alphabets 

-- we are in need of human readable alphabet to make the semantics of Genes 
-- we will use alphanumerical 'characters' for the alphabet 

-- DNA string uses its own alphabert, RNA too, same for Proteins alphabet 
--in this approach we will use the (see below) data constructors as alphabet 
--and will use it with a decorations like DNAGENE
data Apphabet = A | B| C | D | E | F | G | I | J | K | L | M | N | O | P | Q | R | S | T | U | W | X | Y | Z
--for simple backteria we can use : 
data Genome'' dna = Genome'' [(dna,[dna])] deriving (Eq, Ord)
-- the (a,[a]) blocks correspond to the package of proteins : 
-- a protein machine (active process in cell biology) is composed from 
-- several proteins , that is why it is a [a] list linked to the 
-- promoter of type a 
-- the 'a' is the type by which we model the single genes 
-- and regulatory elements : promoters 
-- genes and promoters are atrings in Genes Aplphabet 
-- ## see Genes_as_Code.hs 
-- most probably a is to be modeled by Enumerable and Hashable 
-- for example : 
data Genome' dna = Genome' [(dna,[(dna,[dna])])]
-- or 
-- the data are composet of "blocks" which are tuples (a, [a])
-- linked with keys of type a 
-- the idea is after the Transcription ( from DNA to RNA)
-- the (a,[a]) blocks will be ytansformed to (b,[b]) blocks 
-- where b is data type for RNA modelling 
-- and we will model the RNA blocks as Map.Map b [b]
-- we will (try) to use Lenses for the transformations 
-- the [b] blocks will be transformed to [p] 
data Genome dna =  Genome (Map.Map dna [(dna,[dna])])

-- example how we can describe a simple backteria genome : 
-- there is no RNA (cache memory /compiled code)
-- Protein packages are directly translated from [a] lists 
-- of genes and self assemble to mature protein machines 
-- there are two such packages of genes : 
-- [Gene1,Gene2] (linked to the regulatory element Promoter1)
-- and 
-- [Gene3] (linked to the regulatory element Promoter2)
-- the last 'genes package' has only one gene and the protein 
-- machine will be composed from only one protein 

{-
data Genes'' = Promoter1 | Promoter2 | Gene1 | Gene2 | Gene3 
genome_ex :: Genome'' Genes''
genome_ex = Genome'' [(Promoter1, [Gene1,Gene2]), (Promoter2,[Gene3])]
genes_block1 = genome_ex^.at Promoter1
-}
-- ####################
data Smth = An | Bn deriving (Show, Eq, Ord)
type Gn dna = Map.Map dna [dna]

ex1 :: Gn Smth
ex1 = Map.fromList [(An,[An]), (Bn,[An,Bn])]

-- this is not a 'biological' operation 
genes_block1 :: Maybe [Smth]
genes_block1 = ex1^.at An
-- ###############

-- the data below 
data DNA_Code = DNA_code_p_1 | DNA_code_g_1 | DNA_code_g_4 | DNA_code_g_5 | DNA_code_g_6
  |
  DNA_code_g_7 | DNA_code_g_8 | DNA_code_g_9 | DNA_code_g_10
  |
  DNA_code_g_2 | DNA_code_p_2 | DNA_code_g_3 | DNA_code_p_3
  |
  DNA_code_x_1 | DNA_code_x_2 | DNA_code_x_3
  deriving (Show, Ord, Eq)

data RNA_Code = RNA_code_p_1 | RNA_code_g_1 | RNA_code_g_4 | RNA_code_g_5 | RNA_code_g_6
  |
  RNA_code_g_7 | RNA_code_g_8 | RNA_code_g_9 | RNA_code_g_10
  |
  RNA_code_g_2 | RNA_code_p_2 | RNA_code_g_3 | RNA_code_p_3
  |
  RNA_code_x_1 | RNA_code_x_2 | RNA_code_x_3
  deriving (Show, Ord, Eq)

data Protein_Code = Pr_code_g_1 | Pr_code_g_2 | Pr_code_g_3 | Pr_code_g_4 | Pr_code_g_5
  |
  Pr_code_g_6 | Pr_code_g_7 | Pr_code_g_8 | Pr_code_g_9 | Pr_code_g_10
  deriving (Show, Ord, Eq)


-- this is real 'biological' transformation 
-- the function reflects how code in DNA form (biologically it is string of des nucleotides, 
-- but here we try to madel it just by strings in human understandable alphabet)
-- is COMPILED to code in RNA form (biologically -> list of des rib nucleotides), 
-- like   DNA_code_p_1 -> RNA_code_p_1
-- moreover the transformation if from MAIN memory (DNA) to CACHE memory (RNA)
-- DNA_code_p_1 --- 'p' means it is a promoter (regulatory) element , transcription 
-- factors (which are ) the protein machines , may attach to such elements and 
-- start transcription (or translation of blocks of genes , corresponding to the promoters)
-- DNA_code_g_2 --- 'g' means it is a gene (code of some protein)
-- DNA_code_x_2 --- this is quite speculative suggestion : we assume there are regulatory 
-- elements in DNA string which wil be transformed to RNA regulatory elements and will 
-- actually work as DNA promoters at RNA level 
-- as you can see the DNA_code_x elements are not in use in 'simple backteria genome' 
-- because there is no RNA (cache momory stage in backteria)
------------------------------------------------------
-- TODO try to identify such structure on biological level 
-------------------------------------------------------

from_DNA_to_RNA :: DNA_Code -> RNA_Code
from_DNA_to_RNA dna = case dna of
  DNA_code_p_1 -> RNA_code_p_1
  DNA_code_p_2 -> RNA_code_p_2
  DNA_code_p_3 -> RNA_code_p_3
  DNA_code_g_1 -> RNA_code_g_1
  DNA_code_g_2 -> RNA_code_g_2
  DNA_code_g_3 -> RNA_code_g_3
  DNA_code_g_4 -> RNA_code_g_4
  DNA_code_g_5 -> RNA_code_g_5
  DNA_code_g_6 -> RNA_code_g_6
  DNA_code_g_7 -> RNA_code_g_7
  DNA_code_g_8 -> RNA_code_g_8
  DNA_code_g_9 -> RNA_code_g_9
  DNA_code_g_10 -> RNA_code_g_10
  DNA_code_x_1 -> RNA_code_x_1
  DNA_code_x_2 -> RNA_code_x_2
  DNA_code_x_3 -> RNA_code_x_3

-- this is function for bacterial case 
-- where protein machines are produced ('spawned') as mature processes 
-- directly from DNA 
-- in case of eucariots the DNA is firstly 'compiled' to 
-- RNA cache memory -> from which the proteins machines are spawned 
-- not every element of DNA_code correspond to a protein
-- because some of DNA_code elements are regulatory elements 
-- this elements are 'target' for special proteins 
-- for example DNA_code elements like DNA_code_p_1 are 'promoters' 
-- which are targeted by special proteins with name 'transcription factors' 
-- this is where we tighten the knot ' -> it give us possibility to make 
-- loops -> for example protein which invoke his own production from DNA_code 
-- more about it later 
from_DNA_to_Proteins :: DNA_Code -> Protein_Code
from_DNA_to_Proteins dna = case dna of
  DNA_code_g_1 -> Pr_code_g_1
  DNA_code_g_2 -> Pr_code_g_2
  DNA_code_g_3 -> Pr_code_g_3
  DNA_code_g_4 -> Pr_code_g_4
  DNA_code_g_5 -> Pr_code_g_5
  DNA_code_g_6 -> Pr_code_g_6
  DNA_code_g_7 -> Pr_code_g_7
  DNA_code_g_8 -> Pr_code_g_8
  DNA_code_g_9 -> Pr_code_g_9
  DNA_code_g_10 -> Pr_code_g_10
  _ -> error "can not translate this DNA_Code element to a Protein_Code"

from_Proteins_to_DNA :: Protein_Code -> DNA_Code
from_Proteins_to_DNA prot = case prot of
  Pr_code_g_1 -> DNA_code_g_1
  Pr_code_g_2 -> DNA_code_g_2
  Pr_code_g_3 -> DNA_code_g_3
  Pr_code_g_4 -> DNA_code_g_4
  Pr_code_g_5 -> DNA_code_g_5
  Pr_code_g_6 -> DNA_code_g_6
  Pr_code_g_7 -> DNA_code_g_7
  Pr_code_g_8 -> DNA_code_g_8
  Pr_code_g_9 -> DNA_code_g_9
  Pr_code_g_10 -> DNA_code_g_10




-- simple 'backterial' genome structure Genome''
-- there are no RNA , there is no extra nesting as in more complex
-- structure for Genome' 
-- in the 'backterial' Genome structure protein machines will be 
-- spawn directly from the [DNA_Code ] blocks -> i.e. directly from 
-- main memory , there is no cache memory for RNA_code 
-- and this is not 'biological' function , it is just mathematical 
-- structure , it means there is no a biological operation like 
-- 'find the promoter element and bring the peace of DNA correspondind to it'
genome_ex1'' :: [(DNA_Code, [DNA_Code])] -- <---> genome_ex1'' :: Genome 
genome_ex1'' = let dnaBlock1 = [ DNA_code_g_2 , DNA_code_g_9] in
  let dnaBlock2 = [DNA_code_g_1 , DNA_code_g_3, DNA_code_g_4 , DNA_code_g_5] in
    [(DNA_code_p_1, dnaBlock1), (DNA_code_p_2, dnaBlock2)]


genes_dna_block_2_ex'' :: Maybe [DNA_Code]
genes_dna_block_2_ex'' = Map.fromList genome_ex1'' ^.at DNA_code_p_1

-- this is data structure for the RNA as Cache_Memory which can be 
-- used for agents like 'eukariotic cell' (like cells in human body) 
-- which has the compiled RNA_code stage 
data Cache_Memory'' rna = Cache_Memory'' [(rna,[rna])]
data Cache_Memory rna = Cache_Memory (Map.Map rna [(rna,[rna])])

-- if biologically this is just a list of strings in aminoacids alphabet 
-- ## Genes_as_Code <- see the 
-- this is just the protein strings collected together 
-- the structure is a short living -> there is no a memory storage of 
-- such collections protein strings 
-- the protein strings are immediatelly folded to 3-D structures 
-- after production from RNA strings (or from DNA strings in case of bacteria) 
-- and collected together in lists : such 3-D folded and collected strings 
-- we will name as protein machines : these are the active processes of the system  
-- we will not introduce a new stage for teh 3-D folded proteins like 
-- newtype Protein_Machines = Protein_Machine { folded :: Protein_Package }
-- but will use the Protein_Machines as synonim to Protein_Packages 
-- but it is good to mention that biologically the stage exist 


data Protein_Packages prot = Protein_Packages [prot] deriving (Show, Eq)
type Protein_Machines prot = Protein_Packages prot

-- the function correspond to the 'biological translation' from 
-- DNA blocks to protein machines 
from_dna_block_to_machines :: Lens' (Maybe [DNA_Code]) (Protein_Packages Protein_Code)
from_dna_block_to_machines = lens getter setter
  where
    getter dna_code = case dna_code of
      (Just d_code) -> Protein_Packages (Prelude.map from_DNA_to_Proteins d_code )
      _ -> error "there is no any DNA_Code block "
    setter dna_code prot_code = case prot_code of
      Protein_Packages [] -> Nothing
      Protein_Packages p_code -> Just $ Prelude.map from_Proteins_to_DNA p_code

-- we are taking genome_ex1'' as concrete example of our genome 
-- transform it to Map.map 
-- focus attention at the DNA_code_p_1 dna regulatory element 
-- make translation from the block to the protein machine using from_dna_block_to_machines
-- function 
-- there steps model the reaö biological operations which happens in bacterias 
protein_block_ex1'' :: Protein_Packages Protein_Code
protein_block_ex1'' = Map.fromList genome_ex1'' ^.at DNA_code_p_1 . from_dna_block_to_machines

translator :: [(DNA_Code,[DNA_Code])] -> DNA_Code -> Protein_Packages Protein_Code
translator genome dna_code = Map.fromList (genome) ^.at (dna_code) . from_dna_block_to_machines

main :: IO ()
main = do
  print genes_block1
  print genes_dna_block_2_ex''
  print protein_block_ex1''
  print $ translator genome_ex1'' DNA_code_p_2


