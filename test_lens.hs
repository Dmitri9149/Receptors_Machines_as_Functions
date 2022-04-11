{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
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

-- this operation correspont to the biological transcription 
-- it is quite comples and is effected by 'transcription machine'


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

-- simple 'backterial' genome structure Genome''
-- there are no RNA , there is no extra nesting as in more complex
-- structure for Genome' 
-- in the 'backterial' Genome tructure protein machines will be 
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
genes_dna_block_2_ex'' = Map.fromList genome_ex1''   ^.at   DNA_code_p_1





main :: IO ()
main = do
  print genes_block1
  print genes_dna_block_2_ex''


{-}
data Genes'' = Promoter1 | Promoter2 | Gene1 | Gene2 | Gene3 deriving (Eq,Ord)
genome_ex :: Genome'' Genes''
genome_ex = Genome'' [(Promoter1, [Gene1,Gene2]), (Promoter2,[Gene3])]
genomeMap x = case x of
  Genome'' z -> Map.fromList z
genome_example1 = genomeMap genome_ex
genes_example1 = 

-}

