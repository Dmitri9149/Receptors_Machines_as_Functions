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
-- to escape to many data transformation ( we will do a more general description later)
-- we will use just ASCII alphabet as a sunstitute for the DNA , RNA , Protein alphabets 
-- not to mix dna, rna, or protein strings we will do the following : 
-- "dna_gene_code1" will corresponds to string in dna aplhabet (string of nucleotides); 
-- "rna-some_code" or "rna_code1" will corresponds to rna aplhabet (string of des rb nucleotides); 
-- "prot_protein1" will corresponds to the protein string (string of aminoacids)
-- and only such strings we will consider as valid

type DNA = String 
type RNA = String 
type Prot = String 

-- the type model 'simple backteria' 
-- the [DNA] lists / or blovks model the protein packages which will be 
-- finally translated to lists / or blocks of protein strings => which will be finally spawned 
-- to protein machines (the folded in 3-D structure protein strings )
-- the first element in a tuple is regulatory element ('promoter' in biological language) by 
-- which we can find / quiry the particular block 
-- so, the block is a code , which will be finally spawned to the protein machine 
type Genome'' = [(DNA,[DNA])]

-- the (a,[a]) blocks correspond to the package of proteins : 
-- a protein machine (active process in cell biology) is composed from 
-- several proteins , that is why it is a [a] list linked to the 
-- promoter of type a 
-- the 'a' is the type by which we model the single genes 
-- and regulatory elements : promoters 
-- genes and promoters are strings in Genes Aplphabet 
-- ## see Genes_as_Code.hs 

-- this is example of more complex Genome, it correspond to eukariots which are 
-- cells with nucleus (like cells in our body) : 
-- the (DNA,[(DNA,[DNA])]) block : 
-- first element is regulatory ('promoter') -> to the element the ((DNA, [DNA])) 
-- block is attached -> the block will be transcribed to block of RNA molecules 
-- RNA molecules works as a cache memory / compiled code 
-- the DNA : we read from DNA , main memeory by blocks and transform the DNA to RNA 
-- to be saved as cached blocks 
-- we think about a DNA block as list of uncompiled modules in main memory : 
-- like a collection of uncompiled modules of a code in original language 
-- every such block is transcribed : transformed to RNA blocc (cache memory of 
-- compiled code)
-- what is the meaning of the RNA blocks ? 
-- well, the cells in our body have different 'types' : there are stemm cells, hepathocytes, 
-- progenitor of lymphocytes, T progenitors, B progenitors, T cells, B cells: 
-- all the 'types' are organised in 'differentiation' binary tree : the leafs are the most mature and 
-- totally differentiated : they can no any longer to differentiate to any other types.
-- at the root of the tree there is pluripotent cell which can differentiate in cell 
-- of any tipe , but via a chain of intermediate types 
-- why the tree exist ? 
-- my hypothesis is :  the tree is complex organization of memory : this is a tree of 
-- cache memory stages with transition from one cache stage to another 
-- it is complex scaling system for adaptation to demands in change for (cells) agents quantity 
-----------
-- TODO -- explain differentiation tree in more details 
-----------
type Genome' = [(DNA,[(DNA,[DNA])])]
-- or 
-- the data are composet of "blocks" which are tuples (a, [a])
-- linked with keys of type a 
-- the idea is after the Transcription ( from DNA to RNA)
-- the (a,[a]) blocks will be tansformed to (b,[b]) blocks 
-- where b is data type for RNA modelling 
-- and we will model the RNA blocks as Map.Map b [b]
-- we will (try) to use Lenses for the transformations 
-- the [b] blocks will be transformed to [p] 

-- we can think of the list naturally as Map !! 
type DNA_Map'' = Map.Map DNA [DNA]
type DNA_Map' =  Map.Map DNA [(DNA,[DNA])]

-- example how we can describe a simple backteria genome : 
-- there is no RNA (cache memory /compiled code)
-- Protein packages are directly translated from [a] lists 
-- of genes and self assemble to mature protein machines 
-- there are two such packages of genes : 
-- ["dna_Gene1","dna_Gene2"] (linked to the regulatory element "dna_Promoter1")
-- and 
-- ["dna_Gene3"] (linked to the regulatory element "dna_Promoter2")
-- the last 'genes package' has only one gene and the protein 
-- machine will be composed from only one protein 

genome_ex :: Genome''
genome_ex = [("dna_Promoter_1", ["dna_Gene_11","dna_Gene_2"]), ("dna_Promoter_2",["dna_Gene_3"])]
-- let us represent it in the Map form 
genome_as_map :: DNA_Map''
genome_as_map = Map.fromList genome_ex 

-- we use lenses : the function just views the genes_block1 at "dna_promoter1" 
-- we are getting result in DNA alphabet : this is block of genes which we 
-- consider as collection of code for the coding of a cell at a particular stage 
-- of differentiation ( we can identify the stage with "dna_Promoter1" regulatory)
-- element 
genome_block1 :: Maybe [DNA]
genome_block1 = genome_as_map^.at "dna_Promoter_1"
genome_block_n :: Maybe [DNA]
genome_block_n = genome_as_map^.at "dna_Promoter_100"

-- more complex examle 
eucariote_ex1 :: Genome'
-- the block is in DNA alphabet 
eucariote_ex1 = 
  let first_block = [ ("dna_Reg_1", ["dna_Gene_11","dna_Gene_2"]) , ("dna_Reg3", ["dna_Gene4"]) ] 
      second_block = [("dna_Reg4", ["dna_Gene_3", "dna_Gene7"]), ("dna_Reg8",["dna_Gene9"])] 
      in [("dna_Promoter_1", first_block), ("dna_Promoter_2", second_block)]

eucariote_as_map1 :: DNA_Map'
eucariote_as_map1 = Map.fromList eucariote_ex1

eucariote_block1 :: Maybe [(DNA, [DNA])]
eucariote_block1 = eucariote_as_map1^.at "dna_Promoter_1"

main :: IO ()
main = do 
  print genome_block1
  print genome_block_n
  print eucariote_block1


{-}
data Genes'' = Promoter1 | Promoter2 | Gene1 | Gene2 | Gene3 deriving (Eq,Ord)
genome_ex :: Genome'' Genes''
genome_ex = Genome'' [(Promoter1, [Gene1,Gene2]), (Promoter2,[Gene3])]
genomeMap x = case x of
  Genome'' z -> Map.fromList z
genome_example1 = genomeMap genome_ex
genes_example1 = 

-}