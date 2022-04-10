module Genome_Iso where 
-- import Data.Types.Injective

-- firstly read 
-- ## Genes_as_Code.hs
-- ## Genes_Examples.hs

-- when we are working with Genes -> .... -> ProteinMachines chain there 
-- are a lot of Isomorphisms between basic types 
-- here we will try to use the isomorphism for better clarification of the 
-- subject 

-- the working units of Genome are not a single genes but some subsets of 
-- Genes which correspond to Protein Machines : the machines are single 
-- string elements (corresponding to single genes translated to the alphabet of 
-- amino acids) which are folded in 3-D structures (proteins) and collected together 
-- to implement the needed interaction between the 3-D proteins
-- some machines are just the single proteins (lists of length 1) but in 
-- general there is to be several proteins in a machiine 
-- in the similar way when we work with a data base some quiries are senseless : 
-- and even not allowed (implemented) : we are looking for some subsets of 
-- elements from several columns in our  tables , not from asingle 
-- particular column in general 
-- (first_name, last_name) has a sence in banking records , 
-- but just 'first_name' is useless there 

-- we expect the a is from Enumerable class 

-- values of the type are subsets of genes
-- it is like main memory of Code records 
newtype DNA_Code a = DNA_Code {from_DNA_Code :: a}

-- values are subsets of RNA strings (transcribed from single genes)
-- this level is like a cache memory of compiled DNA_Code 
-- we compile and save only that DNA_Code which are in big need for using 
-- at the current stage 
newtype RNA_Code b = RNA_Code {from_RNA_Code :: b}

-- we have to express there is isomorphism between the a and b types 

-- instance (Iso a b, Iso b c) => Iso a c