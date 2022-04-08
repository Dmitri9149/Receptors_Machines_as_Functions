{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Genes_Example where 
import Control.Arrow ( (>>>) )

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
data DNA_Packages = DNA_package1 | DNA_package2 | DNA_package3 | DNA_package4

data RNA_Packages = RNA_package1 | RNA_package2 | RNA_package3 | RNA_package4

-- promoters are special DNA elements which are targeted by transcription factors 
-- when the factor attachs to the element the transcription of corresponging 
-- DNA package begins -> the it transforms to RNA package and then to 
-- Proteins packege (Machine)
data Promoters = PM1 | PM2 | PM3 | PM4 
data Machines = Pt_machine1 | Pt_machine2 |Pt_machine3 | Pt_machine4

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







