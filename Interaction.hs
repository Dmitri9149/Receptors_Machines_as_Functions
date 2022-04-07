module Interaction where

-- the configuration : there is 'gas' of particles of type 'a' 
-- particles may interact : there is function 'interact' which 
-- take two particles of type 'a' and may be produce a new particle 
-- of type 'a' -> pt1 'interact' pt2 (similar to evaluate function)
-- we have a list ['a'] ('gas') paticles -> describe the 
-- evolution of the 'gas' : two particles are consumed and 
-- one particle is produced 

interact :: a -> Maybe a
interact = undefined 

evolution :: (a -> Maybe a) -> [a] -> [a]
evolution = undefined 
