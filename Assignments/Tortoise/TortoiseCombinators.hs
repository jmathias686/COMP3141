module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.


-- Should focus on each possible scenario, starting with if either of the instructions are just stop
-- then will look at each possible constructor for Instructions, and recursively deal with any possible constructors after
-- e.g. i1 = Move 39 (Turn 26 (Stop)) will pass Move 39 then Turn 26 and then 'remove' the stop and replace with i2.
andThen :: Instructions -> Instructions -> Instructions
andThen i Stop = i
andThen Stop i = i
andThen (Move m i1) i2 = Move m $ (andThen i1 i2)
andThen (Turn r i1) i2 = Turn r $ (andThen i1 i2)
andThen (SetStyle l i1) i2 = SetStyle l $ (andThen i1 i2)
andThen (SetColour c i1) i2 = SetColour c $ (andThen i1 i2)
andThen (PenDown i1) i2 = PenDown $ (andThen i1 i2)
andThen (PenUp i1) i2 = PenUp $ (andThen i1 i2)



-- Loop will check if n is non-positive, otherwise recursively run the instruction n times
loop :: Int -> Instructions -> Instructions
loop n i
    | n <= 0 = Stop
    | otherwise = i `andThen` (loop (n-1) i)



-- Passes the instructions and assumed start state of PenDown as True (Boolean) into invis_helper function
-- which will run through each possible scenario of instruction and keep track of the current state of the pen

-- Pass PenDown or PenUp as penState, starts as True for PenDown
-- If PenUp or PenDown, will alter the state of the Boolean tracking
-- If Stop instr, will pass the PenDown instr if state is True, and Stop
--
invisibly :: Instructions -> Instructions
invisibly i = PenUp $ invis_helper i True
  where
    invis_helper :: Instructions -> Bool -> Instructions
    invis_helper Stop penState
        | penState = PenDown $ Stop
        | otherwise = Stop
    invis_helper (Move m i) penState =  Move m $ invis_helper i penState
    invis_helper (Turn r i) penState = Turn r $ invis_helper i penState
    invis_helper (SetStyle l i) penState = SetStyle l $ invis_helper i penState
    invis_helper (SetColour c i) penState = SetColour c $ invis_helper i penState
    invis_helper (PenDown i) penState = invis_helper i True
    invis_helper (PenUp i) penState = invis_helper i False



-- Works backwards using an accumulator, with the state being passed through
-- to track if changes like colour, style or penControl need to be shifted
--
-- Possible alternate solution would be essentially the same,
--   except remove Turn 180's from intial call, and negate move distance
--    (i.e. Move m i => retr_helper i (Move (-m) $ a) state, 
retrace :: Instructions -> Instructions
retrace i = Turn 180 $ retr_helper i (Turn 180 Stop) start
  where
    retr_helper :: Instructions -> Instructions -> TortoiseState -> Instructions
    retr_helper Stop a state = a
    retr_helper (Move m i) a state = retr_helper i (Move m $ a) state
    retr_helper (Turn r i) a state = retr_helper i (Turn (-r) $ a) state
    retr_helper (SetStyle l i) a state = retr_helper i (SetStyle (style state) $ a) (state {style = l})  
    retr_helper (SetColour c i) a state = retr_helper i (SetColour (colour state) $ a) (state {colour = c})
    retr_helper (PenDown i) a state = retr_helper i (if penDown state then PenDown $ a else PenUp $ a) (state {penDown = True})
        -- | penDown state = retr_helper i (PenDown $ a) (state {penDown = True})
        -- | otherwise = retr_helper i (PenUp $ a) (state {penDown = True})
    retr_helper (PenUp i) a state = retr_helper i (if penDown state then PenDown $ a else PenUp $ a) (state {penDown = False})
        -- | penDown state = retr_helper i (PenDown $ a) (state {penDown = False})
        -- | otherwise = retr_helper i (PenUp $ a) (state {penDown = False})



-- Implemented using the previous combinators, e.g. andThen to combine the different instructions,
-- then will retrace back to inital state invisibly
-- NOTE: after each list, needs to go from inital state again to produce three separate images in one
overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay is = fullIs
  where
    backIs = map (retrace . invisibly) is
    separateIs = zipWith andThen is backIs
    fullIs = foldr andThen Stop separateIs
    
