module VoteSystem
  where

import Data.List

data Candidate = Candidate String -- Name of candidate
   deriving (Show, Eq)

-- The ballot cast by a voter is represented as a list of candidates in
-- order of preference
type Ballot = [Candidate]


data Election = Election [Candidate] -- The candidate list.  Candidate numbers correspond to their position in this list
   deriving (Show, Eq)


-- Pile and CountingState are used to help keep track while counting votes
-- for each candidate.  A pile represents all of the ballots that are currently
-- being counted towards a candidate
type Pile   = (Candidate,[Ballot])
data CountingState = CountingState Election [Pile]
   deriving (Show)

-- =====================================================================
-- Provided functions 
-- ------
showBallot :: Ballot -> String
showBallot cs = unlines (map showLine numbered)
    where
        numbered = zip [1..] cs
        showLine (k, (Candidate name)) = (show k) ++ ". " ++ name

-- This runs election counting according to some algorithm
doCount :: (CountingState -> Candidate) -> Election -> [Ballot] -> Candidate
doCount alg elect ballots = alg (makeFirstRoundState elect ballots)


-- =====================================================================
-- Part 1
-- ------
checkElements :: Eq a => [a]->[a] -> [Bool]
checkElements [] _ = []
checkElements (x:xs) ys
    | x`elem` ys = True : checkElements xs ys
    | otherwise = False : checkElements xs ys        


validBallot :: Election -> Ballot -> Bool
validBallot (Election cs) vs
    | (length (nub cs) == length (nub vs)) && (and  (checkElements vs cs)) = True
    | otherwise = False
-- =====================================================================
-- Part 1.5
-- ------
-- Returns a list of the candidates still in contention.
stripCandidate :: Pile -> Candidate
stripCandidate ( x ,_ ) = 
stripCandidate (c,[y]) = c  

candidates :: [Pile] -> [Candidate]
candidates x  = map stripCandidate x

 

-- =====================================================================
-- Part 2
-- ------

-- Make piles, thereby creating the initial state for the election
-- This function takes a big list of ballots, and groups them 
-- into Piles based on the top candidate on each ballot
makePiles :: [Ballot] -> [Pile]
makePiles ballots = map makePile grouped
    where 
        -- headEq checks if the top preferences on two ballots are the same:
        --  i.e. whether they belong in the same pile
          headEq b1 b2 = (head b1) == (head b2) 
          headCmp ((Candidate n1):_) ((Candidate n2):_) = compare n1 n2
          -- Group the ballots into lists based on first preference
          grouped = groupBy headEq $ sortBy headCmp ballots 
          -- Make a list of ballots into  a (Candidate, [Ballot]) tuple
          -- by pulling out the first candidate in the first ballot 
          -- of each pile
          makePile ballots = ((head (head ballots)), ballots)


-- makeFirstRoundState:
-- Take a list of all ballots case in the election, along with 
-- the election details and transform that into the initial counting state.
--
-- The initial counting state must have invalid ballots removed.
-- There will be a pile for each candidate in the election who got at least
-- one valid first preference vote
-- FIXME: add type definition for makeFirstRoundState
-- FIXME: replace dummy definition of makeFirstRoundState with your solution
makeFirstRoundState e bs = CountingState (Election [dummyCand]) 
                                         [(dummyCand, [[dummyCand]])]
    where dummyCand = Candidate "arnie" 


-- =====================================================================
-- Part 3 - First past the post
-- ------

-- Sort vote piles in order of smallest to largest
-- FIXME: add type definition for sortPiles
-- FIXME: replace this dummy definition with real definition
sortPiles piles = piles 

-- Decide an election based on the First Past the Post strategy
-- FIXME: add type definition for fptpCount 
-- FIXME: replace this dummy definition with real definition
fptpCount (CountingState e piles) = (Candidate "???")
-- =====================================================================
-- Part 4 - Runoff voting
-- ------
--
-- FIXME: define runoffCount here
runoffCount cs = (Candidate "???")
 
-- =====================================================================
-- Part 5 - preferential voting
-- ------
-- prefCount has been provided for you and you will not need to modify it
-- prefStep is the main function you will be modifying
-- onlyContenders is provided as a hint for how you may structure the solution
-- ------

-- prefCount: Carries out multiple rounds of the preferential voting system until
-- a winner is known
-- FIXME: add type definition for prefCount 
prefCount es@(CountingState e piles) = 
        -- If theres only one candidate's pile remaining, we know who the 
        -- winner is
        if length piles == 1 then fst (head piles)
            -- Otherwise we eliminate a candidate and call this
            -- fn again recursively
            else prefCount (prefStep es)


-- onlyContenders takes a ballot and removes any candidates that are no 
-- longer in the race.  The first argument to this function is a list 
-- of candidates that are still in the race
onlyContenders :: [Candidate] -> Ballot -> Ballot
onlyContenders remaining ballot = [Candidate "???"] --FIXME


-- prefStep: Perform a single round of the vote tallying in
-- the preferential voting system.  In each round,
-- the candidate with the least votes is eliminated, and their
-- votes reallocated to candidates which are still in the running.
prefStep :: CountingState -> CountingState
prefStep (CountingState e piles) = CountingState e (tail piles) --FIXME

