module Main where
import VoteSystem
import Data.List
-- =====================================================================
-- main function
-- -------------

main = do
    printTests
    putStrLn "Results\n==================="
    putStrLn ("FPTP Winner: " ++ (show $ doCount fptpCount testElection testBallots))
    putStrLn ("Simple Pref Winner: " ++ (show $ doCount prefCount testElection testBallots))
--    putStrLn "\nBallots\n==================="
--    putStr (unlines (map (showBallot) testBallots))


-- =====================================================================
-- Test data
-- ---------
testCands = [(Candidate "Jane"), (Candidate "Arnold"), (Candidate "Reg"), (Candidate "Lassie")]
[c1, c2, c3, c4] = testCands
testElection = Election testCands

-- An initial count state for testing - with invalid ballots filtered out
testPiles = [ (c1, replicate 3 [c1, c4, c2, c3]),
              (c2, [[c2, c1, c4, c3]]),
              (c3, replicate 5 [c3, c2, c4, c1]),
              (c4, replicate 2 [c4, c1, c2, c3])]
[p1, p2, p3, p4] = testPiles

-- Construct a list of ballots for testing - add in some invalid ballots
testBallots =   ([c1, c2, c3, Candidate "bob"]:[c1, c1, c1, c1]:
                    [Candidate "bob"]:[]:(concatMap snd testPiles))
                -- Add in lots invalid ballots so if they're not filtered fred will win
                    ++ (replicate 20 [Candidate "Fred", c2, c3, c4]) 

-- An initial state for the counting
testState = CountingState testElection testPiles


-- =====================================================================
-- Tests
-- ---------

-- Function to print out test results
printTests = do
        putStrLn "Tests"
        putStrLn "==========="
        putStr $ unlines $ map printTest testPassList
        putStrLn ""
    where passStr False = "Failed :("
          passStr True  = "Passed :)"
          printTest (n, p) = n ++ " " ++ (passStr p)

-- List of the tests and their names
testPassList = [("testValidBallot", testValidBallot),
        ("testInvalidBallot", testInvalidBallot),
        ("testCandidates", testCandidates),
        ("testFirstRoundState", testFirstRoundState),
        ("testSortPiles", testSortPiles),
        ("testFptpCount", testFptpCount),
        ("testRunoffCount", testRunoffCount),
        ("testPrefCount1", testPrefCount1),
        ("testPrefCount2", testPrefCount2),
        ("testPrefCount3", testPrefCount3),
        ("testPrefCount4", testPrefCount4)
        ]

-- Check that valid ballots pass
testValidBallot = and $ map (validBallot testElection) testVotes
    where testVotes = [[c1, c2, c3, c4], [c4, c3, c2, c1], [c2, c4, c1, c3]]

-- Check that invalid ballots fail
testInvalidBallot = and $ map (not . (validBallot testElection)) testVotes
    where testVotes = [[], [c1, c1, c1, c1], [c2, c1, c3], 
                [c1, c2, Candidate "Manchurian", c3], 
                [c1, c2, c3, c4, Candidate "Manchurian"]]

-- Test that candidates function correctly extracts candidates
testCandidates = candidates testPiles == testCands

-- Test makeFirstRoundState
testFirstRoundState = (e == testElection) && ((candSort bs) == (candSort testPiles))
    where       (CountingState e bs) = makeFirstRoundState testElection testBallots
                candSort = sortBy (\((Candidate n1), _) ((Candidate n2), _) ->  compare n1 n2)

-- Test that sort works correctly
testSortPiles = (candidates sorted) == [c2, c4, c1, c3]
        where sorted = sortPiles testPiles


-- Test that fptpCount works correctly
testFptpCount = (fptpCount (CountingState testElection testPiles)) == c3

-- TODO: add test for runoffCount here
-- Result calculated by hand: c1 and c3 make it to second round;
-- c1 beats c3 6 to 5 in second round
testRunoffCount = c1 == result
    where result = runoffCount (CountingState testElection testPiles)


-- Test preferential counting with increasing numbers of candidates
-- test with just one candidate
testPrefCount1 = result == c1
    where result = prefCount (CountingState testElection (take 1 testPiles))

-- test with more - results checked by hand
testPrefCount2 = result == c1
    where result = prefCount (CountingState testElection (take 2 testPiles))

testPrefCount3 = result == c3
    where result = prefCount (CountingState testElection (take 3 testPiles))

testPrefCount4 = result == c1
    where result = prefCount (CountingState testElection (take 4 testPiles))

