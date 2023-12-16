module Main (
    main
) where

import SwarmOps as S
import System.Random
import Control.Monad
import Control.Parallel.Strategies
import System.Environment
import System.Exit

-- iterate through one instance of the swarm
iterateSwarm :: Swarm -> [Double] -> IO Swarm
iterateSwarm swarm randomNums = do 
    let gB = globalBest swarm
        i  = inertia swarm
        c  = cognition swarm
        s  = social swarm
        o  = objFunc swarm
        b  = bounds swarm
        m  = fromIntegral(maxIters swarm)
        i' = i * ((m-1)/m)
        
    case (choice swarm) of
        -- no Parallelization
        1 -> do
            let p = map (\a -> updateParticle a gB randomNums i c s o b) (particles swarm)
            return swarm {particles = p, globalBest = updateGB p, inertia = i'} 
        -- use of parListChunk
        2 -> do
            let p = map (\a -> updateParticle a gB randomNums i c s o b) (particles swarm)
                    `using` parListChunk (chunk swarm) rdeepseq
            return swarm {particles = p, globalBest = updateGB p, inertia = i'} 
        -- use of only parList
        3 -> do
            let p = map (\a -> updateParticle a gB randomNums i c s o b) (particles swarm)
                    `using` parList rdeepseq
            return swarm {particles = p, globalBest = updateGB p, inertia = i'} 
        _ -> die $ "Usage: swarm-test-exe <Parallelization Choice (1,2,3)> <Optimization Example (1,2,3,4,5)>"

pso :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> Int -> Int -> Int -> (Double, Double) -> IO Swarm
pso maxIters' size' posDims' veloDims' inertia' cognition' social' chunk' choice' fnChoice bounds' = do
    -- Initialize random values for particle positions and velocities separately
    initialPos <- replicateM size' $ replicateM posDims' (randomRIO bounds')
    initialVelo <- replicateM size' $ replicateM veloDims' (randomRIO (-1, 1))
    let initialVals = zip initialPos initialVelo

    -- Initialize the swarm
    let initialSwarm = swarmInit initialVals size' maxIters' inertia' cognition' social' posDims' veloDims' chunk' choice' fnChoice bounds'

    -- Run PSO iterations
    finalSwarm <- runPSOHelper maxIters' initialSwarm
    return finalSwarm

-- Helper function for recursive PSO iterations
runPSOHelper :: Int -> Swarm -> IO Swarm
runPSOHelper 0 swarm = return swarm
runPSOHelper n swarm = do
    -- Generate random numbers for updating particles
    randomNums  <- replicateM ((posDims swarm) + (veloDims swarm)) (randomRIO (0, 1))

    -- Update the swarm
    updatedSwarm <- iterateSwarm swarm randomNums

    -- Continue the iterations
    runPSOHelper (n - 1) updatedSwarm

main :: IO ()
main = do
    args <- getArgs  
    progName <- getProgName
    
    putStrLn "<Parallelization Choices>"
    putStrLn "<1:No Parallelization>"
    putStrLn "<2:Use of parListChunk>"
    putStrLn "<3:Use of purely parList>\n"
    putStrLn "<Optimization Examples>"
    putStrLn "<1:Sphere>"
    putStrLn "<2:Quartic>"
    putStrLn "<3:Rosenbrock>"
    putStrLn "<4:Ackley>"
    putStrLn "<5:Rastrigin>\n"
    case args of
        [parChoice, optEx] -> do
            let maxIters' = 10000
                size' = 250
                posDims' = 2
                veloDims' = 2
                inertia' = 1
                cognition' = 1.5
                social' = 1.25
                chunk' = 100
                bounds' = (-100,100)
                
            finalSwarm <- pso maxIters' size' posDims' veloDims' inertia' cognition' social' chunk' (read parChoice :: Int) (read optEx :: Int) bounds'
            putStrLn "Final Global Best Position:"
            print $ globalBest finalSwarm
            putStrLn "Final Global Best Fitness:"
            print $ (objFunc finalSwarm) $ globalBest finalSwarm
        
        _ -> die $ "Usage: " ++ progName ++ " <Parallelization Choice (1,2,3)> <Optimization Example (1,2,3,4,5)>"
        
-- test using stack run -- -- swarm-test-exe +RTS -N4 -l -RTS n m
-- threadscope swarm-test-exe.eventlog
-- must do so in ubuntu after going to cd "/mnt/c/Users/vindi/Haskell Stuff/project/swarm-test"
