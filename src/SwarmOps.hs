module SwarmOps (
    updatePos,
    updateVelo,
    updatePB,
    updateParticle,
    updateGB,
    particleInit,
    swarmInit,
    Swarm(..),
    Particle(..)
) where

import qualified Data.List as L
import Control.Parallel.Strategies
import Control.DeepSeq

-- Sphere Function
sphereFunction :: [Double] -> Double
sphereFunction xs = sum $ map (^(2::Integer)) xs

-- Quartic function
quartFunction :: [Double] -> Double
quartFunction xs = sum $ map (^(5::Integer)) xs

-- Rosenbrock's Banana Function
rosenbrockFunction :: [Double] -> Double
rosenbrockFunction [x, y] = (1 - x)^(2::Integer) + 100 * (y - x^(2::Integer))^(2::Integer)
rosenbrockFunction _ = error "Rosenbrock's function is defined for 2 dimensions only."

-- Ackley's Function
ackleyFunction :: [Double] -> Double
ackleyFunction xs = -20 * exp (a) - exp (b) + exp 1 + 20
  where
    n = fromIntegral $ length xs
    a = -0.2 * sqrt (sum (map (\x -> x^(2::Integer)) xs) / n)
    b = sum (map (\x -> cos (2 * pi * x)) xs) / n
    
-- Rastrigin's Objective Function
rastriginObjective :: [Double] -> Double
rastriginObjective xs = 10 * fromIntegral (length xs) + sum [x^(2::Integer) - 10 * cos (2 * pi * x) | x <- xs]

data Particle =  Particle {position :: [Double],
                           velocity :: [Double],
                           bestPos :: [Double],
                           bestFitness :: Double} deriving (Show)
                           
instance NFData Particle where
    rnf (Particle positionRNF velocityRNF bestPosRNF bestFitnessRNF) = 
                                                           rnf positionRNF `deepseq`
                                                           rnf velocityRNF `deepseq`
                                                           rnf bestPosRNF `deepseq`
                                                           rnf bestFitnessRNF         
                           
-- define Swarm
data Swarm = Swarm {particles :: [Particle],
                    size :: Int,
                    maxIters :: Int,
                    globalBest :: [Double],
                    inertia :: Double,
                    cognition :: Double,
                    social :: Double,
                    posDims :: Int,
                    veloDims :: Int,
                    chunk :: Int,
                    choice :: Int,
                    bounds :: (Double, Double),
                    objFunc :: ([Double] -> Double)}
                          
-- update particle pos, and outputs new pos vector
-- when reaching boundaries, implemented "bouncing" to stop particles from
-- getting stuck on bounds
updatePos :: Particle -> [Double] -> (Double, Double) -> Particle
updatePos particle randomNums bounds' = 
    particle {position = zipWith3 f' pos velo' randomNums,
              velocity = zipWith3 g' pos velo randomNums}
        where pos  = position particle
              velo = velocity particle
              velo'
                |length pos > length velo  = velo ++ (replicate ((length pos) - (length velo)) 0)
                |otherwise                 = velo
              (low, high) = bounds'
              f' p v r
                |p + v*r> high  = 2*high - (p + v*r)
                |p + v*r < low  = 2*low - (p + v*r)
                |otherwise      = p + v*r
              g' p v r
                |p + v*r > high || p + v < low = -v
                |otherwise                     = v
    
-- update particle velo, and outputs new velo vector
updateVelo :: Particle -> [Double] -> [Double] -> Double -> Double -> Double -> Particle
updateVelo particle randomNums globalBest' inertia' cognitive' social' =
    particle {velocity = L.zipWith5 (\v p pB rN gB -> inertia' * v + cognitive' * rN * (pB - p) + social' * rN * (gB - p) ) 
             (velocity particle) (position particle) (bestPos particle) randomNums globalBest'}
    

--updates the personal best of that particle
updatePB :: Particle -> ([Double] -> Double) -> Particle
updatePB particle objFunc'
    |fit < bestFitness particle = particle {bestFitness = fit,
                                            bestPos     = position particle}
    |otherwise                  = particle
        where fit = objFunc' $ position particle

-- combines both updateVelo and pos to update the particle
updateParticle :: Particle -> [Double] -> [Double] -> Double -> Double -> Double -> ([Double] -> Double) -> (Double, Double) -> Particle
updateParticle particle globalBest' randomNums inertia' cognitive' social' objFunc' bounds' =
    updatePB (updatePos (updateVelo particle randomNums globalBest' inertia' cognitive' social') randomNums bounds') objFunc'
    
-- updates the global best of the swarm for the objective function
updateGB :: [Particle] -> [Double]
updateGB particles' = position $ L.minimumBy (\a b -> compare (bestFitness a) (bestFitness b)) particles'

    
-- initialize particle with random value within given ranges
particleInit :: [Double] -> [Double] -> ([Double] -> Double) -> Particle
particleInit initPosition initVelocity objFunc' =
    Particle {position = initPosition, velocity = initVelocity,
              bestPos = initPosition, bestFitness = objFunc' initPosition}

-- intialize swarm with random value within given ranges
swarmInit :: [([Double],[Double])] -> Int -> Int -> Double -> Double -> Double -> Int -> Int -> Int -> Int -> Int -> (Double, Double) -> Swarm
swarmInit initialVals size' maxIters' inertia' cognition' social' posDims' veloDims' chunk' choice' fnChoice bounds' = 
             Swarm {particles = particles',
                    size = size',
                    maxIters = maxIters',
                    globalBest = updateGB particles',
                    inertia = inertia',
                    cognition = cognition',
                    social = social',
                    posDims = posDims',
                    veloDims = veloDims',
                    chunk = chunk',
                    choice = choice',
                    bounds = bounds',
                    objFunc = objFunc'}
                        where objFunc'
                                |fnChoice == 1  = sphereFunction 
                                |fnChoice == 2  = quartFunction
                                |fnChoice == 3  = rosenbrockFunction
                                |fnChoice == 4  = ackleyFunction
                                |fnChoice == 5  = rastriginObjective
                                |otherwise      = error "Usage: swarm-test-exe <Parallelization Choice (1,2,3)> <Optimization Example (1,2,3,4,5)>"
                              particles' = map (\(initP, initV) -> particleInit initP initV objFunc') initialVals