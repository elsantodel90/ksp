module KSP.Rockets where

import Data.List
import KSP.Parts

type Rocket = [Stage]

type Stage = (String, [Part]) -- Stage = Name + Parts

data StageStats = StageStats {name :: String, deltav :: Double, minAcceleration :: Double, maxAcceleration :: Double, burnTime :: Double, stageMass :: Double}
type RocketStats = [StageStats]

instance Show StageStats where
    show (StageStats stageName sdeltav sminAcceleration smaxAcceleration sburnTime sstageMass) = 
                 "**** " ++ stageName ++ " ****\n" ++
                 "deltav = " ++ show sdeltav ++ "\n" ++
                 "minAcceleration = " ++ show sminAcceleration ++ "\n" ++
                 "maxAcceleration = " ++ show smaxAcceleration ++ "\n" ++
                 "burnTime = " ++ show sburnTime ++ "\n" ++
                 "stageMass = " ++ show sstageMass

stageStats :: Double -> Stage -> StageStats
stageStats extraMass (stageName, stage) = StageStats { 
                                           name = stageName,
                                           deltav = dv, 
                                           minAcceleration = minA, 
                                           maxAcceleration = maxA, 
                                           burnTime = bt,
                                           stageMass = totalMass
                                         }
                where total f = sum $ map f stage
                      totalThrust   = total thrust
                      totalMass     = extraMass + total mass
                      totalDryMass  = extraMass + total dryMass
                      totalBurnRate = total burnRate
                      netIsp        = totalThrust / totalBurnRate
                      minA = totalThrust / totalMass
                      maxA = totalThrust / totalDryMass
                      bt   = total fuel / totalBurnRate
                      dv   = - netIsp * log ( totalDryMass / totalMass )
                      
rocketStats :: Rocket -> RocketStats
rocketStats rocket = zipWith stageStats (scanl (+) 0 stageMasses) rocket
                where stageMasses = map (sum . map mass) rocketPartsByStage
                      rocketPartsByStage = map snd rocket

showRocketStats :: RocketStats -> IO ()
showRocketStats stats = do
                           sequence_ . intersperse (putStrLn "") $ map print stats
                           putStrLn ""
                           putStr "Total deltav: "
                           print . sum $ map deltav stats

showStatsForRocket :: Rocket -> IO ()
showStatsForRocket = showRocketStats . rocketStats

-- ROCKETS!!

haskell2b,  haskell4, haskell4b :: Rocket

-- Haskell2b was an immediate success, performing orbital maneuvers and rendez-vous on its first flights.
haskell2b =  ( "Space stage", 
                [mk16Parachute, commandPodMk1, tr18a, flT800, flT400,flT400,lvT30]
             ): 
             ( "Second solid stage",
                   [tt38k,rt10,avR8,
                    tt38k,rt10,avR8,
                    tt38k,rt10,avR8,
                    tt38k,rt10,avR8,
                    tt38k,rt10,avR8,
                    tt38k,rt10,avR8]
             ):
             ( "First solid stage",
               [tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut]
             ):
           []
-- Haskell3 is widely considered to be a design failure: the result of neglecting calculations and following a "brute force" approach.
{-
haskell3 =  [commandPodMk1, lv909, flT400, lt1LandingStruts]:
          [tr18a,mk16Parachute, commandPodMk1, tr18a, flT800, flT400, flT400,flT800,     
                    flT800, flT800, flT800, flT800, 
                    flT400, flT400, flT400, flT400, 
                    lv909, lv909, lv909, lv909, 
                    ftx2FuelDuct, ftx2FuelDuct, ftx2FuelDuct, ftx2FuelDuct,
                    avR8,avR8,avR8,avR8,avR8,avR8,avR8,avR8]: 
          [tt38k,rt10,avR8,
            tt38k,rt10,avR8,
            tt38k,rt10,avR8,
            tt38k,rt10,avR8,
            tt38k,rt10,avR8,
            tt38k,rt10,avR8,
            rt10, rt10, rt10, rt10, rt10, rt10]:
          [tr18a,rt10, rt10, rt10,avR8, eas4Strut,
            tr18a,rt10, rt10, rt10,avR8, eas4Strut,
            tr18a,rt10, rt10, rt10,avR8, eas4Strut,
            tr18a,rt10, rt10, rt10,avR8, eas4Strut,
            tr18a,rt10, rt10, rt10,avR8, eas4Strut,
            tr18a,rt10, rt10, rt10,avR8, eas4Strut,
            rt10,rt10,rt10,tt38k, eas4Strut, avR8,
            rt10,rt10,rt10,tt38k, eas4Strut, avR8,
            rt10,rt10,rt10,tt38k, eas4Strut, avR8,
            rt10,rt10,rt10,tt38k, eas4Strut, avR8]:
          []
-}
-- Largely based on haskell2b, a small final stage is added to increase total deltav, and a few rt10 are added to compensate this extra mass.
-- Designed for moon landing and return. Very successful.
haskell4 =  ( "Lunar module", 
                 [mk16Parachute, commandPodMk1, flT400, lt1LandingStruts, lt1LandingStruts, lt1LandingStruts,lv909]
            ): 
            ( "Space stage", 
                 [tr18a, flT800, flT400,flT400,lvT30]
            ): 
            ( "Second solid stage", 
                [tt38k,rt10,avR8,
                 tt38k,rt10,avR8,
                 tt38k,rt10,avR8,
                 tt38k,rt10,avR8,
                 tt38k,rt10,avR8,
                 tt38k,rt10,avR8,
                 rt10,rt10,rt10,
                 eas4Strut,eas4Strut,eas4Strut]
            ):
            ( "First solid stage", 
               [tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                tr18a,rt10, rt10, rt10,avR8, eas4Strut,
                rt10,rt10,rt10,rt10,rt10,rt10,
                eas4Strut,eas4Strut,eas4Strut,eas4Strut,eas4Strut,eas4Strut]
            ):
          []
-- Haskell4b is basically Haskell4 with the rt10 initial stages separated into various sub-stages, and many rt10 removed,
-- to decrease the acceleration while losing very little total deltav.
-- Haskell4 wastes too much fuel due to drag when leaving the atmosphere, by using too much acceleration.
-- Haskell4b achieves essentially the same as Haskell 4 with 80% of the solid fuel.
haskell4b = ( "Lunar module",
                [mk16Parachute, commandPodMk1, flT400, lt1LandingStruts, lt1LandingStruts, lt1LandingStruts,lv909]
            ): 
            ( "Space stage",
                [tr18a, flT800, flT400,flT400,lvT30, rv105,rv105, flR25rcs]
            ):
            ( "Fifth solid stage", 
                  [tt38k,rt10,avR8,
                   tt38k,rt10,avR8]
            ):
            ( "Fourth solid stage",
                  [tr18a,rt10,rt10,rt10,avR8,avR8,avR8,eas4Strut,
                   tr18a,rt10,rt10,rt10,avR8,avR8,avR8,eas4Strut]
            ):
            ( "Third solid stage", 
                  [tr18a,rt10, rt10,avR8,eas4Strut,
                   tr18a,rt10, rt10,avR8,eas4Strut]
            ):
            ( "Second solid stage", 
                  [tr18a,rt10, rt10, rt10,eas4Strut,eas4Strut,eas4Strut,eas4Strut,eas4Strut,
                   tr18a,rt10, rt10, rt10,eas4Strut,eas4Strut,eas4Strut,eas4Strut,eas4Strut]
            ):
            ( "First solid stage", 
                  [tr18a,rt10, rt10, rt10,rt10, avR8,avR8,eas4Strut,eas4Strut,eas4Strut,
                   tr18a,rt10, rt10, rt10,rt10, avR8,avR8,eas4Strut,eas4Strut,eas4Strut,
                   eas4Strut,eas4Strut,eas4Strut]
            ):
          []

