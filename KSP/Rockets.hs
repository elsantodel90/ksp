module KSP.Rockets where

import Data.List
import KSP.Parts

type Rocket = [Stage]

type Stage = (String, [Part]) -- Stage = Name + Parts

data StageStats = StageStats {name :: String, 
                              deltav :: Double, 
                              minAcceleration :: Double, 
                              maxAcceleration :: Double, 
                              burnTime :: Double, 
                              stageMass :: Double}
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
                      dv   = - netIsp * log ( totalDryMass / totalMass ) -- Tsiolkovsky's rocket equation
                      
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

haskell2b,  haskell4, haskell4b,haskell4c, orbiter1, orbiter2 :: Rocket

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
-- Identical to haskell4b, except it has two sets of four RCS blocks, instead of only one set of two, and they are MUCH better placed
-- for efficient translation and torque application.
haskell4c = ( "Lunar module",
                [mk16Parachute, commandPodMk1, flT400, lt1LandingStruts, lt1LandingStruts, lt1LandingStruts,lv909]
            ): 
            ( "Space stage",
                [tr18a, flT800, flT400,flT400,lvT30, rv105,rv105, rv105,rv105, rv105,rv105, rv105,rv105, flR25rcs]
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

-- Similar to orbiter1, with more fine-grained asparagus staging (Only one fuel tank per stage).
-- Immediately established a new record for less-fuel-to-kerbin-orbit, with only 4 flT400 (vs the 5 used by orbiter1).
-- Due to it's "upside-down" design (used fuel tanks are ejected from the TOP of the vehicle), it requires some skill
-- from the pilot to ensure a safe decoupling of used fuel stages.
orbiter2 =  ( "Stage4",
                [commandPodMk1, flT400, avR8,avR8,avR8,avR8, lvT30]
            ): 
            ( "Stage3",
                [tr18a, flT400, ftx2FuelDuct, borrowed lvT30]
            ):
            ( "Stage2",
                [tr18a, flT400, ftx2FuelDuct, borrowed lvT30]
            ):
            ( "Stage1",
                [tr18a, flT400, ftx2FuelDuct, borrowed lvT30]
            ):
          []

-- My Campaign Rockets!!!!

upGoer1,upGoer1liquid, upGoer2,rocko1,rocko1b, orbitatis1,orbitatis1b,orbiter3,apolin1 :: Rocket

-- upGoer1 is designed to go as high as possible, as cheap as possible. Upper atmosphere reached.

upGoer1 = ( "Second stage",
                [mk16Parachute, commandPodMk1, seaLevel rt10]
          ): 
          ( "First stage",
                [seaLevel rt10]
          ):
          []

-- Version con combustible liquido, utilizado para cumplir un contrato de testing de un paracaidas.

upGoer1liquid = ( "Second stage",
                [mk16Parachute, commandPodMk1, 
                     flT400, seaLevel lvT30]
          ): 
          ( "First stage",
                [tr18a, flT400, seaLevel lvT30]
          ):
          []

-- Exitoso en ir bien arriba al espacio y salir de la atmosfera por primera vez.
upGoer2 = ( "Third solid stage",
                [mk16Parachute, commandPodMk1, tr18a, rt10]
          ):
          ( "Second solid stage",
                [tr18a, seaLevel rt10]
          ):
          ( "First solid stage",
                [tr18a, seaLevel rt10]
          ):
          []

-- No muy exitoso
rocko1 =  ( "Second solid stage",
                [mk16Parachute, commandPodMk1, 
                 tr18a, seaLevel rockomaxBACC, seaLevel rt10, seaLevel rt10, avt1,avt1]
          ):
          ( "First solid stage",
                [tr18a, seaLevel rockomaxBACC, seaLevel rt10, seaLevel rt10, avt1,avt1]
          ):
          []

-- Cumplio mision de testear el rockomaxBACC, exitosa trayectoria suborbital
rocko1b =  ( "Rockomax stage",
                [mk16Parachute, commandPodMk1, 
                 tr18a, seaLevel rockomaxBACC]
          ):
          ( "Second solid stage",
                [seaLevel rockomaxBACC,seaLevel rockomaxBACC]
          ):
          ( "First solid stage",
                [tr18a, seaLevel rockomaxBACC , seaLevel rockomaxBACC, tr18a, seaLevel rockomaxBACC, seaLevel rockomaxBACC]
          ):
          []

-- Proyecto similar al orbiter1, pero teniendo en cuenta que no disponemos de fuel-ducts pero si de combustible solido pulenta.

orbitatis1 =  ( "Orbital stage",
                [mk16Parachute,commandPodMk1, flT400,lv909]
            ): 
            ( "Liquid stage",
                [tt38k,tt38k, flT400, flT400, avR8,avR8, seaLevel lvT30, lvT30]
            ):
            ( "Solid stage",
                [tr18a, seaLevel rockomaxBACC]
            ):
          []

orbitatis1b =  ( "Orbital stage",
                [mk2radialParachute, mk2radialParachute, commandPodMk1, misteryGoo, sc9001scienceJr, flT400,lv909]
            ): 
            ( "Suborbital stage",
                [tt38k,tt38k, flT400, flT400, avR8,avR8, lvT30, lvT30]
            ):
            ( "Second solid stage",
                [tr18a, seaLevel rockomaxBACC]
            ):
            ( "First solid stage",
                [tr18a, tr18a, seaLevel rt10, seaLevel rt10]
            ):
          []

-- Rocket design to get to orbit, using as little fuel as possible. First use of asparagus staging. [Built in the game's demo]
orbiter1 =  ( "Stage3",
                [mk16Parachute,commandPodMk1, flT400,lvT30]
            ): 
            ( "Stage2",
                [tt38k,tt38k, flT400, flT400, ftx2FuelDuct, ftx2FuelDuct, borrowed lvT30]
            ):
            ( "Stage1",
                [tt38k,tt38k, flT400, flT400, ftx2FuelDuct, ftx2FuelDuct, borrowed lvT30]
            ):
          []

-- Rocket design to get to orbit effectively and *cheaply*.

orbiter3 =  ( "Orbital Stage",
                [mk2radialParachute, mk2radialParachute,commandPodMk1, flT400, avR8,avR8,avR8,lv909]
            ):
            ( "Solid 2",
                [tr18a, seaLevel rockomaxBACC]
            ):
            ( "Solid 1",
                [tr18a, seaLevel rockomaxBACC]
            ):
          []

-- Rocket design to get to the mun, stablish a permanent scientific probe (thermometer) in orbit around it, make a kerbal lay foot on it and return home safely.

-- Planned Requisites:
-- > First stages to get the final two stages into orbit.
-- > Scientific probe stage, with 1400 m/s of deltav (to transfer to the mun, and establish an orbit around it)
-- > Mun lander: with 2000 m/s of deltav (descends to the mun from orbit, returns back to kerbin)
-- > DESIGN MISTAKE: Forgot to add mobileProcessingLab in calculations.

apolin1 =   ( "Mun Lander",
                [mk16Parachute,commandPodMk1, flT200,rockomax48_7s, lt1LandingStruts,lt1LandingStruts,lt1LandingStruts]
            ):
            ( "Scientific-orbiter probe",
                [tr18a, probodobodyneOcto, oxStatSolar,oxStatSolar, z100Bateries,z100Bateries, communotron16, clampotron,mobileProcessingLab, 
                 tr18a, flT200, flT100, rockomax48_7s]
            ):
            ( "Lifting stage",
                [tr18a, flT800, skipper]
            ):
            ( "Lifting stage",
                [tt38k, tt38k , flT800, flT800, seaLevel $ borrowed skipper]
            ):
            ( "Lifting stage",
                [tt38k, tt38k , flT800, flT800, seaLevel $ borrowed skipper]
            ):
            ( "Lifting stage",
                [tr18a , seaLevel s1SRB_KD25k,seaLevel s1SRB_KD25k]
            ):
          []
