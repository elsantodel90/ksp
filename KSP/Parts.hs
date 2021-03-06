module KSP.Parts where

import Data.Maybe

data MotorSpec = MotorSpec {motorThrust :: Double, seaLevelIsp :: Double ,isp :: Double}

g0 :: Double
g0 = 9.82 -- Por cuestiones locas del KSP, se usa esta constante de conversion y no la posta (???)

data Part = Part {motorSpec :: Maybe MotorSpec, mass :: Double, dryMass :: Double}

seaLevel :: Part -> Part
seaLevel part = part {motorSpec = Just newMotorSpec}
                  where oldMotorSpec = fromJust $ motorSpec part
                        newMotorSpec = oldMotorSpec {isp = seaLevelIsp oldMotorSpec}

-- Turns a part to an equivalent with zero mass. Useful to express that an engine from a later stage is used (as in asparagus staging)
borrowed :: Part -> Part
borrowed p = Part {motorSpec = motorSpec p, mass = 0.0, dryMass = 0.0}

motorBurnRate :: MotorSpec -> Double
motorBurnRate spec = motorThrust spec / (isp spec * g0)

thrust :: Part -> Double
thrust = maybe 0.0 motorThrust . motorSpec

fuel :: Part -> Double
fuel p = mass p - dryMass p

burnRate :: Part -> Double
burnRate = maybe 0.0 motorBurnRate . motorSpec

-- Parts!! (Los valores de todas las partes se obtienen mirando el juego directamente, en el VAB)

commandPodMk1 :: Part
stayputnik,probodobodyneOcto :: Part
flR25rcs,ftx2,flT100, flT200, flT400,flT800,stratusV :: Part
ftx2FuelDuct :: Part
lvT30,lvT45,lv909, rockomax48_7s,skipper,rt10,rockomaxBACC, sepratron1,s1SRB_KD25k :: Part
rv105 :: Part
tt38k, tr18a :: Part
eas4Strut,modGirder :: Part
aerodynamicNoseCone,avR8,avt1 :: Part
mk16Parachute, mk2radialParachute, lt1LandingStruts,z100Bateries,oxStatSolar,clampotron :: Part
communotron16,misteryGoo,sc9001scienceJr,mobileProcessingLab :: Part

-- Command Pods
commandPodMk1 = Part {motorSpec = Nothing, mass = 0.84, dryMass = 0.84}

-- Probes
stayputnik = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}
probodobodyneOcto = Part {motorSpec = Nothing, mass = 0.1, dryMass = 0.1}

-- Fuels
flR25rcs = Part {motorSpec = Nothing, mass = 0.55, dryMass = 0.55}
ftx2 = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}
flT100 = Part {motorSpec = Nothing, mass = 0.5625, dryMass = 0.0625}
flT200 = Part {motorSpec = Nothing, mass = 1.125, dryMass = 0.125}
flT400 = Part {motorSpec = Nothing, mass = 2.25, dryMass = 0.25}
flT800 = Part {motorSpec = Nothing, mass = 4.5, dryMass = 0.5}
stratusV = Part {motorSpec = Nothing, mass = 0.235, dryMass = 0.235}

ftx2FuelDuct = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Engines
lvT30 = Part {motorSpec = Just $ MotorSpec {motorThrust = 215.0, seaLevelIsp = 320, isp = 370}, mass = 1.25, dryMass = 1.25}
lvT45 = Part {motorSpec = Just $ MotorSpec {motorThrust = 200.0, seaLevelIsp = 320, isp = 370}, mass = 1.5, dryMass = 1.5}
lv909 = Part {motorSpec = Just $ MotorSpec {motorThrust = 50.0, seaLevelIsp = 300, isp = 390}, mass = 0.5, dryMass = 0.5}
rockomax48_7s = Part {motorSpec = Just $ MotorSpec {motorThrust = 30.0, seaLevelIsp = 300, isp = 350}, mass = 0.1, dryMass = 0.1}
skipper = Part {motorSpec = Just $ MotorSpec {motorThrust = 650.0, seaLevelIsp = 320, isp = 370}, mass = 3.0, dryMass = 3.0}

rt10 = Part {motorSpec = Just $ MotorSpec {motorThrust = 250.0, seaLevelIsp = 225, isp = 240}, mass = 3.75, dryMass = 0.5}
rockomaxBACC = Part {motorSpec = Just $ MotorSpec {motorThrust = 315.0, seaLevelIsp = 230, isp = 250}, mass = 7.875, dryMass = 1.5}
sepratron1 = Part {motorSpec = Just $ MotorSpec {motorThrust = 18.0, seaLevelIsp = 100, isp = 100}, mass = 0.0725, dryMass = 0.0125}
s1SRB_KD25k = Part {motorSpec = Just $ MotorSpec {motorThrust = 650.0, seaLevelIsp = 230, isp = 250}, mass = 21.75, dryMass = 3.0}

-- RCS
rv105 = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Decouplers

tt38k = Part {motorSpec = Nothing, mass = 0.025, dryMass = 0.025}
tr18a = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Structural
eas4Strut = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}
modGirder = Part {motorSpec = Nothing, mass = 0.125, dryMass = 0.125}

-- Aerodynamic
aerodynamicNoseCone = Part {motorSpec = Nothing, mass = 0.03, dryMass = 0.03}
avR8 = Part {motorSpec = Nothing, mass = 0.02, dryMass = 0.02}
avt1 = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Utility
mk16Parachute = Part {motorSpec = Nothing, mass = 0.1, dryMass = 0.1}
mk2radialParachute = Part {motorSpec = Nothing, mass = 0.15, dryMass = 0.15}
lt1LandingStruts = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}
z100Bateries = Part {motorSpec = Nothing, mass = 0.005, dryMass = 0.005}
oxStatSolar = Part {motorSpec = Nothing, mass = 0.005, dryMass = 0.005}
clampotron = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Science
communotron16       = Part {motorSpec = Nothing, mass = 0.005, dryMass = 0.005}
misteryGoo          = Part {motorSpec = Nothing, mass = 0.15, dryMass = 0.15}
sc9001scienceJr     = Part {motorSpec = Nothing, mass = 0.2, dryMass = 0.2}
mobileProcessingLab = Part {motorSpec = Nothing, mass = 3.5, dryMass = 3.5}
