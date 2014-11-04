module KSP.Parts where

data MotorSpec = MotorSpec {motorThrust :: Double, isp :: Double}

g0 :: Double
g0 = 9.82 -- Por cuestiones locas del KSP, se usa esta constante de conversion y no la posta (???)

data Part = Part {motorSpec :: Maybe MotorSpec, mass :: Double, dryMass :: Double}

motorBurnRate :: MotorSpec -> Double
motorBurnRate spec = motorThrust spec / (isp spec * g0)

thrust :: Part -> Double
thrust = maybe 0.0 motorThrust . motorSpec

fuel :: Part -> Double
fuel p = mass p - dryMass p

burnRate :: Part -> Double
burnRate = maybe 0.0 motorBurnRate . motorSpec

-- Parts!! (Los valores de todas las partes se obtienen mirando el juego directamente, en el VAB)
-- TODO: Mejorar el modelado de Isp (la eficiencia en vacio y en atmosfera es diferente)

commandPodMk1 :: Part
flR25rcs,ftx2,flT400,flT800,stratusV :: Part
ftx2FuelDuct :: Part
lvT30,lvT45,lv909,rt10 :: Part
rv105 :: Part
tt38k, tr18a :: Part
eas4Strut :: Part
aerodynamicNoseCone,avR8,avt1 :: Part
mk16Parachute,lt1LandingStruts :: Part

-- Command Pods
commandPodMk1 = Part {motorSpec = Nothing, mass = 0.8, dryMass = 0.8}

-- Fuels
flR25rcs = Part {motorSpec = Nothing, mass = 0.55, dryMass = 0.55}
ftx2 = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}
flT400 = Part {motorSpec = Nothing, mass = 2.25, dryMass = 0.25}
flT800 = Part {motorSpec = Nothing, mass = 4.5, dryMass = 0.5}
stratusV = Part {motorSpec = Nothing, mass = 0.235, dryMass = 0.235}

ftx2FuelDuct = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Engines
lvT30 = Part {motorSpec = Just $ MotorSpec {motorThrust = 215.0, isp = 370}, mass = 1.25, dryMass = 1.25}
lvT45 = Part {motorSpec = Just $ MotorSpec {motorThrust = 200.0, isp = 370}, mass = 1.5, dryMass = 1.5}
lv909 = Part {motorSpec = Just $ MotorSpec {motorThrust = 50.0, isp = 390}, mass = 0.5, dryMass = 0.5}
rt10 = Part {motorSpec = Just $ MotorSpec {motorThrust = 250.0, isp = 230}, mass = 3.7475, dryMass = 0.5}

-- RCS
rv105 = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Decouplers

tt38k = Part {motorSpec = Nothing, mass = 0.025, dryMass = 0.025}
tr18a = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Structural
eas4Strut = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Aerodynamic
aerodynamicNoseCone = Part {motorSpec = Nothing, mass = 0.03, dryMass = 0.03}
avR8 = Part {motorSpec = Nothing, mass = 0.02, dryMass = 0.02}
avt1 = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}

-- Utility
mk16Parachute = Part {motorSpec = Nothing, mass = 0.01, dryMass = 0.01}
lt1LandingStruts = Part {motorSpec = Nothing, mass = 0.05, dryMass = 0.05}
