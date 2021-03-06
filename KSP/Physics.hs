module KSP.Physics where

data ParentRelation = ParentRelation { parent :: Body, distance :: Double} -- Distance is measured center to center
data Atmosphere = Atmosphere { scaleHeight :: Double, atmosphericPressure :: Double} -- atmosphericPressure in atmospheres

noAtmosphere :: Atmosphere
noAtmosphere = Atmosphere {scaleHeight = 1.0, atmosphericPressure = 0.0}

data Body = Body { name :: String, 
                   parentRelation :: Maybe ParentRelation, 
                   radius :: Double, 
                   mu :: Double, 
                   rotationalPeriod :: Double,
                   atmosphere :: Atmosphere}

instance Show Body where
    show b = "<Celestial body = " ++ name b ++ ">"

-- Units stuff (should be moved to it's own module?)

kilometers, meters :: Double
kilometers = 1000.0
meters = 1.0

seconds, minutes, hours, days :: Double
seconds = 1.0
minutes = 60.0 * seconds
hours   = 60.0 * minutes
days    = 6.0 * hours

-- Planet calculation stuff

orbitalVelocity :: ParentRelation -> Double
orbitalVelocity (ParentRelation p d) = sqrt $ mu p / d

orbitalPeriod :: ParentRelation -> Double
orbitalPeriod pr@(ParentRelation _ d) = rotationalPeriodFromSurfaceSpeedAndRadius (orbitalVelocity pr) d

gravity :: Body -> Double -> Double
gravity p h = mu p / h **2

speedAtNewDistance :: Body -> Double -> Double -> Double -> Double
speedAtNewDistance p oldDistance oldSpeed newDistance = sqrt $ 2 * (energy + mu p / newDistance)
                        where energy = 0.5 * oldSpeed**2.0 - mu p / oldDistance

surfaceGravity :: Body -> Double
surfaceGravity p = gravity p (radius p)

muFromGravityAndRadius :: Double -> Double -> Double
muFromGravityAndRadius g r = g * r**2

rotationalPeriodFromSurfaceSpeedAndRadius :: Double -> Double -> Double
rotationalPeriodFromSurfaceSpeedAndRadius surfaceSpeed _radius = 2 * pi * _radius / surfaceSpeed

specificOrbitalEnergy :: Double -> Double -> Double
specificOrbitalEnergy _mu semiMajorAxis = -_mu / (2 * semiMajorAxis)

scapeVelocity :: Body -> Double -> Double
scapeVelocity p _distance = sqrt $ 2 * mu p / _distance

surfaceScapeVelocity :: Body -> Double
surfaceScapeVelocity p = scapeVelocity p (radius p)

relationToBody :: (ParentRelation -> a) -> (Body -> a)
relationToBody f p = f pr
                         where errorMessage = "El cuerpo " ++ name p ++ " no orbita alrededor un cuerpo padre!"
                               pr = maybe (error errorMessage) id $ parentRelation p

bodyOrbitalPeriod :: Body -> Double
bodyOrbitalPeriod = relationToBody orbitalPeriod

bodyOrbitalVelocity :: Body -> Double
bodyOrbitalVelocity = relationToBody orbitalVelocity

stationaryDistance :: Body -> Double
stationaryDistance p = (mu p * rotationalPeriod p ** 2.0 / (4.0 * pi ** 2.0)) ** (1.0 / 3.0)

densityAtHeight :: Atmosphere -> Double -> Double
densityAtHeight at h = 1.2230948554874 * (atmosphericPressure at) * exp (-h / scaleHeight at) -- Conversion atm to density

bodyDensityAtHeight :: Body -> Double -> Double
bodyDensityAtHeight p = densityAtHeight (atmosphere p)

standardDragCoefficient :: Double
standardDragCoefficient = 0.2

terminalVelocityAtHeight :: Body -> Double -> Double
terminalVelocityAtHeight p h = sqrt $ gravity p (h + radius p) / (bodyDensityAtHeight p h * standardDragCoefficient * 0.004) -- 1/2 de la conversion magica masa -> area

weightedMean :: Double -> Double -> Double -> Double -> Double
weightedMean wA a wB b = (wA * a + wB * b) / (wA + wB)

fallingSpeedAtSurface :: Body -> Double -> Double -> Double -> Double
fallingSpeedAtSurface p parachutesDrag parachutesMass payloadMass = sqrt $ q / drag
                                where q    = standardDragCoefficient * (terminalVelocityAtHeight p 0)**2.0
                                      drag = weightedMean parachutesMass parachutesDrag payloadMass 0.0 -- Pessimistic, calculated speed will be (just very slightly) higher than in reality

accelerationForFuelOptimalVerticalAscentAtHeight :: Body -> Double -> Double
accelerationForFuelOptimalVerticalAscentAtHeight p h = derivative h * terminalVelocityAtHeight p h
                                 where eps          = 1e-9
                                       derivative _h = (terminalVelocityAtHeight p (_h+eps/2) - terminalVelocityAtHeight p (_h-eps/2)) / eps

halfHohmannDeltaV :: Body -> Double -> Double -> Double
halfHohmannDeltaV p d1 d2 = sqrt (mu p) * (sqrt (2 * d2 / (d1 * (d1 + d2))) - sqrt (1.0 / d1))

fullHohmannDeltaV :: Body -> Double -> Double -> Double
fullHohmannDeltaV p d1 d2 = sqrt (mu p) * (sqrt (2 * d2 / (d1 * (d1 + d2))) - sqrt (1.0 / d1) + sqrt (1.0 / d2) - sqrt (2 * d1 / (d2 * (d1 + d2))))

printVelocityTable :: Body -> Double -> Double -> IO ()
printVelocityTable p maxH deltaH = mapM_ putStrLn [show h ++ " -> " ++ show (terminalVelocityAtHeight p h) | h <- [0,deltaH..maxH] ]

-- Planets!
kerbin :: Body

kerbinRadius, kerbinGravity, kerbinSurfaceRotationalSpeed,kerbinScaleHeight,kerbinAtmosphericPressure :: Double
kerbinRadius = 600 * kilometers -- KSP Wiki
kerbinGravity = 9.805 -- KSP Wiki
kerbinSurfaceRotationalSpeed = 174.6 -- NavBall in-game
kerbinScaleHeight = 5000.0  -- KSP Wiki
kerbinAtmosphericPressure = 1.0 -- KSP Wiki

kerbin = Body { name             = "Kerbin", 
                parentRelation   = Nothing, -- We have no sun (yet)
                radius           = kerbinRadius, 
                mu               = muFromGravityAndRadius kerbinGravity kerbinRadius,
                rotationalPeriod = rotationalPeriodFromSurfaceSpeedAndRadius kerbinSurfaceRotationalSpeed kerbinRadius,
                atmosphere       = Atmosphere { scaleHeight = kerbinScaleHeight, 
                                                atmosphericPressure = kerbinAtmosphericPressure
                                              }
              }

-- Moons!

mun :: Body

munOrbitingDistance, munMu, munRadius :: Double
munOrbitingDistance = 12000 * kilometers -- KSP Wiki / Tracking Station + Kerbin Radius
munRadius = 200 * kilometers -- KSP Wiki
munMu = 6.51384e10 -- KSP Wiki

mun = Body { name           = "Mun",
             parentRelation = Just ParentRelation {  parent          = kerbin,
                                                     distance        = munOrbitingDistance
                                                  },
             radius = munRadius,
             mu     = munMu,
             rotationalPeriod = bodyOrbitalPeriod mun, -- Haskell lazyness <3
             atmosphere = noAtmosphere
           }
