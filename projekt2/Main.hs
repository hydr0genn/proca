import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy

--g = 6.67430e-11
g = 0.1
t = 0.02
scaleFactor = 100


data ObjectState = ObjectState
    { position :: (Double, Double)
    , velocity :: (Double, Double)
    , acceleration :: (Double, Double)
    , mass :: Double
    -- , previous :: Maybe ObjectState
    } deriving (Show)

type World = (ObjectState, ObjectState, Picture)  -- (planet, sun)

planet :: ObjectState
planet = ObjectState
    { position = (-1, 2)
    , velocity = (0.01, 0.01)
    , acceleration = (0, 0)
    -- , mass = 5.97e24
    , mass = 0.01
    -- , Nothing
    }

sun :: ObjectState
sun = ObjectState
    { position = (2, 0)
    , velocity = (0, 0)
    , acceleration = (0, 0)
    -- , mass = 1.99e30
    , mass = 1
    -- , Nothing
    }


distanceX :: ObjectState -> ObjectState -> Double
distanceX ob1 ob2 = (x2 - x1)
  where
    (x1, _) = position ob1
    (x2, _) = position ob2

distanceY :: ObjectState -> ObjectState -> Double
distanceY ob1 ob2 = (y2 - y1)
  where
    (_, y1) = position ob1
    (_, y2) = position ob2

distance :: ObjectState -> ObjectState -> Double
distance ob1 ob2 = sqrt (((distanceX ob1 ob2)**2) + ((distanceY ob1 ob2)**2))

acceleration' :: ObjectState -> ObjectState -> Double
acceleration' ob1 ob2 = g * mass ob2 / (distance ob1 ob2)**2

accelerationX :: ObjectState -> ObjectState -> Double
accelerationX ob1 ob2 = acceleration' ob1 ob2 * distanceX ob1 ob2 / distance ob1 ob2 

accelerationY :: ObjectState -> ObjectState -> Double
accelerationY ob1 ob2 = acceleration' ob1 ob2 * distanceY ob1 ob2 / distance ob1 ob2 

velocityX :: ObjectState -> ObjectState -> Double
velocityX ob1 ob2 = u + accelerationX ob1 ob2 * t
  where 
    (u, _) = velocity ob1

velocityY :: ObjectState -> ObjectState -> Double
velocityY ob1 ob2 = u + accelerationY ob1 ob2 * t
  where 
    (_, u) = velocity ob1

velocityTotal :: ObjectState -> ObjectState -> Double
velocityTotal ob1 ob2 = sqrt (((velocityX ob1 ob2)**2) + ((velocityY ob1 ob2)**2))

positionX :: ObjectState -> ObjectState -> Double
positionX ob1 ob2 = px + u * t + 0.5 * accelerationX ob1 ob2 * t**2
  where 
    (px, _) = position ob1 
    (u, _) = velocity ob1

positionY :: ObjectState -> ObjectState -> Double
positionY ob1 ob2 = py + u * t + 0.5 * accelerationY ob1 ob2 * t**2
  where 
    (_, py) = position ob1 
    (_, u) = velocity ob1


-- nextState :: ObjectState -> ObjectState -> Double -> Double -> Double -> Double -> Double -> Double -> ObjectState
-- nextState state ob2 x1 x2 v1 v2 a1 a2 = state {
--       position = (x1, x2)
--     , velocity = (v1, v2)
--     , acceleration = (a1, a2)
--     , mass = mass state
--     -- , previous = Just state 
-- } where 
--     x1 = positionX state ob2
--     x2 = positionY state ob2
--     v1 = velocityX state ob2
--     v2 = velocityY state ob2
--     a1 = accelerationX state ob2
--     a2 = accelerationY state ob2

nextState :: ObjectState -> ObjectState -> ObjectState
nextState state ob2 = state {
      position = (x1, x2)
    , velocity = (v1, v2)
    , acceleration = (a1, a2)
    , mass = mass state
    -- , previous = Just state 
} where 
    x1 = positionX state ob2
    x2 = positionY state ob2
    v1 = velocityX state ob2
    v2 = velocityY state ob2
    a1 = accelerationX state ob2
    a2 = accelerationY state ob2


-- Gloss Display Function
drawWorld :: World -> Picture
drawWorld (planetState, sunState) = Pictures
    [ translatePlanet planetState $ Color blue $ circleSolid 10
    , translatePlanet sunState    $ Color yellow $ circleSolid 20
    ]

translatePlanet :: ObjectState -> Picture -> Picture
translatePlanet ob pic = translate x' y' pic
  where
    (x, y) = position ob
    x' = realToFrac x * scaleFactor
    y' = realToFrac y * scaleFactor

-- Update physics
updateWorld :: Float -> World -> World
updateWorld _ (p, s, bg) =
    let p' = nextState p s
        s' = nextState s p
    in (p', s', bg)

main :: IO ()
main = do
    -- Load PNG or JPG background (must be in project directory)
    Just bg <- loadJuicyPNG "amongus.png"

    -- Start simulation
    play
        (InWindow "Planet Orbit Simulation" (800, 600) (100, 100))
        black
        (round $ 1 / t)
        (planet, sun, scaleBackground bg)
        drawWorld
        (\_ w -> w)
        updateWorld


-- -- Main Gloss loop
-- main :: IO ()
-- main = play
--     (InWindow "Planet Orbit Simulation" (800, 600) (100, 100))  -- window settings
--     black      -- background color
--     (round $ 1 / t)  -- steps per second
--     (planet, sun)    -- initial world state
--     drawWorld        -- drawing function
--     (\_ w -> w)      -- input handler (no controls)
--     updateWorld      -- simulation step




-- -- Simulation loop
-- simulate :: Int -> ObjectState -> ObjectState -> IO ()
-- simulate 0 _ _ = return ()
-- simulate n p s = do
--     let p' = nextState p s
--     let s' = nextState s p
--     -- printf "Step %d\n" (100 - n)
--     -- printf "Planet Position: %.5f, %.5f\n" (fst $ position p') (snd $ position p')
--     -- printf "Sun Position: %.5f, %.5f\n\n" (fst $ position s') (snd $ position s')
--     printf "%.5f, %.5f\n" (fst $ position p') (snd $ position p')
--     printf "%.5f, %.5f\n\n" (fst $ position s') (snd $ position s')
--     threadDelay $ round (t * 1000000)  -- sleep for t seconds
--     simulate (n - 1) p' s'

-- -- Main entry point
-- main :: IO ()
-- main = simulate 100 planet sun  -- Run for 100 steps