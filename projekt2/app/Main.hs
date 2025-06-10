import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Text.Read (readMaybe) 

--g = 6.67430e-11
g :: Double
g = 0.013
t :: Double
t = 0.02
scaleFactor :: Float
scaleFactor = 100


windowWidth, windowHeight :: Float
windowWidth = 800
windowHeight = 600

halfWidth, halfHeight :: Float
halfWidth = windowWidth / 2
halfHeight = windowHeight / 2

data ObjectState = ObjectState
    { position :: (Double, Double)
    , velocity :: (Double, Double)
    , acceleration :: (Double, Double)
    , mass :: Double
    } deriving (Show)

data World = World
    { planetState :: ObjectState
    , sunState :: ObjectState
    , background :: Picture
    , running :: Bool
    , clickState :: ClickState
    }

data ClickState = AwaitingMassRatioInput String
                | WaitingForPlanetClick
                | WaitingForSunClick Double Double
                | SimulationStarted
                deriving (Show)

planet :: ObjectState
planet = ObjectState
    { position = (-100, 100)
    , velocity = (0.05, 0.05)
    , acceleration = (0, 0)
    , mass = 0.01
    }

sun :: ObjectState
sun = ObjectState
    { position = (100, 100)
    , velocity = (0, 0)
    , acceleration = (0, 0)
    , mass = 100 
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


nextState :: ObjectState -> ObjectState -> ObjectState
nextState state ob2 = state {
      position = (x1, x2)
    , velocity = (v1, v2)
    , acceleration = (a1, a2)
    , mass = mass state
} where
    x1 = positionX state ob2
    x2 = positionY state ob2
    v1 = velocityX state ob2
    v2 = velocityY state ob2
    a1 = accelerationX state ob2
    a2 = accelerationY state ob2

isOutOfBounds :: ObjectState -> Bool
isOutOfBounds obj = abs x' > halfWidth || abs y' > halfHeight
  where
    (x, y) = position obj
    x' = realToFrac x * scaleFactor
    y' = realToFrac y * scaleFactor


drawWorld :: World -> Picture
drawWorld (World p s bg _ (AwaitingMassRatioInput inputStr)) = Pictures
    [ bg
    , translate (-380) 50 $ scale 0.2 0.2 $ color white $ text "Enter Sun/Planet mass ratio and press Enter:"
    , translate (-150) (-20) $ scale 0.25 0.25 $ color yellow $ text inputStr
    , translate (-50) (-50) $ scale 0.1 0.1 $ color yellow $ text " (e.g., 1000)"
    ]
drawWorld (World p s bg _ clickState) = Pictures
  [ bg
  , translatePlanet s $ Color yellow $ circleSolid 20
  , translatePlanet p $ Color blue   $ circleSolid 10
  , translate (-390) (-270) $ scale 0.1 0.1 $ color white $ text statsTextPlanet
  , translate (-390) (-290) $ scale 0.1 0.1 $ color white $ text statsTextSun
  , instructionMessage clickState 
  ]
  where
    statsTextPlanet = formatObjectInfo "Planet" p s
    statsTextSun = formatObjectInfo "Sun" s p

instructionMessage :: ClickState -> Picture
instructionMessage WaitingForPlanetClick =
    translate (-250) 0 $ scale 0.2 0.2 $ color green $ text "Click to set the Planet's position"
instructionMessage (WaitingForSunClick _ _) =
    translate (-250) 0 $ scale 0.2 0.2 $ color green $ text "Click to set the Sun's position"
instructionMessage _ = Blank


handleEvent :: Event -> World -> World
handleEvent (EventKey (Char c) Down _ _) world@(World {clickState = AwaitingMassRatioInput s})
    | c `elem` "0123456789." = world { clickState = AwaitingMassRatioInput (s ++ [c]) }
    | otherwise = world

handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world@(World {clickState = AwaitingMassRatioInput s})
    | not (null s) = world { clickState = AwaitingMassRatioInput (init s) }
    | otherwise = world

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {planetState = p, sunState = s, clickState = AwaitingMassRatioInput str}) =
    case readMaybe str of
        Just ratio ->
            let newSun = s { mass = mass p * ratio }
            in world { sunState = newSun, clickState = WaitingForPlanetClick }
        Nothing ->
            world

handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) (World p s bg _ WaitingForPlanetClick) =
    let
        planetX_double = realToFrac mx / realToFrac scaleFactor
        planetY_double = realToFrac my / realToFrac scaleFactor
    in
        World p { position = (planetX_double, planetY_double) } s bg False (WaitingForSunClick planetX_double planetY_double)

handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) (World p s bg _ (WaitingForSunClick px py)) =
    let
        sunX_double = realToFrac mx / realToFrac scaleFactor
        sunY_double = realToFrac my / realToFrac scaleFactor
        initialPlanet = p { position = (px, py) }
        initialSun = s { position = (sunX_double, sunY_double) }
    in
        World initialPlanet initialSun bg True SimulationStarted

handleEvent _ w = w

formatObjectInfo :: String -> ObjectState -> ObjectState -> String
formatObjectInfo name a b = unlines
  [ name ++ ":"
  , "  Mass: " ++ printf "%.2f" (mass a)
  , "  Velocity: " ++ printf "%.5f" (velocityTotal a b)
  , "  Acceleration: " ++ printf "%.5f" (acceleration' a b)
  , "  Mass: " ++ printf "%.5f" (mass a)
  , "  Position: (" ++ printf "%.2f" x ++ ", " ++ printf "%.2f" y ++ ")"
  ]
  where
    (x, y) = position a

translatePlanet :: ObjectState -> Picture -> Picture
translatePlanet ob pic = translate x' y' pic
  where
    (x, y) = position ob
    x' = realToFrac x * scaleFactor
    y' = realToFrac y * scaleFactor

updateWorld :: Float -> World -> World
updateWorld _ (World p s bg True SimulationStarted)
  | isOutOfBounds p || isOutOfBounds s = World p s bg False SimulationStarted
  | otherwise = World (nextState p s) (nextState s p) bg True SimulationStarted
updateWorld _ world = world

main :: IO ()
main = do
    mBg <- loadJuicyPNG "amongus.png"
    let bg = case mBg of
               Just pic -> pic
               Nothing  -> Blank 

    play
      (InWindow "Proca grawitacyjna" (round windowWidth, round windowHeight) (100, 100))
      black
      (round $ 1 / t)
      (World planet sun (scaleBackground bg) False (AwaitingMassRatioInput ""))
      drawWorld
      handleEvent
      updateWorld

scaleBackground :: Picture -> Picture
scaleBackground = scale 1.0 1.0