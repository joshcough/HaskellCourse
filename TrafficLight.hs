module TrafficLight where

import Control.Concurrent
import Control.Monad

data TrafficLight = Red | Yellow | Green deriving Show
data TimedTrafficLight = TimedTrafficLight TrafficLight Int

instance Show TimedTrafficLight where
  show (TimedTrafficLight light time) = show (light, time)

lightNext :: TrafficLight -> TrafficLight
lightNext Red    = Green
lightNext Yellow = Red
lightNext Green  = Yellow

lightDuration :: TrafficLight -> Int
lightDuration Red    = 10
lightDuration Yellow = 3
lightDuration Green  = 7

red    = mkLight Red
yellow = mkLight Yellow
green  = mkLight Green

mkLight :: TrafficLight -> TimedTrafficLight
mkLight light = TimedTrafficLight light (lightDuration light)

tick :: TimedTrafficLight -> TimedTrafficLight
tick (TimedTrafficLight tlight 0) = mkLight $ lightNext tlight
tick (TimedTrafficLight tlight n) = TimedTrafficLight tlight (n-1)

shouldHitGas :: TrafficLight -> Bool
shouldHitGas Red    = False
shouldHitGas Green  = True
shouldHitGas Yellow = True

shouldHitGas' :: TimedTrafficLight -> Bool
shouldHitGas' (TimedTrafficLight Red    _) = False
shouldHitGas' (TimedTrafficLight Green  _) = True
shouldHitGas' (TimedTrafficLight Yellow n) | n > 1 = True
shouldHitGas' (TimedTrafficLight Yellow _) = True

loopLight :: TimedTrafficLight -> Int -> IO ()
loopLight light timeRemaining = do
  putStrLn $ show light
  if (timeRemaining == 0)
  then return ()
  else do
    threadDelay 1000000
    loopLight (tick light) (timeRemaining-1)

