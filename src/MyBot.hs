module Main where

import Data.List
import Data.Maybe (mapMaybe)
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Ants

tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

updateGameTurn :: World -> GameTurn -> [Order] -> GameTurn
updateGameTurn w gt [] = gt
updateGameTurn w gt orders = updateGameTurn w (unoccupied w gt (head orders)) (tail orders)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

{- |
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  let generatedOrders = map generateOrders $ myAnts $ ants gs
      unoccupiedOrders = mapMaybe (tryOrder (world gs)) generatedOrders
      gt = updateGameTurn (world gs) (GameTurn {ordersMade = Map.empty}) unoccupiedOrders
      orders = Map.elems $ ordersMade gt
    
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab: