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

updateGameTurnFood :: World -> GameTurn -> [(Food, Maybe Order)] -> GameTurn
updateGameTurnFood w gt [] = gt
updateGameTurnFood w gt pointorders = updateGameTurnFood w (unoccupiedFood w gt (head pointorders)) (tail pointorders)

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
  let --Food gathering.
      shortetsFoodOrders = map snd (sort [( distance gp (point myant) food_loc,
                                   (food_loc,
                                    tryOrder (world gs) [Order {ant = myant,
                                                                direction = (fst (directions (world gs) (point myant) food_loc))},
                                                         Order {ant = myant,
                                                                direction = (snd (directions (world gs) (point myant) food_loc))}
                                                         ]
                                   )
                                 ) | food_loc <- food gs, myant <- myAnts (ants gs)])

      food_gt = updateGameTurnFood (world gs) (GameTurn {ordersMade = Map.empty, foodTargets = Map.empty}) shortetsFoodOrders

      --Exploring the map

      --Unblocking hills
      hillOrders = [[Order{ant = Ant{point =(hillpoint h), owner = Me}, direction = North},
                     Order{ant = Ant{point =(hillpoint h), owner = Me}, direction = South},
                     Order{ant = Ant{point =(hillpoint h), owner = Me}, direction = West},
                     Order{ant = Ant{point =(hillpoint h), owner = Me}, direction = East}] | h <- (hills gs), (hillpoint h) `elem` (map point (myAnts (ants gs)))]
      unoccupyHillsOrders = mapMaybe (tryOrder (world gs)) hillOrders
      hill_gt = updateGameTurn (world gs) food_gt unoccupyHillsOrders

      --Get final orders that will be returned
      orders = Map.elems $ ordersMade hill_gt

  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
