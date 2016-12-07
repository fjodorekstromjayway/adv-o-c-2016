module Day1 where

import Prelude
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (drop, stripPrefix, take)
import Partial.Unsafe (unsafePartial)

data Direction = North
  | East
  | South
  | West

type State = {
  facing :: Direction,
  x :: Int,
  y :: Int
}
 
go :: Direction -> Int -> State -> State
go North  steps state  = { x: state.x,          y: state.y + steps, facing: North }
go East   steps state  = { x: state.x + steps,  y: state.y,         facing: East  }
go South  steps state  = { x: state.x,          y: state.y - steps, facing: South }
go West   steps state  = { x: state.x - steps,  y: state.y,         facing: West  }

letterToDirection :: String -> State -> Direction
letterToDirection "R" state  = right state.facing
letterToDirection "L" state  = left state.facing
letterToDirection _   _      = North

strToInt :: String -> Int
strToInt str = unsafePartial $ fromJust $ fromString $ drop 1 str

left :: Direction -> Direction
left North  = West
left West   = South
left South  = East
left East   = North

right :: Direction -> Direction   
right North = East
right East  = South
right South = West
right West  = North

showState :: State -> String
showState obj = show obj.x <> ", " <> show obj.y

update :: State -> String -> State
update currentState inputItem = go (letterToDirection (take 1 inputItem) currentState) (strToInt(inputItem)) (currentState)


navigate = foldl (update) state input where
  state = { facing: North, x: 0, y: 0 }
  input = [ "R3", "L5", "R2", "L1", "L2", "R5", "L2", "R2", "L2", "L2", "L1", "R2", "L2", "R4", "R4", "R1", "L2", "L3", "R3", "L1", "R2", "L2", "L4", "R4", "R5", "L3", "R3", "L3", "L3", "R4", "R5", "L3", "R3", "L5", "L1", "L2", "R2", "L1", "R3", "R1", "L1", "R187", "L1", "R2", "R47", "L5", "L1", "L2", "R4", "R3", "L3", "R3", "R4", "R1", "R3", "L1", "L4", "L1", "R2", "L1", "R4", "R5", "L1", "R77", "L5", "L4", "R3", "L2", "R4", "R5", "R5", "L2", "L2", "R2", "R5", "L2", "R194", "R5", "L2", "R4", "L5", "L4", "L2", "R5", "L3", "L2", "L5", "R5", "R2", "L3", "R3", "R1", "L4", "R2", "L1", "R5", "L1", "R5", "L1", "L1", "R3", "L1", "R5", "R2", "R5", "R5", "L4", "L5", "L5", "L5", "R3", "L2", "L5", "L4", "R3", "R1", "R1", "R4", "L2", "L4", "R5", "R5", "R4", "L2", "L2", "R5", "R5", "L5", "L2", "R4", "R4", "L4", "R1", "L3", "R1", "L1", "L1", "L1", "L4", "R5", "R4", "L4", "L4", "R5", "R3", "L2", "L2", "R3", "R1", "R4", "L3", "R1", "L4", "R3", "L3", "L2", "R2", "R2", "R2", "L1", "L4", "R3", "R2", "R2", "L3", "R2", "L3", "L2", "R4", "L2", "R3", "L4", "R5", "R4", "R1", "R5", "R3" ]
