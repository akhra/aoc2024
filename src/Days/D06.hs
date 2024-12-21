module Days.D06 where

import Lib ( Dispatch, dispatchWith )
import Lib.Field ( Field, Point )
import Lib.Field qualified as Field
import Control.Arrow ( (&&&) )
import Control.Monad ( guard, join )
import Data.Maybe ( isJust )
import Data.List qualified as List
import Data.Set qualified as Set

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = length . List.nub . run . parseInput

part2 :: String -> Int
part2 = length . run2 . parseInput

data Facing
  = North
  | East
  | South
  | West
  deriving (Show, Enum, Eq, Ord)

data Patrol = Patrol
  { position :: Point
  , facing   :: Facing
  } deriving (Show, Eq, Ord)

data World = World
  { field  :: Field Bool
  , patrol :: Patrol
  } deriving Show

run :: World -> [Point]
run initial = go initial [initial.patrol.position]
  where
  go world visited = case takeStep world of
    Just world' -> go world' (world'.patrol.position : visited)
    Nothing     -> visited

run2 :: World -> [()]
run2 World{..} = do
  pos <- Field.indices field
  guard $ patrol.position /= pos
  guard $ maybe False not (Field.lookup pos field)
  let field' = Field.update pos True field
  guard $ runLoopDetect (World field' patrol)
  pure ()

runLoopDetect :: World -> Bool
runLoopDetect initial = go initial Set.empty
  where
  go :: World -> Set.Set Patrol -> Bool
  go world prior = case takeStep world of
    Just world'
      -- significant optimization: only store/check status after turning
      | world'.patrol.facing /= world.patrol.facing
        -> if Set.member world'.patrol prior
          then True
          else go world' $ Set.insert world'.patrol prior
      | otherwise
        -> go world' prior
    Nothing -> False

takeStep :: World -> Maybe World
takeStep world@World{..} = do
  (into, isObstacle) <- tryStep world
  if isObstacle
    then takeStep $ World field patrol{ facing = turnCW patrol.facing }
    else pure $ World field patrol{ position = into }

tryStep :: World -> Maybe (Point, Bool)
tryStep (World field (Patrol point facing))
  = sequence (stepTo, Field.lookup stepTo field)
  where
  stepTo = case facing of
    North -> point + Field.north
    East  -> point + Field.east
    South -> point + Field.south
    West  -> point + Field.west

turnCW :: Facing -> Facing
turnCW West   = North
turnCW facing = succ facing


--
data InputEntity
  = IEmpty
  | IObstacle
  | IPatrol Facing
  deriving (Show, Eq)

parseInput :: String -> World
parseInput = makeWorld . Field.parse parseEntity

parseEntity :: Char -> InputEntity
parseEntity = \case
  '.' -> IEmpty
  '#' -> IObstacle
  '^' -> IPatrol North
  '>' -> IPatrol East
  'v' -> IPatrol South
  '<' -> IPatrol West
  c   -> error $ "can't parse input character " <> show c

makeWorld :: Field InputEntity -> World
makeWorld
  = uncurry World
  . (fmap (fmap (==IObstacle)) &&& findPatrol)

findPatrol :: Field InputEntity -> Patrol
findPatrol inputField = maybe (error "no patrol found") id $ do
  let
    justPatrol patrol@(IPatrol _) = Just patrol
    justPatrol _                  = Nothing
    justPatrols = Field.map justPatrol inputField
  pos <- Field.findIndex isJust justPatrols
  IPatrol facing <- join $ Field.lookup pos justPatrols
  pure $ Patrol pos facing
