module Data.HexSpace.Grid (Grid,findPath) where

import Data.HexSpace.Coordinates
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Prelude hiding (any,all,or,and)


import Data.List (unfoldr)

import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding((:->)))

type Grid i a = Map (Cube i) a

findPath :: (Integral i, Ord i)
         => Set (Cube i)        -- ^ walkable cells
         -> Cube i                -- ^ start
         -> Cube i                -- ^ goal
         -> Maybe [Direction]     -- ^ shortest path (list of cardinals)
findPath walkable start goal
  | start == goal                       = Just []
  | start `Set.notMember` walkable
 || goal  `Set.notMember` walkable        = Nothing
  | otherwise                           = reconstruct <$> search
  where
    heuristic p       = distance (goal <-> p)

    neighbours p      =
      [ (d,n) | d <- [NW .. W]
              , let n = p <+> cardinal d
              , n `Set.member` walkable ]

    -- initial frontier, scores and parents
    open0             = PQ.singleton start (heuristic start)
    g0                = Map.singleton start 0      -- g(start)=0
    parent0           = Map.empty                  -- child ↦ (parent,dir)

    -- main loop
    search            = go open0 g0 parent0
      where
        go !open !gScore !parent =
          case PQ.minView open of
            Nothing                         -> Nothing
            Just (u :-> _, open')
              | u == goal                   -> Just parent
              | otherwise                   ->
                  let gu  = gScore Map.! u
                      step (d,v) (op,gs,pa) =
                        let gv  = gu + 1
                            better = maybe True (gv <) (Map.lookup v gs)
                        in if not better then (op,gs,pa)
                           else let f = gv + heuristic v
                                in ( PQ.insert v f op
                                   , Map.insert v gv gs
                                   , Map.insert v (u,d) pa )
                      (open1,g1,parent1) =
                           foldr step (open',gScore,parent) (neighbours u)
                  in go open1 g1 parent1

    -- path reconstruction (only called when goal was reached)
    reconstruct pa    = reverse $ unfoldr step goal
      where
        step p        = do (prev,dir) <- Map.lookup p pa
                           pure (dir,prev)
