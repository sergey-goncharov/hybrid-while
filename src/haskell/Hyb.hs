module Monads where
import Data.Monoid

-- The type of hybrid computations over the state space s, with results in x
--
-- HConv d f r represents a successful trajectory f : [0,d) -> s of duration d, 
-- plus the end result r
--
-- HDiv d f represents a divergent trajectory f : [0,d) -> s of duration d
-- 
-- Because dependent pairs and subtyping are not available, trajectories are 
-- represented by functions, totally defined over all reals (positive and negative)
--
data Hyb s x = HConv Double (Double -> s) x | HDiv Double (Double -> s)

evalTraj :: Hyb s x -> Double -> s
evalTraj (HConv d f r) = f
evalTraj (HDiv d f)    = f

duration :: Hyb s x -> Double
duration (HConv d f r) = d
duration (HDiv d f)    = d

sample :: Hyb s x -> Double -> [s]
sample p eps = h (duration p) (evalTraj p)
  where 
    h d f = if (d <= 0) then [] else (f 0 : h (d - eps) (\t -> f (t + eps))) 

-- This is a bit random
instance (Show x, Show s) => Show (Hyb s x) where
    show (HConv d f r) = 
      "duration: " ++ (show d) 
      ++ "; at 0: " ++ (show (f 0)) 
      ++ "; at " ++ (show d) ++ ": " ++ (show  r)

    show (HDiv d f) = 
      "duration: " ++ (show d) 
      ++ "; at 0: " ++ (show (f 0)) 

instance Functor (Hyb s) where
    fmap f p = p >>= return . f

instance Applicative (Hyb s) where
    pure = return
    f <*> p = f >>= \g-> (p >>= return . g)

instance Monad (Hyb s) where

  -- The real trajectory must have the type [0,0) -> s,
  -- so, there are no points on which we can evaluate it
  return = HConv 0 (const undefined)

  -- Divergent trajectories are not extendable
  (HDiv d f) >>= g = (HDiv d f)

  -- Extend a convergent trajectory to the right 
  (HConv d f r) >>= g = 
      case g r of 
        HConv d' h p -> HConv (d + d') g' p
        HDiv  d' h   -> HDiv  (d + d') g' 
      where g' x = if (x < d) then f x else evalTraj (g r) (x - d)   

wait :: Double -> x -> Hyb x x
wait d x = HConv d (const x) x

-- example: concatenate two trajectories
main = sample ((wait 1 1) >> (wait 1 2)) 0.1
