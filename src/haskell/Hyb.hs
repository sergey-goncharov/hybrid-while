{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monads where

import Control.Monad.State
import qualified Control.Monad.Fail as Fail
  
import Data.List (transpose)
import qualified Data.Vector.Storable as Vec
import Numeric.GSL
import Numeric.LinearAlgebra
import Graphics.Plot

vec :: (Vec.Storable a) => a -> Vector a
vec x = Vec.singleton x

type Time = Double
type Stream a = [a]

showL :: (Show a) => [a] -> IO ()
showL []       = putStrLn "empty"
showL [x]      = putStrLn $ show x
showL (x : xs) = (putStrLn $ show x) >> showL xs

data SymTraj s = OdeTraj { 
-- trajectory specified by ode
    init  :: s,                   -- initial value
    flow  :: Time -> s -> s       -- eqations
} |
  FunTraj {   
-- explicitly given trajectory
    runTraj :: Time -> s -> s
} 

data Bound s = Bound {
  inv :: s -> Bool,               -- must hold throughout 
  dur :: Maybe Time               -- possible explicit bound
} 

predBound :: (s -> Bool) -> Bound s
predBound p = Bound p Nothing

numBound :: Time -> Bound s
numBound d = Bound (const True) $ Just d 

data HybSym s a = HybSym {runHybSym :: s -> TrajSeq s a}
data TrajSeq s a = TrivTraj a s | ConsTraj (SymTraj s) (Bound s) (HybSym s a)

instance Functor (HybSym s) where
    fmap f p = p >>= return . f

instance Applicative (HybSym s) where
    pure = return
    f <*> p = f >>= \g-> p >>= return . g

instance Monad (HybSym s) where
  return = HybSym . TrivTraj
  
  HybSym f >>= g = HybSym $ \init -> case (f init) of 
                        TrivTraj x next -> runHybSym (g x) next 
                        ConsTraj t d r  -> ConsTraj t d $ r >>= g
    
  fail = Fail.fail

instance Fail.MonadFail (HybSym s) where
  fail = undefined 

instance MonadState s (HybSym s) where
  state p = HybSym $ \init -> let (x, next) = p init in TrivTraj x next 

funTraj :: (Time -> s -> s) -> Bound s -> HybSym s s
funTraj f d = HybSym $ \init -> ConsTraj (FunTraj $ \t -> const $ f t init) d (state $ \s -> (s, s))

odeTraj :: (Time -> s -> s) -> Bound s -> HybSym s s
odeTraj f d = HybSym $ \init -> ConsTraj (OdeTraj init f) d (state $ \s -> (s, s))

wait :: Time -> HybSym a a
wait d = funTraj (const id) (numBound d) 

sample :: HybSym [Double] a -> [Double] -> Stream Time -> Stream [Double]

sample (HybSym f) init [] = []
sample (HybSym f) init xs@(x : _) = 
  case (f init) of 
    (TrivTraj _ next)                    -> map (const next) xs
    (ConsTraj (FunTraj f) (Bound p d) r) -> 
       let (ys, zs) = break (\t -> not (p $ f (t - x) init) || maybe False (t - x>) d) xs in
       map (\t -> f (t - x) init) ys ++ if (null zs) then [] else f (head zs - x) init : sample r (f (head zs - x) init) (tail zs)
     
    (ConsTraj (OdeTraj i f) (Bound p d) r) -> 
       let sol      = zip xs $ map toList $ toRows $ odeSolve f i (fromList xs);
           (ys, zs) = break (\(t, v) -> not (p v) || maybe False (t - x>) d) sol in
       map snd ys ++ if (null zs) then [] else (snd $ head zs) : sample r (snd $ head zs) (map fst $ tail zs)

fallTraj y x = map (\x -> x - y^2) x 

xdot t [x,v] = [v, -9.8]
ts = linspace 100 (0,20)
sol = odeSolve xdot [10,0] ts

-- A ball
ball = odeTraj xdot $ predBound (\[x,v] -> x >0 || v >=0)

-- A bouncing ball
bball = do ball; [x,v] <- get; put [x, -0.6 * v]; bball

-- program = funTraj fallTraj (Bound (all (>0.0)) Nothing)
main = mplot (fromList [0,0.01..3] : (map fromList $ transpose $ sample bball [2,0] [0,0.01..3]))


