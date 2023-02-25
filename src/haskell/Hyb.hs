{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monads where

import Control.Monad.State
import qualified Control.Monad.Fail as Fail
  
import Data.List (transpose, findIndex)
import qualified Data.Vector.Storable as Vec
import Numeric.GSL
import Numeric.LinearAlgebra
import Graphics.Plot

import Debug.Trace

vec :: (Vec.Storable a) => a -> Vector a
vec x = Vec.singleton x

type Time = Double
type Stream a = [a]

showL :: (Show a) => [a] -> IO ()
showL []       = putStrLn "empty"
showL [x]      = putStrLn $ show x
showL (x : xs) = (putStrLn $ show x) >> showL xs

data SymTraj s = OdeTraj {          -- trajectory specified by ode
    init  :: s,                     -- initial value
    flow  :: Time -> s -> s         -- eqations
} | FunTraj {                       -- trajectory, given explicitly
    runTraj :: Time -> s -> s       
}

data Bound s = Bound {
    inv :: s -> Bool,               -- must hold throughout 
    dur :: Maybe Time               -- possible explicit bound
} 

predBound :: (s -> Bool) -> Bound s
predBound p = Bound p Nothing

numBound :: Time -> Bound s
numBound = Bound (const True) . Just

data HybSym s a  = HybSym {runHybSym :: s -> TrajSeq s a}
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

-- slightly more versatile than odeSolve 
odeSolve' :: (Double -> [Double] -> [Double]) -> [Double] -> Vector Double -> Matrix Double
odeSolve' f init ts | Vec.null ts            = (0 >< length init) []
odeSolve' f init ts | Vec.null (Vec.tail ts) = fromLists [init]
odeSolve' f init ts | otherwise              = odeSolve f init ts

sample :: HybSym [Double] a -> [Double] -> Vector Time -> Matrix Double

sample (HybSym f) start ts | Vec.null ts = (0 >< length start) []
sample (HybSym f) start ts | not $ Vec.null ts = case f start of 
    (TrivTraj _ next)                    
        -> fromColumns $ [Vec.generate (Vec.length ts) (const x) | x <- next]

    (ConsTraj (FunTraj f) (Bound p d) r)
        -> let sol = fromLists $ Vec.foldr 
                     (\t xs -> let x = f (t - ts ! 0) start in 
                               if p x && maybe True (t - ts !0 <=) d 
                               then x : xs else [x]) [] ts in
           sol === sample r (f (ts ! (rows sol - 1) - ts ! 0) start) (Vec.drop (rows sol) ts)
     
    (ConsTraj (OdeTraj init f) (Bound p d) r) 
        -> let (ts',ts'') = case d of Nothing -> (ts, Vec.empty)
                                      Just d  -> Vec.span (<= d + ts ! 0) ts in
           let ext  = if Vec.null ts'' then ts' else ts' Vec.++ vector [ts'' ! 0] in                            
           let sol' = odeSolve' f init ext in
           let sol  = sol' ? [0..Vec.length ts' - 1] in
           let next = if rows sol == 0 then init else concat $ toLists $ sol' ? [rows sol' - 1] in
           case findIndex (not . p . toList) (toRows sol) of
                Nothing -> sol === sample r next ts''
                Just n  -> if n == 0 then (0 >< length start) [] 
                                     else sol ? [0..n - 1] === sample r (concat $ toLists $ sol' ? [min n $ rows sol']) (Vec.drop n ts)

fallTraj y x = map (\x -> x - y^2) x 

xdot t [x,v] = [v, -9.8]
ts  = linspace 300 (0,81)
sol = odeSolve xdot [10,0] ts

test = [
    wait 1.0,
    wait 1.0 >> put [2.0] >> wait 1 >> put [3.0] >> wait 0.1,
    funTraj fallTraj (predBound $ all (>0.0)),
    test !! 2 >> put [2.0] >> test !! 2,
    odeTraj xdot $ predBound (\[x,v] -> x >0 || v >=0),
    do test !! 4; [x,v] <- get; put [x, -0.6 * v]; test !! 4; return [x]
 ]

-- A ball
ball = odeTraj xdot $ predBound (\[x,v] -> x >0 || v >=0)

-- A bouncing ball
bball = do ball; [x,v] <- get; put [x, -0.6 * v]; bball

renatos_test = do put [1.0]
                  odeTraj (\_ [x] -> [-x]) $ numBound 40
                  odeTraj (\_ [x] -> [x])  $ numBound 40
                  [x] <- get 
                  if x == 1 then put [2] else put [3]
                  
program = funTraj fallTraj (Bound (all (>0.0)) Nothing)
main = mplot $ ts : (toColumns $ sample bball [2,0] ts)


