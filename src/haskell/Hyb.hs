module Monads where
import Data.Monoid

class (Monad m) => MonadHybrid m where
    trajectory :: Double -> (Double -> a) -> m a
    start      :: m a -> a

wait :: (MonadHybrid m) => Double -> a -> m a
wait d a = trajectory d (const a)

data SymTraj  a = OdeInit a (Double -> a -> a) | Traj (Double -> a)
data SymBound a = OdeInv (Double -> a) | Dur Double

-- This is a monad only if the total duration is non-zero
data HybSym a = HybSym [(SymTraj a, SymBound a)] 

instance Functor (HybSym) where
    fmap f p = p >>= return . f

instance Applicative (HybSym) where
    pure = return
    f <*> p = f >>= \g-> p >>= return . g

instance Monad (HybSym) where
  return x = HybSym [(Traj undefined, Dur 0)]
   
  f >>= g = undefined 
     -- here, we've got a problem, because we do not know,
     -- what the last point of the input trajectory is
  

instance MonadHybrid (HybSym) where
  trajectory d f = HybSym [(Traj f, Dur d)]
  
  start (HybSym ((Traj f, _) : _))       = f 0 
  start (HybSym ((OdeInit x _ , _) : _)) = x
  start _ = undefined

