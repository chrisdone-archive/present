{-# LANGUAGE AutoDeriveTypeable #-}

-- | Minimial inclusion of transformer's StateT library.
--
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001

module Control.Monad.Trans.State.Strict (
    -- * The State monad
    State,
    state,
    runState,
    evalState,
    execState,
    mapState,
    withState,
    -- * The StateT monad transformer
    StateT(..),
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    -- * State operations
    get,
    put,
    modify,
    modify',
    gets
  ) where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Control.Monad.Fix

-- ---------------------------------------------------------------------------
-- | A state monad parameterized by the type @s@ of the state to carry.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
type State s = StateT s Identity

-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state :: (Monad m)
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state f = StateT (return . f)

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: State s a   -- ^state-passing computation to execute
         -> s           -- ^initial state
         -> (a, s)      -- ^return value and final state
runState m = runIdentity . runStateT m

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalState' m s = 'fst' ('runState' m s)@
evalState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> a          -- ^return value of the state computation
evalState m s = fst (runState m s)

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execState' m s = 'snd' ('runState' m s)@
execState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> s          -- ^final state
execState m s = snd (runState m s)

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runState' ('mapState' f m) = f . 'runState' m@
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

-- | @'withState' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withState' f m = 'modify' f >> m@
withState :: (s -> s) -> State s a -> State s a
withState = withStateT

-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')
    {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
    empty = StateT $ \ _ -> mzero
    StateT m <|> StateT n = StateT $ \ s -> m s `mplus` n s

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
    m >>= k  = StateT $ \ s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \ _ -> fail str

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero       = StateT $ \ _ -> mzero
    m `mplus` n = StateT $ \ s -> runStateT m s `mplus` runStateT n s

instance (MonadFix m) => MonadFix (StateT s m) where
    mfix f = StateT $ \ s -> mfix $ \ ~(a, _) -> runStateT (f a) s

-- | Fetch the current value of the state within the monad.
get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)

-- | @'put' s@ sets the state within the monad to @s@.
put :: (Monad m) => s -> StateT s m ()
put s = state $ \ _ -> ((), s)

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- * @'modify'' f = 'get' >>= (('$!') 'put' . f)@
modify' :: (Monad m) => (s -> s) -> StateT s m ()
modify' f = do
    s <- get
    put $! f s

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = state $ \ s -> (f s, s)
