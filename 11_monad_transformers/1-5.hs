{-# LANGUAGE UndecidableInstances #-}
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.State

-- task 1

newtype StrRdrT m a = StrRdrT { runStrRdrT :: String -> m a }

instance Functor m => Functor (StrRdrT m) where
    fmap :: Functor m => (a -> b) -> StrRdrT m a -> StrRdrT m b
    fmap f rd = StrRdrT $ \s -> fmap f (runStrRdrT rd s)

instance Applicative m => Applicative (StrRdrT m) where
    pure :: a -> StrRdrT m a
    pure = StrRdrT . const . pure

    (<*>) :: Applicative m => StrRdrT m (a -> b) -> StrRdrT m a -> StrRdrT m b
    fr <*> r = StrRdrT $ \s -> runStrRdrT fr s <*> runStrRdrT r s

instance Monad m => Monad (StrRdrT m) where
    return :: a -> StrRdrT m a
    return = pure

    (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
    rd >>= k = StrRdrT $ \s -> do
        a <- runStrRdrT rd s
        runStrRdrT (k a) s

instance MonadFail m => MonadFail (StrRdrT m)  where
    fail :: String -> StrRdrT m a
    fail = StrRdrT . const . fail

-- task 2

askStrRdr :: Monad m => StrRdrT m String
askStrRdr = asksStrRdr id

asksStrRdr :: Monad m => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT $ \s -> return (f s)

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr rd s = runIdentity $ runStrRdrT rd s

-- task 3

instance MonadTrans StrRdrT where
    lift :: Monad m => m a -> StrRdrT m a
    lift = StrRdrT . const

-- task 4

instance MonadState s m => MonadState s (StrRdrT m) where
    get :: MonadState s m => StrRdrT m s
    get = lift get
    put :: MonadState s m => s -> StrRdrT m ()
    put = lift . put
    state :: MonadState s m => (s -> (a, s)) -> StrRdrT m a
    state = lift . state

-- task 5

class Monad m => MonadStrRdr m where
    askSR :: m String
    asksSR :: (String -> a) -> m a
    strRdr :: (String -> a) -> m a

instance Monad m => MonadStrRdr (StrRdrT m) where
    askSR :: StrRdrT m String
    askSR = asksSR id
    asksSR :: (String -> a) -> StrRdrT m a
    asksSR f = StrRdrT $ \s -> return (f s)
    strRdr :: (String -> a) -> StrRdrT m a
    strRdr = asksSR

instance MonadStrRdr m => MonadStrRdr (StateT s m) where
    askSR :: StateT s m String
    askSR = lift askSR
    asksSR :: (String -> a) -> StateT s m a
    asksSR = lift . asksSR
    strRdr :: (String -> a) -> StateT s m a
    strRdr = lift . strRdr
