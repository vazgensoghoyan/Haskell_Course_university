import Control.Monad.Identity
import Control.Monad.RWS

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

