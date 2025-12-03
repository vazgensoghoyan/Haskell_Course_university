{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
import Control.Monad.Identity
import Control.Applicative (liftA2)
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Reader

-- task 1

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Functor m => Functor (LoggT m) where
    fmap :: Functor m => (a -> b) -> LoggT m a -> LoggT m b
    fmap f = 
        let mapLogged g (Logged s x) = Logged s (g x)
        in LoggT . fmap (mapLogged f) . runLoggT

instance Applicative m => Applicative (LoggT m) where
    pure :: Applicative m => a -> LoggT m a
    pure x = LoggT $ pure (Logged "" x)

    (<*>) :: Applicative m => LoggT m (a -> b) -> LoggT m a -> LoggT m b
    LoggT mf <*> LoggT mx = LoggT $ liftA2 app mf mx
        where
            app (Logged s1 f) (Logged s2 x) =
                Logged (s1 ++ s2) (f x)

instance Monad m => Monad (LoggT m) where
    return :: Monad m => a -> LoggT m a
    return = pure

    (>>=) :: Monad m => LoggT m a -> (a -> LoggT m b) -> LoggT m b
    m >>= k  = LoggT $ do
        (Logged s x) <- runLoggT m
        (Logged s' x') <- runLoggT $ k x
        return $ Logged (s' ++ s) x'

instance MonadFail m => MonadFail (LoggT m) where
    fail :: MonadFail m => String -> LoggT m a
    fail = LoggT . fail

-- task 2

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT . return $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg l = runIdentity $ runLoggT l

-- task 3

instance MonadTrans LoggT where
    lift :: Monad m => m a -> LoggT m a
    lift = LoggT . fmap (Logged "")

-- task 4

instance MonadState s m => MonadState s (LoggT m) where
    get :: MonadState s m => LoggT m s
    get = lift get

    put :: MonadState s m => s -> LoggT m ()
    put = lift . put

    state :: MonadState s m => (s -> (a, s)) -> LoggT m a
    state = lift . state

-- task 5

instance MonadReader r m => MonadReader r (LoggT m) where
    ask :: MonadReader r m => LoggT m r
    ask = lift ask

    local :: MonadReader r m => (r -> r) -> LoggT m a -> LoggT m a
    local f lg = LoggT $ local f (runLoggT lg)

    reader :: MonadReader r m => (r -> a) -> LoggT m a
    reader = lift . reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f lg = LoggT $ f (runLoggT lg)

-- task 6

class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
    w2log :: Monad m => String -> LoggT m ()
    w2log s = LoggT . return $ Logged s ()

    logg :: Monad m => Logged a -> LoggT m a
    logg  = LoggT . return

instance MonadLogg m => MonadLogg (StateT s m) where
    w2log :: MonadLogg m => String -> StateT s m ()
    w2log = lift . w2log

    logg :: MonadLogg m => Logged a -> StateT s m a
    logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
    w2log :: MonadLogg m => String -> ReaderT r m ()
    w2log = lift . w2log

    logg :: MonadLogg m => Logged a -> ReaderT r m a
    logg  = lift . logg
