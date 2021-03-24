> {-# LANGUAGE
>     TypeFamilies
>   , InstanceSigs
>   , TypeOperators
> #-}
> 
> module Data.Format.Pdf.With (
>     (:>)(..)
>   , makeWith
>   , mutateWith
> ) where



> import           Control.Monad.Fail
> import           Control.Monad.IO.Class
> import           Data.IORef
> 
> import Data.Format.Pdf.Monad





Supporting DSLs with "With"
================================================================================

The `PdfM` monad handles the details of producing a /lexically/ valid PDF file.


> newtype (s :> m) a = With
>   { unWith :: IORef s -> m a }
> 
> infixr 7 :>
> 
> instance (Monad m) => Functor (s :> m) where
>   fmap f (With x) = With $
>     \ref -> do
>       a <- x ref
>       return (f $! a)
>   {-# INLINE fmap #-}
> 
> instance (Monad m) => Applicative (s :> m) where
>   pure a = With $ \_ -> return a
>   {-# INLINE pure #-}
> 
>   (With f) <*> (With x) = With $
>     \ref -> do
>       f' <- f ref
>       x' <- x ref
>       return (f' $! x')
>   {-# INLINE (<*>) #-}
> 
> instance (Monad m) => Monad (s :> m) where
>   return = pure
>   {-# INLINE return #-}
> 
>   (With x) >>= f = With $
>     \ref -> do
>       a <- x ref
>       unWith (f a) ref
>   {-# INLINE (>>=) #-}
> 
> instance (MonadIO m) => MonadIO (s :> m) where
>   liftIO x = With $ \_ -> liftIO x
>   {-# INLINE liftIO #-}
> 
> instance (MonadFail m) => MonadFail (s :> m) where
>   fail msg = With $ \_ -> fail msg
>   {-# INLINE fail #-}



> instance
>   ( IsPdfM m
>   ) => IsPdfM (s :> m)
>   where
>     type Emits (s :> m) = Emits m
> 
>     getsPdfSt
>       :: (PdfSt (Emits m) -> a) -> (s :> m) a
>     getsPdfSt f = With $
>       \_ -> getsPdfSt f
> 
>     mutatesPdfSt
>       :: (PdfSt (Emits m) -> PdfSt (Emits m)) -> (s :> m) ()
>     mutatesPdfSt f = With $
>       \_ -> mutatesPdfSt f




> makeWith
>   :: ( MonadIO m )
>   => s -> (s :> m) () -> (s -> m a) -> m a
> makeWith s1 (With doAction) make = do
>   ref <- liftIO (newIORef s1)
>   doAction ref
>   liftIO (readIORef ref) >>= make

> mutateWith
>   :: ( MonadIO m )
>   => (s -> s) -> (s :> m) ()
> mutateWith f = With $ \ref ->
>   liftIO (modifyIORef' ref f)
