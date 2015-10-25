{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Monoid ((<>), Monoid(..))

import Prelude hiding (getChar, putChar)

import qualified Prelude as P (getChar, putChar)
import qualified Data.DList as DL

import Control.Monad (when, join)

newtype DList a =
  DList { runDList :: [a] -> [a] }

data Free f a = Free (f (Free f a))
              | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
  pure = Pure

  Pure f <*> x = fmap f x
  Free f <*> x = Free (fmap (<*> x) f)

instance Functor f => Monad (Free f) where
  return = pure

  Pure x >>= f = f x
  Free x >>= f = Free (fmap (>>= f) x)

liftF :: MonadFree f m => f a -> m a
liftF = wrap . fmap pure

class (Functor f, Monad m) => MonadFree f m | m -> f where
  wrap :: f (m a) -> m a

instance Functor f => MonadFree f (Free f) where
  wrap = Free

newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b }

instance Functor (Codensity f) where
  fmap f (Codensity c) = Codensity (\k -> c (k . f))

instance Applicative (Codensity f) where
  pure x = Codensity (\k -> k x)

  Codensity f <*> Codensity x = Codensity (\k -> f (x . (k .)))

instance Monad (Codensity f) where
  Codensity x >>= f = Codensity (\k -> x (flip runCodensity k . f))

instance MonadFree f m => MonadFree f (Codensity m) where
  wrap x = Codensity (\k -> wrap (fmap (flip runCodensity k) x))

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity (Codensity c) = c return

type List a = Codensity (Free ((,) a)) ()

empty :: List a
empty = pure ()

singleton :: a -> List a
singleton x = liftF (x, ())

append :: List a -> List a -> List a
append xs ys = xs >> ys

exec :: Free ((,) a) () -> [a]
exec (Pure _)       = []
exec (Free (x, xs)) = x : exec xs

data TeletypeF k = PutChar Char k
                 | GetChar (Char -> k)
                 deriving Functor

type Teletype a = forall m. MonadFree TeletypeF m => m a

getChar :: Teletype Char
getChar = liftF (GetChar id)

putChar :: Char -> Teletype ()
putChar c = liftF (PutChar c ())

revEcho :: Teletype ()
revEcho = do
  c <- getChar
  when (c /= ' ') $ do
    revEcho
    putChar c

improve :: Functor f => (forall m. MonadFree f m => m a) -> Free f a
improve = lowerCodensity

runIO :: Free TeletypeF a -> IO a
runIO (Pure x) = return x
runIO (Free (GetChar k)) = P.getChar >>= runIO . k
runIO (Free (PutChar c k)) = P.putChar c >> runIO k

data Output a = Read (Output a) | Print Char (Output a) | Finish a
  deriving (Show, Eq)

input = replicate 10000 'c' ++ ' ' : repeat 'c'

runPure :: Free TeletypeF a -> [Char] -> Output a
runPure (Pure x)             _      = Finish x
runPure (Free (GetChar k))   (i:is) = Read (runPure (k i) is)
runPure (Free (PutChar c k)) (i:is) = Print c (runPure k is)

type Log     = List String
type FastLog = DL.DList String

prog1 :: String
prog1 = last . exec . lowerCodensity $ prog' empty 20000
  where
    prog' :: Log -> Int -> Log
    prog' log 0 = log
    prog' log n = prog' (log `append` singleton (show n)) (n - 1)

prog2 :: String
prog2 = last (DL.toList (prog' [] 20000))
  where
    prog' :: FastLog -> Int -> FastLog
    prog' log 0 = log
    prog' log n = prog' (log <> [show n]) (n - 1)
