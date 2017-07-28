{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, MultiParamTypeClasses,
             TypeSynonymInstances, ConstraintKinds, ViewPatterns,
             FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             StandaloneDeriving, TupleSections, ScopedTypeVariables #-}

module ProbDist(
    FMap(..), Ap(..), Pure(..), Bind(..), Fail(..),
    (<$>), return, sequence, sequence_, replicateM,
    Prob(..), Out(..), Dist, runDist, dist, nullDist, measureDist, splitDist,
    ApproxDist, runApproxDist, approxDist, approxLift, showApproxDist,
    Ratio', Ratio'', Rational', Rational'', Float', uniform, binomial, showDist,
    module Prelude
) where

import Prelude hiding (
    fmap, (<$>), (<$), pure, (<*>), (*>), (<*),
    return, (>>=), (>>), fail, sequence, sequence_)
import qualified Prelude as P

import qualified Control.Monad as Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.List

import qualified Data.Map.Strict as M
import Data.Function
import Data.List
import Data.Ratio
import Data.Either

import Numeric

--------------------------------------------------------------------------------
class FMap f a b where
    fmap :: (a -> b) -> f a -> f b

    (<$) :: b -> f a -> f b
    x <$ fy = fmap (const x) fy

class Pure f a where
    pure :: a -> f a

class Ap f a b where
    (<*>) :: f (a -> b) -> f a -> f b
    ( *>) :: f a -> f b -> f b
    (<* ) :: f a -> f b -> f a

class Bind m a b where
    infixl 1 >>=, >>
    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    mx >> my = mx >>= const my

class Fail m a where
    fail :: String -> m a
    fail = error

(<$>) :: FMap f a b => (a -> b) -> f a -> f b
(<$>) = fmap

return :: Pure m a => a -> m a
return = pure

sequence :: (Pure m [a], Bind m a [a], Bind m [a] [a]) => [m a] -> m [a]
sequence []     = return []
sequence (m:ms) = do x <- m; xs <- sequence ms; return (x:xs)

sequence_ :: (Pure m (), Bind m a ()) => [m a] -> m ()
sequence_ []     = return ()
sequence_ (m:ms) = m >> sequence_ ms

replicateM :: (Pure m [a], Bind m a [a], Bind m [a] [a]) => Int -> m a -> m [a]
replicateM n mx = sequence (replicate n mx)

--------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} Functor f => FMap f a b where
    fmap = P.fmap
    (<$) = (P.<$)

instance {-# OVERLAPPABLE #-} Applicative f => Pure f a where
    pure = P.pure

instance {-# OVERLAPPABLE #-} Applicative f => Ap f a b where
    (<*>) = (P.<*>)
    (<*) = (P.<*)
    (*>) = (P.*>)

instance {-# OVERLAPPABLE #-} Monad m => Bind m a b where
    (>>=) = (P.>>=)
    (>>) = (P.>>)

instance {-# OVERLAPPABLE #-} Monad m => Fail m a where
    fail = P.fail

--------------------------------------------------------------------------------
type Prob p = (Num p, Fractional p, Ord p)
type Out o = (Eq o, Ord o)
newtype Dist p o = Dist{ runDist :: [(o, p)] }

dist :: (Prob p, Out o) => [(o, p)] -> Dist p o
dist = Dist . M.toList . M.fromListWith (+)

splitDist :: (Prob p, Out l, Out r)
          => Dist p o -> (o -> Either l r) -> (Dist p l, Dist p r)
splitDist (Dist os) f
  = (dist ls, dist rs)
  where
    (ls, rs) = partitionEithers $ do
        (o, p) <- os
        return $ either (Left.(,p)) (Right.(,p)) (f o)

nullDist :: Dist p o -> Bool
nullDist = null . runDist

measureDist :: Prob p => Dist p o -> p
measureDist = sum . map snd . runDist

instance (Prob p, Out o') => FMap (Dist p) o o' where
    fmap f (Dist os) = dist [(f o, p) | (o, p) <- os]
    x <$ _ = pure x

instance (Prob p, Out o) => Pure (Dist p) o where
    pure x = Dist [(x, 1)]

instance (Prob p, Out o') => Bind (Dist p) o o' where
    Dist os >>= f = do
        dist [(o, p*q) | (f -> Dist os', p) <- os, (o, q) <- os']
    _ >> d = d

instance (Prob p, Show p, Show o) => Show (Dist p o) where
    show (Dist os) = "dist " ++ showDist os

instance (Prob p, Out o) => Monoid (Dist p o) where
    mempty                       = Dist []
    mappend (Dist os) (Dist os') = dist $ os ++ os'
    mconcat ds                   = dist $ concatMap runDist ds

--------------------------------------------------------------------------------
newtype ApproxDist p o
  = ApproxDist{ runApproxDist :: p -> Dist p o }

approxDist :: (Prob p, Out o) => [(o, p)] -> ApproxDist p o
approxDist os = ApproxDist $ \t ->
    Dist . filter ((> t) . snd) . runDist $ dist os

approxLift :: Dist p o -> ApproxDist p o
approxLift = ApproxDist . const

showApproxDist :: (Prob p, Out o, Show p, Show o)
               => ApproxDist p o -> p -> String
showApproxDist d = showDist . runDist . runApproxDist d

instance (Prob p, Out o') => FMap (ApproxDist p) o o' where
    fmap f d = ApproxDist $ do
        Dist os <- runApproxDist d
        runApproxDist $ approxDist [(f o, p) | (o, p) <- os] 
    x <$ _ = pure x

instance (Prob p, Out o) => Pure (ApproxDist p) o where
    pure x = ApproxDist $ const (pure x)

instance (Prob p, Out o') => Bind (ApproxDist p) o o' where
    d >>= f = ApproxDist $ \t ->
        flip (runApproxDist . approxDist) t $ do
            (o,  p) <- runDist $ runApproxDist d t
            (o', q) <- runDist $ runApproxDist (f o) (t/p)
            return (o', p*q)
    _ >> d = d

instance (Prob p, Out o) => Monoid (ApproxDist p o) where
    mempty       = ApproxDist $ const mempty
    mappend d d' = ApproxDist $ mappend <$> runApproxDist d <*> runApproxDist d'
    mconcat ds   = ApproxDist $ mconcat <$> sequence (map runApproxDist ds)

--------------------------------------------------------------------------------
uniform :: (Prob p, Out o) => [o] -> Dist p o
uniform os = dist $ map (flip (,) (1 / genericLength os)) os

-- Pre: 0 < p < 1
binomial :: (Prob p, Out n, Integral n) => n -> p -> Dist p n
binomial n p
  = Dist $ binomial' 0 (q^n)
  where
    q = 1-p
    binomial' k r | k <= n
      = (k, r) : binomial' (k + 1) r'
      where r' = (r * p * fromIntegral (n-k)) / (q * fromIntegral (k+1))
    binomial' _ _ = []

--------------------------------------------------------------------------------
newtype Ratio' i
  = Ratio' (Ratio i) deriving (Eq, Ord, Enum, Num, Fractional, Real, RealFrac)

newtype Ratio'' i
  = Ratio'' (Ratio i) deriving (Eq, Ord, Enum, Num, Fractional, Real, RealFrac)

newtype Float'
  = Float' Float
  deriving (Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFloat, RealFrac)

type Rational'  = Ratio'  Integer
type Rational'' = Ratio'' Integer

instance (Eq i, Integral i, Show i) => Show (Ratio' i) where
    show (Ratio' p)
      | d == 1    = show n
      | otherwise = show n ++ "/" ++ show d ++ "=" ++ show (Ratio'' p)
      where (n, d) = (numerator p, denominator p)

instance (Eq i, Integral i, Show i) => Show (Ratio'' i) where
    show (Ratio'' p)
      = show (s * q) ++ "." ++ padLeft 2 '0' (show $ abs r) ++ "%"
      where
        s = floor (signum p) :: Integer
        (q, r) = floor (abs p * 10000) `quotRem` 100 :: (Integer, Integer)

instance Show Float' where
    show (Float' x)
      = showFFloat (Just 2) (100 * x) "%"

--------------------------------------------------------------------------------
showDist :: (Prob p, Show p, Show o) => [(o, p)] -> String
showDist os 
      = "[" ++ intercalate ", " (map show' os) ++ "]" ++ remS
      where
        show' (o, p) = "(" ++ show o ++ ", " ++ show p ++ ")"
        remS | rem /= 0  = " {- r=" ++ show rem ++ " -" ++ "}"
             | otherwise = ""
             where rem = 1 - sum (map snd os)

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs
  | l < n     = replicate (n-l) x ++ xs
  | otherwise = xs
  where l = length (take n xs)
