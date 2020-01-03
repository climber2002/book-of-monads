{- LANGUAGE MultiParamTypeClasses -}
{- LANGUAGE FunctionalDependencies -}
{- LANGUAGE TypeSynonymInstances -}
{- LANGUAGE FlexibleContexts -}
{- LANGUAGE FlexibleInstances -}
{- LANGUAGE InstanceSigs -}
{- LANGUAGE UndecidableInstances -}
{- LANGUAGE GADTs -}
{- LANGUAGE RankNTypes -}
{- LANGUAGE PolyKinds -}
{- LANGUAGE ScopedTypeVariables -}
{- LANGUAGE DataKinds -}
{- LANGUAGE KindSignatures -}
{- LANGUAGE TypeApplications -}
{- LANGUAGE LambdaCase -}
{- LANGUAGE TypeOperators -}
{- LANGUAGE MonadComprehensions -}
{- LANGUAGE ApplicativeDo -}
{- LANGUAGE DeriveFunctor -}
{- LANGUAGE GeneralizedNewtypeDeriving -}
{- LANGUAGE TypeInType -}
{- LANGUAGE ConstraintKinds -}

class Eq' a where
  (===) :: a -> a -> Bool

eqList :: Eq' a => [a] -> [a] -> Bool
eqList [] [] = True
eqList (x : xs) (y : ys) = x === y && (eqList xs ys)
eqList _ _ = False

instance Eq' Bool where
  True === True = True
  False === False = True
  _ === _ = False

instance Eq' a => Eq' [a] where
  (===) = eqList

-- Excercise 0.2
instance (Eq' a, Eq' b) => Eq' (a, b) where
  (a1, b1) === (a2, b2) = a1 === a2 && b1 === b2

class Container c where
  empty :: c a
  insert :: a -> c a -> c a

instance Container [] where
  empty      = []
  insert x xs = x : xs

newtype Queue a = Queue { unQueue :: [a] }

instance Container Queue where
  empty = Queue []
  insert x (Queue xs) = Queue (xs ++ [x])