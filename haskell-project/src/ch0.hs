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