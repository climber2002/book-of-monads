lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = do a <- x
                 b <- y
                 return (f a b)

-- Exercise 3.1 
ap :: Monad m => m (b -> c) -> m b -> m c
ap mf mb = do f <- mf
              b <- mb
              return (f b)

-- Exercise 3.4
convertTriples :: (a, b, c) -> (a, (b, c))
convertTriples (a, b, c) = (a, (b, c))