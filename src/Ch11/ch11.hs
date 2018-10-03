import Control.Monad
import Control.Applicative


sequenceB1 :: (Applicative f) => [f a] -> f [a]
sequenceB1 [] = pure []
sequenceB1 (x:xs) =
   (:) <$> x <*> sequenceB1 xs

sequenceB2 xs =
  foldr (\x acc -> (:) <$> x <*> acc) (pure []) xs
