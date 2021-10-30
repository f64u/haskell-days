module OrderlessList where
import           Counter                        ( counterize )


-- | A list that doesn't care about the order of its elements in comparison.
--   Aka a MultiSet (without the bells and whistles).
newtype OrderlessList a = OrderlessList { getOList :: [a] }

instance Foldable OrderlessList where
  foldr f a lst = foldr f a (getOList lst)

instance (Show a) => Show (OrderlessList a) where
  show = ("OrderlessList " ++) . show . getOList

instance Ord a => Eq (OrderlessList a) where
  lst1 == lst2 = counterize lst1 == counterize lst2
instance Ord a => Ord (OrderlessList a) where
  lst1 `compare` lst2 = counterize lst1 `compare` counterize lst2
