module ListyInstances where

import Data.Monoid
import Listy

-- instance Semigroup (Listy a) where     -- gave error since its already declared in Listy.hs
--     (<>) (Listy l) (Listy l') =
--       Listy $ mappend l l'
instance Monoid (Listy a) where
    mempty = Listy []