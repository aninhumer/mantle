
----------------------------------------------------------------------
-- |
-- Module  :  Data.Boolean.Overload
-- 
-- Definitions of Prelude function names in terms of their corresponding
-- Data.Boolean generalised implementation. This can then be used as part
-- of a partial or complete Prelude replacement.
--
-- Also exports ifThenElse for use with RebindableSyntax.

----------------------------------------------------------------------



module Data.Boolean.Overload
  ( module Data.Boolean,
    (&&), (||), not,
    ifThenElse,
    (==), (/=), 
    (<), (>), (<=), (>=),
    min, max
  ) where

import Data.Boolean
import Prelude hiding
  ( (&&), (||), not,
    (==), (/=), 
    (<), (>), (<=), (>=),
    min, max
  )

(&&) :: Boolean a => a -> a -> a
(&&) = (&&*)

(||) :: Boolean a => a -> a -> a
(||) = (||*)

not :: Boolean a => a -> a
not = notB


-- For use with RebindableSyntax
ifThenElse :: IfB a => BooleanOf a -> a -> a -> a
ifThenElse = ifB


(==) :: EqB a => a -> a -> BooleanOf a
(==) = (==*)
(/=) :: EqB a => a -> a -> BooleanOf a
(/=) = (/=*)


(<) :: OrdB a => a -> a -> BooleanOf a
(<) = (<*)
(>) :: OrdB a => a -> a -> BooleanOf a
(>) = (>*)
(<=) :: OrdB a => a -> a -> BooleanOf a
(<=) = (<=*)
(>=) :: OrdB a => a -> a -> BooleanOf a
(>=) = (>=*)

min :: (IfB a, OrdB a) => a -> a -> a
min = minB
max :: (IfB a, OrdB a) => a -> a -> a
max = maxB


