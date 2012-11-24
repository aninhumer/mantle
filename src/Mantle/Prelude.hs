
module Mantle.Prelude (
    module Prelude,
    module Data.Boolean.Overload
) where

import Prelude hiding (
    (&&), (||), not,
    (==), (/=),
    (<), (>), (<=), (>=),
    min, max,
    (/) )

import Data.Boolean.Overload
