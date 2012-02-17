module DebugUtil
       (
         (.$.),
         (...),
         trace
       )where

import Debug.Trace

infixr 0 .$.
(.$.) :: Show a => (a -> b) -> a -> b
f .$. x = trace (show x) f x

infixr 9 ...
(...) :: Show b => (b -> c) -> (a -> b) -> (a -> c)
f ... g = (f .$.) . g 
