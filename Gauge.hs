{-# LANGUAGE ScopedTypeVariables #-}

module Gauge where

import Data.Fixed


-- * The interpolation algorithm

-- | How far is x from low to high.
-- proportion 3 0 5 == 0.6
proportion x low high =
  (x - low) / (high - low)

-- | What gauge corresponds to `f`,
-- where f1 and g1 are a frequency and gauge near but lower than f,
-- and f2 and g2 are a frequency and gauge near but higher than f.
-- (Actually, f can be out of bounds too.
-- In fact, if it's out of the sample's range, it has to be.)
gauge f f1 g1 f2 g2 =
  let p = proportion f f1 f2
  in ( (1 - p) * f1 * g1 + p * f2 * g2 ) / f


-- * Some frequencies

-- Defined in terms of 2 octaves down from A=440
a = 27.5           -- Equal to A in 12-edo
c = a * 2**(10/41) -- PITFALL: Differs from C in 12-edo.

to_freq :: Float -- ^ 41-edo note value, mod 41, where C = 0
        -> Float -- ^ octave
        -> Float -- ^ frequency in Hz
to_freq note_mod_41 octave =
  c * 2 ** ( (note_mod_41 + octave*41) / 41 )


-- ** to print the frequencies of a bunch of (mod41, octave) pairs

myPrint :: forall a t. (Foldable t, Show a)
  => t a -> IO ()
myPrint = mapM_ $ putStrLn . show

-- ^ Each element of `tuning` is a (string, (note mod 41, octave)).
-- Note that the string labels do not need to be unique --
-- for instance, I've tried two gauges for string 6, and both appear here.
tuning = [  ( 1, (23,3) )
         ,  ( 2, (10,3) )
         ,  ( 3, (38,2) )
         ,  ( 4, (25,2) )
         ,  ( 5, (12,2) )
         ,  ( 6, (40,1) )
         ,  ( 6, (40,1) )
         ,  ( 7, (30,0) )
         ,  ( 8, ( 2,1) )
         ,  ( 9, (15,1) )
         ,  ( 9, (15,1) )
         ,  (10, (28,1) )
         ,  (11, ( 0,2) )
         ,  (12, (13,2) )
         ]

y = [ (s, fromIntegral (floor $ 10 * to_freq m d) / 10)
    | (s, (m,d)) <- tuning ]
