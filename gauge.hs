{-# LANGUAGE ScopedTypeVariables #-}

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
a = 110
c = 110 * 2**(10/41)

to_freq :: Float -- ^ 41-edo note value, mod 41
        -> Float -- ^ octave
        -> Float -- ^ frequency in Hz
to_freq note_mod_41 octave =
  c * 2 ** ( (note_mod_41 + octave*41) / 41 )

freqs_I_am_using =
  [ to_freq i j
  | (i,j) <- [ (23 , 1  )
             , (10 , 1  )
             , (38 , 0  )
             , (25 , 0  )
             , (12 , 0  )
             , (39 , 0  ) -- string 6, treble
             , (26 , -2 ) -- string 7, bass
             , (39 , -2 )
             , (11 , -1 )
             , (24 , -1 )
             , (37 , -1 )
             , ( 9 , 0  ) ] ]

freqs_ideal_for_these_gauges =
  [ to_freq i j
  | (i,j) <- [ (23,1 )
             , (10,1 )
             , (37,0 )
             , (25,0 )
             , (15,0 )
             , (4 ,0 ) -- string 6, treble
             , (25,-2) -- string 7, bass
             , (35,-2)
             , (3 , -1)
             , (20, -1)
             , (38, -1)
             , (3 , 0) ] ]

edo41 = [ ( div' i 41,
            mod' i 41,
            440 * 2 ** (i / 41) )
        | i <- [-4*41 .. 0] ]


-- * A utility

myPrint :: forall a t. (Foldable t, Show a)
  => t a -> IO ()
myPrint = mapM_ $ putStrLn . show
