"""
This reads a table and computes the gauges to try next.
Some of those computations won't work,
because this doesn't understand how to handle out-of-sample frequencies.
For those frequencies, just run interpolate_to_freq() by hand,
using the two rows from the data that are nearest to
(despite both being on the same side of) the desired frequency.
"""

import math as math
from   math import log, exp
import numpy as np
import pandas as pd


##################
### Input data ###
##################

strings = (
  pd.read_csv("data/input.csv")
  # . drop ( columns = ["i-note", "u-note"] ) # to fit on screen
)

wound   = ( strings[ strings["wound"] == True ]
            . sort_values( "i-Hz" ) )
unwound = ( strings[ strings["wound"] == False ]
            . sort_values( "i-Hz" ) )


######################
### Some Functions ###
######################

def proportion ( x    :  float,
                 low  :  float,
                 high :  float
                )     -> float:
  """How far is x from low to high.
  For instance, proportion 3 0 5 == 0.6"""
  return ( low if high - low < 1e-5 # PITFALL: 1e-5 as an approximate 0
                                    # is kind of a kludge.
           else (x - low) / (high - low) )

def log_proportion ( x    :  float,
                     low  :  float,
                     high :  float
                    )     -> float:
  return proportion ( x = log(x),
                      low = log(low),
                      high = log(high) )

def max_below ( freq :  float,
                df   :  pd.DataFrame
               )     -> pd.Series:
  "The string with the highest observed frequency below `freq`."
  df = ( df [ df["i-Hz"] <= freq ]
         . sort_values ( [ "i-Hz" ] ) )
  if len(df) > 0:
    return df.iloc[-1]
  else: return None

def min_above ( freq :  float,
                df   :  pd.DataFrame
               )     -> pd.Series:
  "The string with the least observed frequency above `freq`."
  df = ( df [ df["i-Hz"] >= freq ]
         . sort_values ( [ "i-Hz" ] ) )
  if len(df) > 0:
    return df.iloc[0]
  else: return None

def interpolate_to_freq (
    target :  float,  # a frequency
    f1     :  float,  # a frequency
    g1     :  float,  # a gauge
    f2     :  float,  # a frequency
    g2     :  float,  # a gauge
)          -> float : # a gauge
  """If (f1,g1) and (f2,g2) are two frequency-gauge pairs that correspond well -- i.e. a gi gauge string sounds and feels good at f1 -- then this returns the gauge corresponding to the target frequency. The target does *not* have to be between f1 and f2."""
  p = log_proportion ( target, f1, f2 )
  return ( (1 - p) * f1 * g1 + p * f2 * g2 ) / target

def ideal_gauge (
    freq : float,
    df : pd.DataFrame
) -> float:
  below = max_below ( freq, df )
  above = min_above ( freq, df )
  if (not (below is None)) & (not (above is None)):
    return interpolate_to_freq (
      freq,
      below["i-Hz"], below["gauge"],
      above["i-Hz"], above["gauge"] )
  else: return None

def ideal_gauge_from_somewhere (
    freq : float
) -> float:
  """First tries the wound strings. If the target freq is out of that sample, tries the unwound strings."""
  ret = ideal_gauge ( freq, wound )
  return ( ret
           if (not ret is None) & (not ret is np.nan)
           else ideal_gauge ( freq, unwound ) )


###################
### Output data ###
###################

strings["err"] = (
      strings["u-41"] + 41 * strings["u-oct"]
  - ( strings["i-41"] + 41 * strings["i-oct"] ) )

strings["try-gauge"] = round (
  strings["u-Hz"] . apply (
    ideal_gauge_from_somewhere ),
  2 )

strings.to_csv("data/output.csv",
               index = False )
