# Input data columns needed

## gauge : float = gauge

## wound : bool = whether the string is wound or not

## i-Hz : float = ideal frequency in Hz

The frequency that that gauge seems to sound and/or feel best at.

## u-Hz : float = the frequency I'm using (tuning to), in Hz

# Output data columns

## try-gauge : float = the interpolated best gauge for u-Hz

This is what I should buy next.

# Ignored data columns

These can be helpful to a human,
but the code ignores them.

## s : int = which string we're talking about

## i-note = ideal note

The note name of i-Hz.

## i-41 : float = 41-edo pitch

How many steps of 41-edo above C i-Hz is (mod 41).
Trailing apostrophes and underscores indicate octave.

## u-note = note I'm using (tuning to)

The note name of u-Hz.

## u-41 : float = 41-edo pitch I'm using (tuning to)

How many steps of 41-edo above C u-Hz is (mod 41).
Trailing apostrophes and underscores indicate octave.
