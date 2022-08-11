# PITFALL: Keep measuring frequency the same way.

For instance, in my case, to make the original data,
I did not measure frequency at the nut,
but instead at the 12th fret,
because on a Chapman Stick that's what's easiest.
It won't hurt the code at all,
but it could be confusing -- for instance,
the data says that I want to tune the first string to a vG,
but in fact it should be a vC at the nut,
because in 20.5-edo vG is 12 steps above vC.

I have to keep measuring frequency the same way
if I add more data to the table later.
Otherwise the rows will be inconsistent,
and the interpolated suggestions will be wrong.

# Purpose

Given information about good* gauge-frequency string pairs,
this computes good string gauges for frequencies not in that data.

# How to use this

Run the Python code.

It works with CSV data. If there's a file called `data/input.csv`,
and its columns are as described in [data/README](data/README.md),
then running the Python code from the root of the project
will create a new file, `data/output.csv`,
with a column called `try-gauge` which is what you want.
I've included sample data,
based on my own observations of a Kite (20.5-edo) Chapman Stick.

You can be using any tuning; it doesn't have to be a Kite guitar.
The program only deals in Hz values, not note values.
(The data can include note values,
but they are just for human consumption.)

Note that the output data will be incomplete
if you ask for out-of-sample values.
For instance, if your data gives frequency-gauge pairs
for frequencies running from 100 Hz to 300 Hz,
and you ask the program what gauge fits well with 50 Hz,
it won't know. For these cases,
as described in the header comment to `gauge.py`,
just run `interpolate_to_freq()` by hand in the interpreter,
inputting the two gauge-frequency pairs closeset to the target frequency,
even if they both lie to one side of it.

The Haskell code is more convenient for ad-hoc calculations,
but it doesn't handle tables.

# The data needed and produced

See [data/README](data/README.md).

# The key insight

The interpolation algorithm is based on the fact that,
at least in the guitars I've used,
frequency * gauge is roughly constant.
Given that fact, if you know that gauge `g1`
sounds and/or feels good at frequency `f1`,
and gauge `g2` sounds and/or feels good at frequency `f2`,
and `f` is between `f1` and `f2`,
you can compute the `g` corresponding to `f`.
That's what the (very short) function
`interpolate_to_freq()` in `gauge.py` does.

# What is "middle C" in 41-edo

Since I'm interested in A-440,
C is defined in terms of that:
`C = 440 * 2 ** (10/41) == 521.04 Hz`.

If I were in 12-edo, it would be a hair sharper:
`C = 440 * 2 ** (3/12) = 523.25 Hz`.
