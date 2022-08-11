# PITFALL: Keep measuring frequency at the 12th fret.

To make the original data,
I did not measure frequency at the nut,
but instead at the 12th fret, because that was easier.
It won't hurt the code at all,
but it could be confusing -- for instance,
the data says that I want to tune the first string to a vG,
but in fact it should be a vC at the nut.

More importantly,
I must keep doing that if I add more data to the table.
Otherwise the rows will be inconsistent,
and the interpolated suggestions will be wrong.

# Purpose

Given information about good* gauge-frequency string pairs,
this computes good string gauges for frequencies not in that data.

# The Python code is better.

It works with CSV data.

The Haskell code is more convenient for ad-hoc calculations,
but it doesn't handle tables.

# The data used

See [data/README](data/README.md).

# What is "middle C" in 41-edo

Since I'm interested in A-440,
C is defined in terms of that:
C = 440 * 2 ** (10/41)
