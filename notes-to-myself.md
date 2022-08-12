# PITFALL: Keep measuring frequency the same way.

For instance, in my case, to make the original data,
I did not measure frequency at the nut,
but instead at the 12th fret,
because on a Chapman Stick that's what's easiest.
It won't impair the code at all,
but it could be confusing -- for instance,
the data says that I want to tune the first string to a vG,
but in fact it should be a vC at the nut,
because in 20.5-edo vG is 12 steps above vC.

I have to keep measuring frequency the same way
if I add more data to the table later.
Otherwise the rows will be inconsistent,
and the interpolated suggestions will be wrong.
