# icfp-2016
ICFP Programming Contest submission for Invisible Imp

As usual, I'm competing alone, mainly because I don't have a set of people that I'm good at working with who share my enthusiasm for programming contests.

I made a bunch of errors along the way to this solution, including spending wasted time on:

(a) reflections (to simulate folding the paper)
(b) trying to find a skeletal path from one corner to another before mapping facets
(c) representing the solution data structure poorly
(d) polygon triangulation
(e) building crappy overlap detectors before just looking up a good one

The main good thing about this solution is that I have a semi-tested set of data structures that work pretty darn well, particularly my arbitrary-precision rational number class.  Scala helps a lot here, since my Ratio class has all the necessary arithmetic operators defined on it so I write code which looks like it's working with plain numbers, and Scala's Integral and Ordered traits do a lot of the heavy lifting.  Operator overloading used appropriately is beautiful.

I took pains not to use trigonometry (hard to do exactly in arbitrary precision), so my coordinate transforms are, well, an ugly mass of algebra based on the intersection of circles.  I probably could have done better with matrix-based transforms, if I had bothered to look up how to generate and apply them.  The only good part of that is that I prune out transforms leading to irrational results fairly quickly.

Strategy-wise, I think I did OK.  Aside from the errors in that I was trying to build (mentioned above), I had a pretty good infrastructure sequence.  First, I built some stuff to fetch and cache the problems without exhausting my request limits.  Then I built structures just to read and write the problem and solution files.  After that, I made a painfully stupid "solver" that just moved (without rotation) the unfolded paper to cover the centroid of the target shape.  That (plus automated execution and submission) got me easily into the top decile.  After I got the transforms working, I made another "solver" that aligned the unfolded paper with a detected 90-degree corner in the skeleton (including rotation), and that briefly propelled me into 13th place (of about 277 contestants at that point).

Unfortunately, I then meandered for days (literally) in my dead-end code, before finally just tiling the source paper with facets corresponding to the target shape.  I got that working after the leaderboard froze, so I'm not really sure where I'm going to end up.  Certainly in the top quartile (I was there even without the tiling solution, as the leaderboard froze), possibly in the top decile.

A team would certainly have helped; one person to work on the automation, one to work on problem solving, and one to work on crafting nasty problem specs to torture other people with.  (All of the problems that I posted were mechanical manipulations of a hand-crafted O-ring.)

Ah well.  It's now 2 hours before the end of the contest, and I'm mostly just cleaning up shop.
