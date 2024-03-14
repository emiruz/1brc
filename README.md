# One billion row challenge (1BRC)

## Introduction

[1BRC](https://github.com/gunnarmorling/1brc) data
looks like this:

```
Hamburg;12.0
Bulawayo;8.9
Palembang;38.8
St. John's;15.2
Cracow;12.6
Bridgetown;26.9
Istanbul;6.2
Roseau;34.4
Conakry;31.2
Istanbul;23.0
```

You need to calculate the min, max and mean for each
place, given a file containing 1 billion rows. There
are other details to the challenge but that's the crux
of it for me.

The original challenge is Java oriented, the best entry
scored around 1.5s on a bare metal Hetzner AX161 (32
core AMD EPYC™ 7502P (Zen2), 128 GB RAM), utilising 8
cores.

Since then, aficionados from many languages have
accepted the challenge and produced some amazing code.
The best
[C version](https://github.com/gunnarmorling/1brc/discussions/138)
uses custom SIMD instructions and completes in 0.15s:
unbelievable.

Writing some vaguely performant code turns out to be
harder than it may seem because the little things add
up when you do them a billion times, and it turns out
that most of us are ignorant of the little things.

# This repository

Note: all the timings are single threaded and executed
on my puny laptop: i7-1165G7 @ 2.80GHz, 16G RAM + SSD.

I've written three single threaded solutions for
different reasons:

1. [1brc.awk](1brc.awk) -- A naive and direct AWK
solution in 11 LoC. Simple as it is, it completes in
about **6m34s**. This is the sort of code you might
write in 10-15 minutes to just "get it done", yet one
has to invest a considerable amount of effort to beat
it.

```
time cat measurements.txt | ./1brc.awk | wc -l

8875

real    6m34.425s
user    6m30.106s
sys     0m17.593s
```

2. [1brc.hs](1brc.hs) -- I'm a fan of Haskell because
I find it much easier to express hard things in it.
This is a little 14 LoC solution which completes in 
about **7m19s**. Despite its size -- it isn't
particularly naively. It parses integers instead of
floats and uses lazy bytestring instead of string,
for performance. It uses `seq` tactically to force
strict evaluation to avoid exceeding the memory.

```
ghc -O2 1brc.hs
time ./1brc | wc -l 

8875

real    7m18.885s
user    7m14.640s
sys     0m5.075s
```
At the time of writing the best known single 
threaded solution completed in ~25s.

3. [1brc_lcrs.f90](1brc_lcrs.f90) -- A Fortran
implementation in 136 LoC, and my first ever
Fortran program! It is simple, direct, and runs
in **2m9s**. Its written from scratch and uses a
[binary prefix tree](https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree)
instead of a hash table. I've found the simplicity 
of Fortran to be a very pleasant surprise, even
though this kind of problem is far from its forte.

```
gfortran -march=native -O3 -o 1brc 1brc_lcrs.f90
time ./1brc | wc -l

8875

real    2m8.789s
user    2m4.470s
sys     0m4.247s
```

4. [1brc_trie.f90](1brc_trie.f90) -- Another
Fortran implementation in 117 LoC; this time using
a [trie](https://en.wikipedia.org/wiki/Trie)
structure for O(1) hops! Its conceptually the
same idea as the LCRS implementation above but
significantly more efficient because it obviates
the linear scan for siblings. It completes in
**1m25s**!

```
gfortran -march=native -O3 -o 1brc 1brc_trie.f90
time ./1brc | wc -l

8875

real    1m25.561s
user    1m21.932s
sys     0m3.638s
```
