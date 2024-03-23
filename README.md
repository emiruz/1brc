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

Writing performant code turns out to be harder than it
may seem because the little things add up when you do
them a billion times.

# Experiments

Note: all the timings were performed on my puny laptop:
XPS13, i7-1165G7 @ 2.80GHz, 16G RAM and SSD (SATA). The
`measurements.txt` file is about 15.5Gb: it does not
fit into RAM. Maximum disk sequential read rate is
about 560MB/s:
```
time cat measurements.txt > /dev/null

real    0m12.919s
user    0m0.004s
sys     0m4.653s
```

List of experiments:

1. [1brc.awk](1brc.awk) -- A naive and direct AWK
solution in 11 LoC. Simple as it is, it completes in
about **6m34s**. This is the sort of code you might
write in 10-15 minutes to just "get it done", yet one
has to invest a considerable amount of effort to beat
it. It is also very useful for checking the output of
more complicated implementations since 11 LoC is much
easier to audit.
```
time mawk -f 1brc.awk measurements.txt | wc -l

8875

real    6m21.198s
user    6m16.224s
sys     0m4.943s
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
At the time of writing the best known single threaded
solution completed in about 25s. At around 250-300
LoC.

3. [1brc.pl](1brc.pl) -- A simple Prolog implementation
in 21 LoC. It uses a DCG to parse the input left to
right and build up a mutable hashtable. It would
complete in **44min10s** (estimated from 1M rows since
its so slow).
```
time swipl 1brc.pl | wc -l

8875

...
```
I suspect this could be made much faster by
implementing a problem specific hash table. I suspect
there is no way to make Prolog fast because backtracking
implies lots of unnecessary scaffolding in this case.

4. [1brc_lcrs.f90](1brc_lcrs.f90) -- A Fortran
implementation in 136 LoC, and my first ever
Fortran program! It is simple, direct, and runs
in **2m9s**. Its written from scratch and uses a
[binary prefix tree](https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree)
instead of a hash table. I've found the simplicity 
of Fortran to be a very pleasant surprise, even
though this kind of problem is not its forte.
```
gfortran -march=native -O3 -o 1brc 1brc_lcrs.f90
time ./1brc | wc -l

8875

real    2m8.789s
user    2m4.470s
sys     0m4.247s
```

5. [1brc_trie.f90](1brc_trie.f90) -- A Fortran
implementation in 118 LoC; using
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

6. [1brc_hash.f90](1brc_hash.f90) -- A Fortran
hash table based implementation in 134 LoC. 
It uses a version of the
[FNV1-a](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function#FNV-1a_hash)
hash algorithm + linear scan, to index an array
directly. It completes in **44s**!
```
gfortran -march=native -flto -ffast-math -Ofast -o 1brc 1brc_hash.f90
time ./1brc | wc -l

8875

real    0m43.873s
user    0m39.283s
sys     0m4.521s

```
I'd note that at this point, just reading
`measurements.txt` and breaking it into new
lines was taking about 20s, so the rest of the
processing only requires 24s!

7. [1brc_hash_mmap.f90](1brc_hash_mmap.f90) --
A Fortran hash table based implementation
additionally using using mmap via the C
interface. It also processes bytes directly
rather than characters: more fancy all around.
It completes in **38s**!
```
gfortran -march=native -ffast-math -Ofast -o 1brc 1brc_hash_mmap.f90
time ./1brc | wc -l

8875

real    0m37.513s
user    0m35.840s
sys     0m1.379s

```
I suspect that this solution would be significantly
faster on a machine with a fast SSD and enough
memory for the whole file to fit.

8. [1brc_hash_mmap_openmp.f90](1brc_hash_mmap_openmp.f90) --
Fortran, hash table + mmap + OpenMP for parallel
processing. Timing based on 4 cores. Best completion
time is **6s**!
```
gfortran-13 -fopenmp -march=native -ffast-math -O3 -o 1brc 1brc_hash_mmap_openmp.f90
time ./1brc | wc -l

8875

real    0m5.712s
user    0m20.291s
sys     0m0.643s
```
Above results with `integer, parameter :: parts = 8`
on my 4 core laptop (hyper-threaded). I can see that
the disk read speed is the bottleneck. The CPUs do
not saturate. Using gfortran-13 reduces the variance
in runs greatly compared to gfortran-10.
