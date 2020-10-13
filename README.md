# Difference Lists in Haskell

The `NonEmpty` version of difference lists: list-like type supporting O(1) append ans snoc operations.

This is a fork of a [`dlist`](http://hackage.haskell.org/package/dlist) package.

```
benchmarking right-append/List
time                 13.63 ms   (13.36 ms .. 14.04 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 13.60 ms   (13.49 ms .. 13.76 ms)
std dev              332.4 μs   (240.8 μs .. 409.5 μs)

benchmarking right-append/NonEmpty
time                 31.87 ms   (25.83 ms .. 35.97 ms)
                     0.927 R²   (0.890 R² .. 0.969 R²)
mean                 23.76 ms   (21.98 ms .. 26.23 ms)
std dev              4.648 ms   (3.053 ms .. 5.538 ms)
variance introduced by outliers: 78% (severely inflated)

benchmarking right-append/DList
time                 38.18 μs   (38.09 μs .. 38.32 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 38.18 μs   (38.03 μs .. 38.46 μs)
std dev              667.8 ns   (325.0 ns .. 1.164 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking right-append/NonEmptyDList
time                 33.58 μs   (33.30 μs .. 33.89 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 33.86 μs   (33.67 μs .. 34.14 μs)
std dev              801.9 ns   (616.6 ns .. 1.240 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking right-append/DNonEmpty
time                 79.79 μs   (79.60 μs .. 80.10 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 80.52 μs   (80.23 μs .. 80.92 μs)
std dev              1.162 μs   (757.0 ns .. 1.795 μs)
```
