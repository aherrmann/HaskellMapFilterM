# Benchmark on monadic filtering on a map in Haskell

The module `Data.Map` (henceforth `M`) offers a filter function for map
containers which takes a pure function predicate and a map, and returns a new
map with all the elements removed on which the predicate is not true. However,
it does not offer a function like `filterM`, which takes a monadic predicate
and returns a filtered map in a monad.

The file [`benchmark.hs`](benchmark.hs) contains a few possible implementations
of such a function `filterM` on maps. Furthermore, the file
[`MapWithFilterM.hs`](MapWithFilterM.hs) contains a modified version of the
module `M` (henceforth `M'`) which includes an implementation of `filterM` that
works on the map's internal representation to avoid conversion-overhead.

## The Data

A performance comparison between all these implementations can be found in the
file
[`bench.html`](http://htmlpreview.github.io/?https://github.com/aherrmann/HaskellMapFilterM/blob/master/bench.html).
It was created with the
[criterion](https://hackage.haskell.org/package/criterion) package.

A list of the methods that were benchmarked follows:

  * `M.filter`: Use the built-in filter which takes a pure predicate. Useful
    for comparison. 
  * `toAscList`: Round-trip through an ascending list of elements, and use
    `filterM` on that list.
  * `toList`: Same as `toAscList` just with a not-necessarily ascending list.
  * `foldr`: Use `M.foldrWithKey` to construct a new map into which we add
    items if the monadic predicate is true.
  * `foldl`: Same as `foldr` but use a left-fold instead.
  * `maybe`: Use the traversable instance of `M.Map` to first translate every
    element `v` into either `Nothing` or `Just v` depending on the monadic
    predicate, then filter the `Nothing`s out, and finally apply `fromJust` to
    every element.
  * `M'.filter`: Is the same as the above `M.filter` just on the modified map
    module. Again, mainly for comparison.
  * `M'.filterM`: Use an implementation that works right on the internal
    representation of the map. Hence, it avoids conversions to lists and
    reconstruction of the map from scratch.

The fastest among the above is `M.filter`. Next up are `toList`/`toAscList`.
Surprisingly, `M'.filterM` does no better than `foldr`/`foldl`. And the slowest
is `maybe`.

## Conclusion

If you can use a pure predicate then use `M.filter`. If you have an inherently
monadic predicate then round-trip through a list with either `M.toAscList`, or
`M.toList` it doesn't matter much.
