# survlab 0.2.0

* `impute_nondetect()` gains a `control` argument accepting a
  `survival::survreg.control()` object. This allows users to tune the
  underlying `survreg` fitting algorithm - most notably `maxiter` - for
  datasets where the default iteration limit causes convergence warnings.

# survlab 0.1.0

* Initial CRAN release.