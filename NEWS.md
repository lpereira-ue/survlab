# survlab 0.2.0

* `impute_nondetect()` gains a `control` argument accepting a
  `survival::survreg.control()` object. This allows users to tune the
  underlying `survreg` fitting algorithm - most notably `maxiter` - for
  datasets where the default iteration limit causes convergence warnings.
* `impute_nondetect()` gains a `seed` argument, set right before the imputed
  values are drawn, so that the imputation can be reproduced.
* `impute_nondetect()` gains a `zero_value` argument (default `1e-5`): exact
  zeros are replaced by it when fitting the log-normal, Weibull, exponential
  and log-logistic distributions, which previously failed silently on data
  containing zeros. The zeros are kept in the final values.
* `impute_nondetect()` now records the AIC and convergence of every
  distribution (attribute `fits`) and selects the best model among the
  converged fits only. If none converged, the best non-converged fit is used,
  a warning is raised, and attribute `best_converged` is `FALSE`. Convergence
  warnings from the individual fits are no longer passed on to the caller.
* Fixed: with `min_value = 0`, logistic imputations were not truncated at 0,
  and draws below 0 were set to exactly 0. They are now drawn from the
  logistic truncated between `min_value` and the detection limit.

# survlab 0.1.0

* Initial CRAN release.