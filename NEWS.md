# islhepi 0.4.0

## Output-changing

* `islh_ci_poisson()` and `islh_crude_rate()` gained `method = "auto"`, which is
  now the **default**. It uses the exact Poisson interval for counts below 10
  and Byar's approximation from 10 upwards, which is the choice most analysts
  would make by hand. Previously both defaulted to `"byar"` throughout, so
  small-count intervals were narrower than the exact method gives. Confidence
  limits for counts under 10 will change. Pass `method = "byar"` to reproduce
  earlier output, and keep an explicit method where a release has to match a
  published series that used one method throughout.
* Both functions now return a `method` column recording the interval used for
  each count, so a mixed table stays auditable.
* `islh_bc_population()` keeps rows whose population is zero. An age band with
  nobody in it is a real measurement rather than a gap, and dropping it turned
  a complete denominator table into an incomplete one. Extracts will have more
  rows than before, and joins that relied on the missing rows should be
  rechecked. The rate functions still refuse a zero denominator, and now name
  the affected strata.
* `islh_surveillance_snapshot()` refuses a `baseline` whose limits describe a
  different duration from the displayed total, and refuses `data` whose periods
  cannot be summed into the interval being shown. Code that compared a
  seven-day total against a baseline of single daily counts now errors instead
  of reporting limits that were seven times too small.

## New

* `islh_suppress_table()` gained `by`, which runs complementary suppression
  within each subtotalled block rather than across the whole column. A single
  cell hidden inside one subtotal is recoverable by subtraction even when the
  column as a whole has several cells hidden.
* Added `islh_suppression_audit()`, which reports every cell
  `islh_suppress_table()` hid and why. The record holds no counts, so it can be
  kept beside a released table.
* `islh_surveillance_snapshot()` gained `baseline_interval` for naming the
  duration of limits that did not come from `islh_surveillance_baseline()`.
* `islh_surveillance_baseline()` gained `interval` for naming the reporting
  period of its input.
* `islh_count_events()`, `islh_surveillance_baseline()` and
  `islh_surveillance_snapshot()` now carry the reporting period on their
  results, which is what the new compatibility checks read. Subsetting a data
  frame drops these attributes; name the period explicitly when a table is
  reshaped between steps.

## Fixes and documentation

* `islh_suppress_table()` rejects `cols` selections that previously resolved to
  the wrong column without saying so: fractional positions, which truncate;
  logical values, where `TRUE` selects the first column rather than every
  column; factors, which index by level code rather than by label; repeated
  entries; empty selections; and positions outside the data frame.
* Documented the alert boundary in `islh_surveillance_snapshot()`.
  `exceeds_reference` is `TRUE` when the total is at or above `upper_limit`,
  not strictly above it. The behaviour is unchanged; only the wording was
  ambiguous. Equality is now covered by a regression test.
* Added a reference test reproducing the multistratum worked example from Fay
  and Feuer (1997), Tables I and II: Michigan birth data with highly uneven
  weights, published rate 75.5 per 100,000 and gamma interval (67.7, 188.3).
  A second test checks the paper's exactness property, that the gamma interval
  reduces to the exact Poisson interval when the standard population is
  proportional to the study population.
* Documented how `islh_crude_rate()` and `islh_dsr()` treat zero denominators.

# islhepi 0.3.0

* Added `islh_check_events()` for record-level review of missing identifiers,
  invalid dates, duplicate identifiers and required-field gaps.
* Added `islh_count_events()` for strict, zero-filled daily, weekly,
  epidemiological-week, monthly, quarterly and annual event counts.
* Added `islh_surveillance_baseline()` for explicit mean-SD, historical-range
  and quantile reference limits, including an auditable finite-sample
  adjustment.
* Added `islh_surveillance_snapshot()` for current-window reporting tables with
  stable date columns, totals and optional baseline comparisons.
* Added an evaluated routine-surveillance guide using simulated event data.

# islhepi 0.2.0

* Added `islh_bc_population()` for tidy LHA and HSDA population denominators
  from pinned BC Stats resources through `bcdata`.
* Added `islh_bc_geography()` for standardized HA, HSDA, LHA and CHSA `sf`
  boundaries, with an Island Health default and stable character join keys.
* Added `islh_bc_sources()` so reports can audit and cite the exact catalogue
  records and resources.
* Added mocked data-access tests. Package checks do not depend on live catalogue
  availability.
* Fixed the installation guidance when more than one optional data package is
  missing. It now reports every package and a runnable installation command.
* Corrected the mapping example for provincial population data joined to the
  default Island Health boundary subset, and quieted routine CSV parsing output.

# islhepi 0.1.0

First release of the Island Health analytical-methods package.

* Moved `islh_age_group()`, `islh_ci_poisson()`, `islh_crude_rate()`,
  `islh_dsr()`, `islh_suppress()`, `islh_suppress_table()` and
  `islh_round_base()` from `islhr` without changing their public interfaces.
* Kept the existing input validation and regression tests with the methods.
* Separated analytical methods from Island Health branding, table styling and
  Quarto report scaffolding.
