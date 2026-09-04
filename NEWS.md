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
