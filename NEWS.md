# islhepi 0.1.0

First release of the Island Health analytical-methods package.

* Moved `islh_age_group()`, `islh_ci_poisson()`, `islh_crude_rate()`,
  `islh_dsr()`, `islh_suppress()`, `islh_suppress_table()` and
  `islh_round_base()` from `islhr` without changing their public interfaces.
* Kept the existing input validation and regression tests with the methods.
* Separated analytical methods from Island Health branding, table styling and
  Quarto report scaffolding.

