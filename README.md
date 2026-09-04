# islhepi

Validated epidemiological methods for Island Health analyses.

`islhepi` contains the analytical helpers previously included in `islhr`:

* age grouping for surveillance and standardisation
* crude rates with Poisson confidence intervals
* directly standardised rates with Fay-Feuer or normal intervals
* small-cell and complementary suppression
* deterministic and controlled random rounding
* BC Stats population denominators from pinned BC Data Catalogue resources
* standardized HA, HSDA, LHA and CHSA boundary objects

The package implements calculation mechanics, not analytic or disclosure
policy. Confirm the appropriate threshold, standard population and interval
method for every release.

Documentation: <https://temuulene.github.io/islhepi/>

New users should start with
[Getting started with islhepi](https://temuulene.github.io/islhepi/articles/islhepi.html).
Every example in that article is evaluated and displays the returned values.

## Installing

Install the development version from GitHub:

```r
install.packages("remotes", type = "binary")
remotes::install_github("temuulene/islhepi")
```

Staff who cannot install from GitHub can install the Windows package attached
to a tagged release:

```r
install.packages("path/to/islhepi_0.2.0.zip", repos = NULL)
```

## Using it

Choose the function by the result you need:

| Task | Function |
|---|---|
| Group ages | `islh_age_group()` |
| Crude or directly standardised rates | `islh_crude_rate()`, `islh_dsr()` |
| Poisson count intervals | `islh_ci_poisson()` |
| Suppression | `islh_suppress()`, `islh_suppress_table()` |
| Count rounding | `islh_round_base()` |
| BC population and boundaries | `islh_bc_population()`, `islh_bc_geography()` |

```r
library(islhepi)

islh_crude_rate(cases = 12, population = 50000)
islh_age_group(c(0, 4, 18, 67, 91))
islh_suppress(c(0, 3, 42), threshold = 5)
```

## Population and geography data

`islhepi` uses permanent BC Data Catalogue identifiers instead of catalogue
search terms. This keeps the selected records stable while allowing the
Province to publish updated estimates and boundaries.

```r
library(dplyr)
library(islhepi)

population_extract <- islh_bc_population("lha", years = 2025, sex = "T")

population <- population_extract |>
  summarise(
    population = sum(population),
    .by = c(geography, geography_code, year, estimate_type)
  )

boundaries <- islh_bc_geography("lha")

map_data <- left_join(
  boundaries,
  population,
  by = join_by(geography, geography_code),
  relationship = "one-to-one",
  na_matches = "never"
)

if (anyNA(map_data$population)) {
  stop("One or more Island Health boundaries did not match population data.")
}
```

The default boundary filter uses the catalogue value `Vancouver Island`.
`health_authority = "Island Health"` is accepted as an alias, and `NULL`
returns all of BC. Use `islh_bc_sources()` to audit and cite the pinned record
and resource identifiers. Population resources cover all of BC, so population
rows outside the filtered boundary object are expected to be discarded by the
join. Network calls are never made when the package loads.

Downloaded data are not bundled into releases. Save a dated extract in the
analysis project when the governing reproducibility or retention standard
requires it.

Use [`islhr`](https://github.com/temuulene/islhr) for Island Health figures,
tables and Quarto reports. The packages can be loaded together.
