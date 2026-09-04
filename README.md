# islhepi

Validated epidemiological methods for Island Health analyses.

`islhepi` provides reusable mechanics for routine surveillance, rates,
standardisation, disclosure control and BC population and geography data. It
does not choose case definitions, alert policy, suppression thresholds or the
public-health response to a signal.

Documentation: <https://temuulene.github.io/islhepi/>

New users should start with
[Getting started with islhepi](https://temuulene.github.io/islhepi/articles/islhepi.html).
For event-level reporting, see the evaluated
[routine surveillance guide](https://temuulene.github.io/islhepi/articles/surveillance.html).

## Installing

Install the development version from GitHub:

```r
install.packages("remotes", type = "binary")
remotes::install_github("temuulene/islhepi")
```

Staff who cannot install from GitHub can install the Windows package attached
to a tagged release:

```r
install.packages("path/to/islhepi_0.3.0.zip", repos = NULL)
```

## Choose the function

| Task | Function |
|---|---|
| Find record-level event-data problems | `islh_check_events()` |
| Create complete daily or weekly counts | `islh_count_events()` |
| Calculate a historical reference | `islh_surveillance_baseline()` |
| Create a current-window reporting table | `islh_surveillance_snapshot()` |
| Group ages | `islh_age_group()` |
| Crude or directly standardised rates | `islh_crude_rate()`, `islh_dsr()` |
| Poisson count intervals | `islh_ci_poisson()` |
| Suppression | `islh_suppress()`, `islh_suppress_table()` |
| Count rounding | `islh_round_base()` |
| BC population and boundaries | `islh_bc_population()`, `islh_bc_geography()` |

## Routine surveillance

```r
library(islhepi)

issues <- islh_check_events(
  events,
  id = encounter_id,
  date = event_date,
  required = c(site, classification)
)

daily <- islh_count_events(
  events,
  date = event_date,
  id = encounter_id,
  by = site,
  interval = "day",
  fill = TRUE,
  groups = c("Central", "North", "South")
)

snapshot <- islh_surveillance_snapshot(
  daily,
  date = period_start,
  value = count,
  by = site,
  periods = 7
)
```

`islh_count_events()` supports ordinary weeks with a chosen start day, ISO
weeks and CDC epidemiological weeks. It constructs reporting periods from dates
instead of requiring hand-written corrections around New Year.

## Rates and disclosure control

```r
islh_crude_rate(cases = 12, population = 50000)
islh_age_group(c(0, 4, 18, 67, 91))
islh_suppress(c(0, 3, 42), threshold = 5)
```

Counts may exceed population or person-time when events can recur. Suppression
thresholds and rounding bases must be supplied explicitly for each release.

## Population and geography data

`islhepi` uses permanent BC Data Catalogue identifiers instead of catalogue
search terms. This keeps the selected records stable while allowing the
Province to publish updated estimates and boundaries.

```r
library(dplyr)

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
returns all of BC. Population resources cover all of BC, so population rows
outside the filtered boundary object are expected to be discarded by the join.

Downloaded data are not bundled into releases. Save a dated extract in the
analysis project when the governing reproducibility or retention standard
requires it.

Use [`islhr`](https://github.com/temuulene/islhr) for Island Health figures,
tables and Quarto reports. The packages can be loaded together.
