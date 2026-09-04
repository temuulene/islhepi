# islhepi

Validated epidemiological methods for Island Health analyses.

`islhepi` contains the analytical helpers previously included in `islhr`:

* age grouping for surveillance and standardisation
* crude rates with Poisson confidence intervals
* directly standardised rates with Fay-Feuer or normal intervals
* small-cell and complementary suppression
* deterministic and controlled random rounding

The package implements calculation mechanics, not analytic or disclosure
policy. Confirm the appropriate threshold, standard population and interval
method for every release.

## Installing

While this repository is private, installation requires a GitHub account with
access and a personal access token:

```r
install.packages("remotes", type = "binary")
remotes::install_github("temuulene/islhepi")
```

Staff who cannot install from GitHub can install the Windows package attached
to a tagged release:

```r
install.packages("path/to/islhepi_0.1.0.zip", repos = NULL)
```

## Using it

```r
library(islhepi)

islh_crude_rate(cases = 12, population = 50000)
islh_age_group(c(0, 4, 18, 67, 91))
islh_suppress(c(0, 3, 42), threshold = 5)
```

Use [`islhr`](https://github.com/temuulene/islhr) for Island Health figures,
tables and Quarto reports. The packages can be loaded together.

