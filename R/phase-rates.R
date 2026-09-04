# Rates and confidence intervals.
#
# The methods here are the published ones named in each function's docs. They
# are defensible defaults, not Island Health policy: confirm with PHASE that
# they match the standard your release is governed by.

#' Confidence interval for a Poisson count
#'
#' `"byar"` uses Byar's approximation, the method Statistics Canada and most
#' cancer registries use for rate intervals. It is accurate for counts of
#' roughly 10 or more and much cheaper than the exact method.
#'
#' `"exact"` inverts the Poisson distribution through the chi-squared
#' relationship (the Garwood interval). Use it for small counts, where Byar's
#' approximation drifts.
#'
#' Byar's approximation is undefined at zero, so a count of zero uses the exact
#' interval whichever method you ask for.
#'
#' @param x Observed counts.
#' @param conf Confidence level.
#' @param method `"byar"` or `"exact"`.
#'
#' @return A data frame with columns `x`, `lower` and `upper`.
#' @export
#'
#' @references
#' Breslow NE, Day NE (1987). *Statistical Methods in Cancer Research,
#' Volume II*. IARC Scientific Publications No. 82, section 2.3.
#'
#' @examples
#' islh_ci_poisson(c(0, 3, 25, 100))
#'
#' # Small counts are where the two methods disagree.
#' islh_ci_poisson(3, method = "exact")
islh_ci_poisson <- function(x, conf = 0.95, method = c("byar", "exact")) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  x <- .islh_check_counts(x, arg = "x")

  alpha <- 1 - conf

  exact_lower <- function(x) {
    ifelse(x == 0, 0, stats::qchisq(alpha / 2, 2 * x) / 2)
  }
  exact_upper <- function(x) {
    stats::qchisq(1 - alpha / 2, 2 * (x + 1)) / 2
  }

  if (method == "exact") {
    return(data.frame(x = x, lower = exact_lower(x), upper = exact_upper(x)))
  }

  z <- stats::qnorm(1 - alpha / 2)
  # Byar's approximation, via the cube-root (Wilson-Hilferty) transformation
  # of the chi-squared distribution.
  lower <- x * (1 - 1 / (9 * x) - z / (3 * sqrt(x)))^3
  upper <- (x + 1) * (1 - 1 / (9 * (x + 1)) + z / (3 * sqrt(x + 1)))^3

  # Byar's is undefined at zero; fall back to the exact interval there.
  zero <- !is.na(x) & x == 0
  lower[zero] <- 0
  upper[zero] <- exact_upper(0)

  data.frame(x = x, lower = pmax(lower, 0), upper = upper)
}

#' Crude rate with a confidence interval
#'
#' Divides event counts by a population or person-time denominator and scales
#' to `per`. The interval comes from [islh_ci_poisson()] on the event count,
#' scaled the same way. Counts may exceed the denominator when people can
#' experience more than one event.
#'
#' @param cases Non-negative whole event counts.
#' @param population Population or person-time at risk. Recycled if length 1.
#' @param per Rate denominator. 100,000 by convention in public health.
#' @param conf Confidence level.
#' @param method Interval method passed to [islh_ci_poisson()].
#'
#' @return A data frame with columns `cases`, `population`, `rate`, `lower`
#'   and `upper`.
#' @export
#'
#' @examples
#' islh_crude_rate(cases = c(12, 45), population = c(50000, 120000))
islh_crude_rate <- function(
  cases,
  population,
  per = 100000,
  conf = 0.95,
  method = c("byar", "exact")
) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  per <- .islh_check_scalar_positive(per, "per")
  cases <- .islh_check_counts(cases, arg = "cases")

  if (length(population) == 1L) {
    population <- rep(population, length(cases))
  }
  if (length(population) != length(cases)) {
    .islh_abort(
      "{.arg population} must be length 1 or the same length as {.arg cases}."
    )
  }
  population <- .islh_check_population(population)


  ci <- islh_ci_poisson(cases, conf = conf, method = method)
  scale <- per / population

  data.frame(
    cases = cases,
    population = population,
    rate = cases * scale,
    lower = ci$lower * scale,
    upper = ci$upper * scale
  )
}

#' Directly standardised rate
#'
#' Weights stratum-specific rates by a standard population, so rates from
#' populations with different age structures can be compared.
#'
#' The default `"gamma"` interval is Fay and Feuer's method. It keeps its
#' nominal coverage when a few strata dominate the weighted variance, which is
#' the usual situation with age-standardised rates and small counts, where a
#' normal-approximation interval is too narrow and can fall below zero.
#' `"normal"` is provided for comparison with published figures that used it.
#'
#' @param cases Non-negative whole event counts per stratum. Counts may exceed
#'   the corresponding denominator when events can recur.
#' @param population Population or person-time at risk per stratum.
#' @param std_population Standard population per stratum. Only the relative
#'   sizes matter; they are normalised to weights internally.
#' @param per Rate denominator.
#' @param conf Confidence level.
#' @param method `"gamma"` for Fay-Feuer, `"normal"` for the normal
#'   approximation.
#'
#' @return A one-row data frame with columns `cases`, `population`, `rate`,
#'   `lower` and `upper`.
#' @export
#'
#' @references
#' Fay MP, Feuer EJ (1997). Confidence intervals for directly standardized
#' rates: a method based on the gamma distribution.
#' *Statistics in Medicine* 16(7):791-801.
#'
#' @examples
#' cases <- c(5, 12, 40, 80)
#' population <- c(20000, 25000, 22000, 15000)
#' standard <- c(30000, 30000, 25000, 15000)
#'
#' islh_dsr(cases, population, standard)
#'
#' # Compare with the crude rate: standardising removes the effect of this
#' # population being older than the standard.
#' islh_crude_rate(sum(cases), sum(population))
islh_dsr <- function(
  cases,
  population,
  std_population,
  per = 100000,
  conf = 0.95,
  method = c("gamma", "normal")
) {
  method <- match.arg(method)
  conf <- .islh_check_conf(conf)
  per <- .islh_check_scalar_positive(per, "per")

  n <- length(cases)
  if (n == 0L) {
    .islh_abort("{.arg cases} must have at least one stratum.")
  }
  if (length(population) != n || length(std_population) != n) {
    .islh_abort(c(
      "{.arg cases}, {.arg population} and {.arg std_population} must be the
       same length (one entry per stratum).",
      x = "Lengths are {n}, {length(population)} and {length(std_population)}."
    ))
  }

  # A standardised rate is a single number summed over every stratum, so one
  # missing stratum makes the whole result missing. Refuse rather than return
  # an NA that looks like a computed answer.
  cases <- .islh_check_counts(cases, arg = "cases", allow_na = FALSE)
  population <- .islh_check_population(population)
  std_population <- .islh_check_population(
    std_population, "std_population", allow_zero = TRUE
  )


  total_standard <- sum(std_population)
  if (total_standard <= 0) {
    .islh_abort(c(
      "{.arg std_population} must have a positive total.",
      x = "Every stratum is zero, so there are no weights to standardise by."
    ))
  }

  weights <- std_population / total_standard
  rate <- sum(weights * cases / population)
  variance <- sum(weights^2 * cases / population^2)

  alpha <- 1 - conf

  if (method == "normal") {
    z <- stats::qnorm(1 - alpha / 2)
    lower <- rate - z * sqrt(variance)
    upper <- rate + z * sqrt(variance)
  } else {
    # Fay-Feuer. The gamma interval matches the first two moments of the
    # weighted sum of Poisson counts, so it behaves when one stratum's weight
    # dominates. `w_max` enters the upper limit and keeps it conservative.
    w_max <- max(weights / population)

    lower <- if (rate == 0) {
      0
    } else {
      (variance / (2 * rate)) *
        stats::qchisq(alpha / 2, 2 * rate^2 / variance)
    }
    upper <- ((variance + w_max^2) / (2 * (rate + w_max))) *
      stats::qchisq(
        1 - alpha / 2,
        2 * (rate + w_max)^2 / (variance + w_max^2)
      )
  }

  data.frame(
    cases = sum(cases),
    population = sum(population),
    rate = rate * per,
    lower = max(lower, 0) * per,
    upper = upper * per
  )
}


