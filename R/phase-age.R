# Age grouping.
#
# The band sets here are the published ones they are named after. PHASE should
# confirm which matches its own surveillance standard before these are used in
# a release; `breaks` also takes a numeric vector so a local standard needs no
# change to this file.

# Lower bounds of each band. The last entry is the open-ended top band.
.islh_age_breaks <- list(
  # Five-year bands to 85+, the standard shape for age-standardised rates and
  # the one the Canadian standard populations are published in.
  five_year = seq(0, 85, by = 5),

  # Public Health Agency of Canada surveillance bands.
  phac = c(0, 1, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80),

  # Broad bands for reports where five-year detail would suppress everything.
  broad = c(0, 20, 65)
)

#' Group ages into standard bands
#'
#' @param age Numeric ages in years. Negative values and `NA` return `NA`.
#' @param breaks Either the name of a published band set (`"five_year"`,
#'   `"phac"` or `"broad"`) or a numeric vector of band lower bounds starting
#'   at 0. The final band is always open-ended.
#' @param labels Optional character vector of band labels, one per band.
#'
#' @return An ordered factor with one level per band.
#' @export
#'
#' @examples
#' islh_age_group(c(0, 3, 17, 42, 67, 91))
#'
#' islh_age_group(c(2, 17, 42, 91), breaks = "broad")
#'
#' # A local standard needs no change to the package.
#' islh_age_group(c(2, 17, 42, 91), breaks = c(0, 18, 65))
islh_age_group <- function(age, breaks = "five_year", labels = NULL) {
  if (!is.numeric(age)) {
    .islh_abort("{.arg age} must be numeric.")
  }

  if (is.character(breaks)) {
    if (length(breaks) != 1L || !breaks %in% names(.islh_age_breaks)) {
      .islh_abort(c(
        "{.arg breaks} must be one of {.val {names(.islh_age_breaks)}},",
        i = "or a numeric vector of band lower bounds starting at 0."
      ))
    }
    bounds <- .islh_age_breaks[[breaks]]
  } else {
    bounds <- sort(unique(as.numeric(breaks)))
    if (length(bounds) < 2L || bounds[1] != 0) {
      .islh_abort("{.arg breaks} must start at 0 and have at least two bands.")
    }
  }

  if (is.null(labels)) {
    upper <- c(bounds[-1] - 1, Inf)
    labels <- ifelse(
      is.infinite(upper),
      paste0(bounds, "+"),
      ifelse(bounds == upper, as.character(bounds),
             paste0(bounds, "-", upper))
    )
  } else if (length(labels) != length(bounds)) {
    .islh_abort(
      "{.arg labels} must have one entry per band ({length(bounds)})."
    )
  }

  # `cut()` with right = FALSE gives [lower, next), which is how age bands are
  # defined: someone aged 4.9 is still in the 0-4 band.
  out <- cut(
    age,
    breaks = c(bounds, Inf),
    labels = labels,
    right = FALSE,
    ordered_result = TRUE
  )
  out[!is.na(age) & age < 0] <- NA
  out
}


