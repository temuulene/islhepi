#' Suppress small cell counts
#'
#' Replaces counts at or below an approved threshold, so a small cell is not
#' published. Zero is never suppressed: nobody having the thing is a real,
#' publishable finding, and hiding it removes information without protecting
#' anyone.
#'
#' @section Choosing a label:
#'
#' A label is a statement about the data, and it has to be true of every cell
#' it is applied to. `"<5"` is false for a cell equal to 5, which the default
#' `inclusive = TRUE` rule suppresses. Either use `inclusive = FALSE` with
#' `"<5"`, or use a neutral label such as `"Suppressed"`. Compact numeric
#' bounds such as `"<5"` and `"<=5"` are checked against the rule and rejected
#' when they could misstate a suppressed value.
#'
#' @param n Count vector. Must be whole, non-negative and finite. Factors are
#'   rejected rather than converted, because converting one gives its level
#'   codes rather than the counts it displays.
#' @param threshold Approved disclosure-control threshold. It must be supplied
#'   explicitly because the appropriate rule depends on the data and context.
#' @param inclusive Suppress positive counts less than or equal to the
#'   threshold. Must be one non-missing `TRUE` or `FALSE`.
#' @param label Display label for suppressed cells. Must be `NULL` or one
#'   non-missing character string. With `NULL`, suppressed values become
#'   missing numeric values; otherwise the result is a character vector.
#'
#' @return A vector with small positive cell counts suppressed.
#' @export
#'
#' @examples
#' islh_suppress(c(0, 3, 42), threshold = 5)
#'
#' # A neutral label is true whatever the rule.
#' islh_suppress(c(0, 3, 42), threshold = 5, label = "Suppressed")
#'
#' # "<5" is only accurate with the exclusive rule.
#' islh_suppress(c(4, 5, 6), threshold = 5, inclusive = FALSE, label = "<5")
islh_suppress <- function(n, threshold, inclusive = TRUE, label = NULL) {
  threshold <- .islh_check_threshold(threshold)
  inclusive <- .islh_check_flag(inclusive, "inclusive")
  label <- .islh_check_suppression_label(
    label,
    threshold = threshold,
    inclusive = inclusive
  )
  counts <- .islh_check_counts(n, arg = "n")

  small <- .islh_small(counts, threshold, inclusive)

  if (is.null(label)) {
    counts[small] <- NA_real_
    return(counts)
  }

  output <- as.character(counts)
  output[is.na(counts)] <- NA_character_
  output[small] <- as.character(label)
  output
}

# Which values does the rule hide? Shared so that every caller agrees on what
# counts as small. NA is never "small" — it is already absent.
.islh_small <- function(n, threshold, inclusive) {
  small <- if (isTRUE(inclusive)) {
    n > 0 & n <= threshold
  } else {
    n > 0 & n < threshold
  }
  small[is.na(small)] <- FALSE
  small
}


