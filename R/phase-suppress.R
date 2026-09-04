# Disclosure control.
#
# These implement mechanics, not policy. The threshold, the rounding base and
# whether complementary suppression is needed all depend on the release and on
# your privacy office. Nothing here has a default that decides that for you.

#' Suppress small cell counts across a data frame
#'
#' Applies [islh_suppress()] to several columns at once.
#'
#' @section Complementary suppression:
#'
#' Set `complementary = TRUE` when the table also shows a total, or when the
#' rows partition a known population. Suppressing a single cell in such a table
#' does not protect it: a reader subtracts the visible cells from the total and
#' recovers the value. Complementary suppression hides the next-smallest cell
#' as well so the arithmetic no longer closes.
#'
#' A complementary cell is hidden because of where it sits in the table, not
#' because it is small — it can be any size at all. It therefore never takes
#' `label`, which describes a small value. It takes `complementary_label`,
#' which defaults to a neutral `"Suppressed"`. Labelling a complementary cell
#' `"<5"` would publish a false statement about the data.
#'
#' Whether you need complementary suppression, and how many cells to hide, is a
#' disclosure policy question rather than a statistical one. Check the rule your
#' release is governed by before publishing.
#'
#' @param data A data frame.
#' @param cols Columns to suppress. Character names or numeric positions.
#' @param threshold Approved disclosure-control threshold. It must be supplied
#'   explicitly because the appropriate rule depends on the data and context.
#' @param inclusive Suppress positive counts less than or equal to the
#'   threshold. Must be one non-missing `TRUE` or `FALSE`.
#' @param label Display label for cells hidden because they are small. With
#'   `NULL`, they become missing numeric values. See the label section of
#'   [islh_suppress()].
#' @param complementary Also suppress the smallest surviving value in any
#'   column where exactly one cell was suppressed. Must be one non-missing
#'   `TRUE` or `FALSE`.
#' @param complementary_label Display label for cells hidden to protect another
#'   cell. Must not imply a value, since such a cell can be any size. Only used
#'   when `label` is set; with no `label` every suppressed cell becomes a
#'   missing value and the column stays numeric.
#'
#' @return The data frame, with the named columns suppressed.
#' @export
#'
#' @examples
#' counts <- data.frame(
#'   area = c("North", "Central", "South"),
#'   cases = c(3, 42, 17),
#'   contacts = c(1, 55, 4)
#' )
#'
#' islh_suppress_table(counts, c("cases", "contacts"), threshold = 5)
#'
#' # With a total in view, one suppressed cell can be recovered by
#' # subtraction, so a second is hidden. Note that the second cell is 17, and
#' # is labelled as suppressed rather than as small.
#' islh_suppress_table(
#'   counts, "cases", threshold = 5,
#'   complementary = TRUE, label = "Suppressed"
#' )
islh_suppress_table <- function(
  data,
  cols,
  threshold,
  inclusive = TRUE,
  label = NULL,
  complementary = FALSE,
  complementary_label = "Suppressed"
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  threshold <- .islh_check_threshold(threshold)
  inclusive <- .islh_check_flag(inclusive, "inclusive")
  complementary <- .islh_check_flag(complementary, "complementary")
  label <- .islh_check_suppression_label(
    label,
    threshold = threshold,
    inclusive = inclusive
  )
  if (!is.null(label) && complementary) {
    complementary_label <- .islh_check_suppression_label(
      complementary_label,
      arg = "complementary_label",
      complementary = TRUE
    )
  }

  if (is.character(cols)) {
    unknown <- setdiff(cols, names(data))
    if (length(unknown) > 0L) {
      .islh_abort("{.arg data} has no column{?s} named {.field {unknown}}.")
    }
  } else if (is.numeric(cols)) {
    if (any(cols < 1L | cols > ncol(data))) {
      .islh_abort("{.arg cols} must be column positions within {.arg data}.")
    }
  }

  for (col in cols) {
    counts <- .islh_check_counts(data[[col]], arg = paste0("data$", col))
    small <- .islh_small(counts, threshold, inclusive)

    extra <- rep(FALSE, length(counts))
    if (isTRUE(complementary) && sum(small) == 1L) {
      # Exactly one hidden cell is the recoverable case: a reader subtracts the
      # visible cells from the total and gets it back. Hide the smallest
      # survivor too. Two or more already break that arithmetic.
      survivors <- counts
      survivors[small | is.na(counts)] <- NA
      if (any(!is.na(survivors))) {
        extra[which.min(survivors)] <- TRUE
      }
    }

    # `label` alone decides the output type, as it does in islh_suppress().
    # With no label, every suppressed cell — small or complementary — becomes
    # a missing value and the column stays numeric.
    if (is.null(label)) {
      counts[small | extra] <- NA_real_
      data[[col]] <- counts
      next
    }

    output <- as.character(counts)
    output[is.na(counts)] <- NA_character_
    output[small] <- as.character(label)
    output[extra] <- if (is.null(complementary_label)) {
      NA_character_
    } else {
      as.character(complementary_label)
    }
    data[[col]] <- output
  }

  data
}

#' Round counts to a fixed base
#'
#' Rounding to a base is used alongside, or instead of, suppression when a
#' release must show every cell. `"nearest"` rounds deterministically, so the
#' same input always gives the same output. `"random"` (sometimes called
#' controlled or unbiased rounding) rounds up or down with probability set by
#' how far the value sits between the two neighbouring multiples, which removes
#' the systematic bias that deterministic rounding introduces in a total.
#'
#' Random rounding gives a different answer each run unless you set a seed, so
#' round once and save the result rather than rounding at render time.
#'
#' @param x Counts to round. Must be whole, non-negative and finite.
#' @param base Rounding base. Like the suppression threshold, this is a
#'   disclosure policy decision, so it must be supplied explicitly.
#' @param method `"nearest"` for deterministic rounding, `"random"` for
#'   controlled random rounding.
#'
#' @return A numeric vector of rounded counts.
#' @export
#'
#' @examples
#' islh_round_base(c(0, 2, 3, 7, 12, 43), base = 5)
#'
#' set.seed(42)
#' islh_round_base(c(2, 3, 7, 12), base = 5, method = "random")
islh_round_base <- function(x, base, method = c("nearest", "random")) {
  method <- match.arg(method)

  if (missing(base)) {
    .islh_abort(c(
      "{.arg base} must be supplied.",
      i = "The rounding base is a disclosure policy decision, so there is
           deliberately no default."
    ))
  }
  base <- .islh_check_scalar_positive(base, "base")
  x <- .islh_check_counts(x, arg = "x")

  if (method == "nearest") {
    # round() breaks .5 ties to even, which keeps rounding from drifting a
    # total in one direction.
    return(round(x / base) * base)
  }

  lower <- floor(x / base) * base
  remainder <- x - lower
  # Probability of rounding up equals the distance already travelled, so the
  # expected value is the original number.
  round_up <- stats::runif(length(x)) < (remainder / base)
  result <- ifelse(round_up, lower + base, lower)
  result[is.na(x)] <- NA_real_
  result
}


