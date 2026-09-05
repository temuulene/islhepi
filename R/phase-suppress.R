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
#' @section Groups:
#'
#' `by` names the columns that identify each block of rows summing to its own
#' published total, and the complementary rule then runs once per block rather
#' than once per column. This matters whenever a table is subtotalled. A table
#' broken down by health authority has one total per authority, so a reader
#' subtracts within an authority; a single cell hidden in one authority is
#' recoverable even though the column as a whole has several cells hidden.
#'
#' With no `by`, the whole column is treated as one block, which is right for a
#' table with a single overall total.
#'
#' @section Auditing what was hidden:
#'
#' The result carries a record of every cell that was suppressed and why. Read
#' it with [islh_suppression_audit()] and keep it with your working notes: it
#' is what lets a reviewer confirm that the complementary rule fired where it
#' should have, without re-running the analysis.
#'
#' The record deliberately holds no counts. It says which cell was hidden, not
#' what was in it, so attaching it to a released table cannot leak the values
#' the suppression was there to protect.
#'
#' @param data A data frame.
#' @param cols Columns to suppress, as character names or whole numeric
#'   positions. Factors, logicals, repeated entries and positions outside
#'   `data` are rejected rather than resolved to a column you did not ask for.
#' @param threshold Approved disclosure-control threshold. It must be supplied
#'   explicitly because the appropriate rule depends on the data and context.
#' @param inclusive Suppress positive counts less than or equal to the
#'   threshold. Must be one non-missing `TRUE` or `FALSE`.
#' @param label Display label for cells hidden because they are small. With
#'   `NULL`, they become missing numeric values. See the label section of
#'   [islh_suppress()].
#' @param complementary Also suppress the smallest surviving value wherever
#'   exactly one cell was suppressed. Must be one non-missing `TRUE` or
#'   `FALSE`.
#' @param complementary_label Display label for cells hidden to protect another
#'   cell. Must not imply a value, since such a cell can be any size. Only used
#'   when `label` is set; with no `label` every suppressed cell becomes a
#'   missing value and the column stays numeric.
#' @param by Optional column names identifying the groups the complementary
#'   rule applies within. See the groups section. Ignored when
#'   `complementary = FALSE`.
#'
#' @return The data frame, with the named columns suppressed, carrying an audit
#'   record readable with [islh_suppression_audit()].
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
#'
#' # A subtotalled table is protected within each subtotal, not across the
#' # whole column.
#' by_authority <- data.frame(
#'   authority = c("Island", "Island", "Interior", "Interior"),
#'   area = c("North", "South", "East", "West"),
#'   cases = c(2, 30, 4, 25)
#' )
#'
#' hidden <- islh_suppress_table(
#'   by_authority, "cases", threshold = 5,
#'   complementary = TRUE, by = "authority", label = "Suppressed"
#' )
#' islh_suppression_audit(hidden)
islh_suppress_table <- function(
  data,
  cols,
  threshold,
  inclusive = TRUE,
  label = NULL,
  complementary = FALSE,
  complementary_label = "Suppressed",
  by = NULL
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

  cols <- .islh_check_cols(cols, data)
  by <- .islh_check_by(by, data, cols)

  # One label per row naming the block it belongs to. With no `by` every row is
  # in the same block, which is the whole-column rule.
  groups <- if (length(by) == 0L) {
    rep("", nrow(data))
  } else {
    do.call(paste, c(unname(as.list(data[by])), list(sep = " | ")))
  }

  audit <- vector("list", length(cols))
  names(audit) <- cols

  for (col in cols) {
    counts <- .islh_check_counts(data[[col]], arg = paste0("data$", col))
    small <- .islh_small(counts, threshold, inclusive)
    extra <- rep(FALSE, length(counts))

    if (isTRUE(complementary)) {
      for (group in unique(groups)) {
        rows <- which(groups == group)
        # Exactly one hidden cell is the recoverable case: a reader subtracts
        # the visible cells from the total and gets it back. Hide the smallest
        # survivor too. Two or more already break that arithmetic.
        if (sum(small[rows]) != 1L) {
          next
        }
        survivors <- counts[rows]
        survivors[small[rows] | is.na(survivors)] <- NA
        if (any(!is.na(survivors))) {
          extra[rows[which.min(survivors)]] <- TRUE
        }
      }
    }

    audit[[col]] <- .islh_suppression_record(col, groups, small, extra, by)

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

  attr(data, "islh_suppression") <- .islh_bind_audit(audit)
  data
}

# One row per hidden cell. Deliberately records no counts: see the auditing
# section of islh_suppress_table().
.islh_suppression_record <- function(column, groups, small, extra, by) {
  rows <- which(small | extra)
  data.frame(
    column = rep(column, length(rows)),
    row = rows,
    group = if (length(by) == 0L) {
      rep(NA_character_, length(rows))
    } else {
      as.character(groups[rows])
    },
    reason = ifelse(small[rows], "small", "complementary"),
    stringsAsFactors = FALSE
  )
}

.islh_bind_audit <- function(audit) {
  empty <- data.frame(
    column = character(),
    row = integer(),
    group = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
  audit <- audit[!vapply(audit, is.null, logical(1))]
  if (length(audit) == 0L) {
    return(empty)
  }
  out <- do.call(rbind, c(list(empty), unname(audit)))
  rownames(out) <- NULL
  out
}

#' Read the record of what a suppression hid
#'
#' Returns one row for every cell [islh_suppress_table()] hid, saying which
#' cell it was and why it was hidden. Keep it with your working notes so a
#' reviewer can confirm the rule behaved as intended.
#'
#' The record holds no counts. It says a cell was hidden, not what was in it,
#' so it is safe to keep alongside a released table.
#'
#' @param data A data frame returned by [islh_suppress_table()].
#'
#' @return A data frame with columns `column`, `row`, `group` and `reason`.
#'   `reason` is `"small"` for a cell the threshold caught and
#'   `"complementary"` for one hidden to protect another. `group` is the block
#'   the complementary rule ran within, or `NA` when no `by` was used. A table
#'   with nothing hidden gives a zero-row data frame.
#' @export
#'
#' @examples
#' counts <- data.frame(
#'   area = c("North", "Central", "South"),
#'   cases = c(3, 42, 17)
#' )
#'
#' hidden <- islh_suppress_table(
#'   counts, "cases", threshold = 5,
#'   complementary = TRUE, label = "Suppressed"
#' )
#' islh_suppression_audit(hidden)
islh_suppression_audit <- function(data) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  record <- attr(data, "islh_suppression", exact = TRUE)
  if (is.null(record)) {
    .islh_abort(c(
      "{.arg data} carries no suppression record.",
      i = "Records are attached by {.fn islh_suppress_table}. Subsetting a
           data frame drops them, so read the record before reshaping."
    ))
  }
  record
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


