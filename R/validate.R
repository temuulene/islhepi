# Input validation.
#
# These sit in front of the disclosure-control and rate functions. The point is
# that a wrong input should stop the calculation, not travel quietly into a
# published table.

# Counts must be whole, non-negative and finite.
#
# `as.numeric()` is deliberately not used to coerce here. On a factor it returns
# the level codes, not the labels, so `factor(c("10", "3", "42"))` would be read
# as 1, 2, 3 and suppressed against the wrong numbers entirely.
.islh_check_counts <- function(x, arg = "n", allow_na = TRUE) {
  if (is.factor(x)) {
    .islh_abort(c(
      "{.arg {arg}} is a factor.",
      x = "Converting a factor gives its level codes, not the counts it shows.",
      i = "Convert it first, for example
           {.code as.numeric(as.character({arg}))}."
    ))
  }

  if (is.character(x)) {
    converted <- suppressWarnings(as.numeric(x))
    if (any(is.na(converted) & !is.na(x))) {
      bad <- unique(x[is.na(converted) & !is.na(x)])
      .islh_abort(c(
        "{.arg {arg}} must be counts.",
        x = "{cli::qty(bad)}Value{?s} {.val {bad}} {?is/are} not {?a number/numbers}."
      ))
    }
    x <- converted
  }

  if (!is.numeric(x)) {
    .islh_abort("{.arg {arg}} must be numeric, not {.cls {class(x)[1]}}.")
  }

  present <- !is.na(x)

  if (!isTRUE(allow_na) && any(!present)) {
    .islh_abort("{.arg {arg}} must not contain missing values.")
  }

  if (any(is.infinite(x[present]))) {
    .islh_abort("{.arg {arg}} must be finite.")
  }

  negative <- x[present] < 0
  if (any(negative)) {
    .islh_abort(c(
      "{.arg {arg}} must not be negative.",
      x = "{cli::qty(sum(negative))}Found {sum(negative)} negative value{?s}."
    ))
  }

  fractional <- abs(x[present] - round(x[present])) > .Machine$double.eps^0.5
  if (any(fractional)) {
    .islh_abort(c(
      "{.arg {arg}} must be whole counts.",
      x = "{cli::qty(sum(fractional))}Found {sum(fractional)} fractional
           value{?s}.",
      i = "A rate or a proportion is not a count; pass the numerator instead."
    ))
  }

  x
}

# Populations must be positive and finite. Zero is rejected by default: a
# stratum with nobody in it has no rate, and dividing by it silently yields Inf.
#
# A zero denominator is a real value, not a missing one, so the error names the
# affected strata and says what to do with them rather than just refusing.
.islh_check_population <- function(x, arg = "population", allow_zero = FALSE) {
  if (!is.numeric(x)) {
    .islh_abort("{.arg {arg}} must be numeric, not {.cls {class(x)[1]}}.")
  }
  if (any(is.na(x))) {
    .islh_abort("{.arg {arg}} must not contain missing values.")
  }
  if (any(is.infinite(x))) {
    .islh_abort("{.arg {arg}} must be finite.")
  }
  if (any(x < 0)) {
    .islh_abort("{.arg {arg}} must not be negative.")
  }

  if (!isTRUE(allow_zero)) {
    zero <- which(x == 0)
    if (length(zero) > 0L) {
      .islh_abort(c(
        "{.arg {arg}} must be positive.",
        x = "{cli::qty(length(zero))}Position{?s} {.val {zero}} {?is/are} zero.",
        i = "Nobody is at risk there, so the stratum has no rate. Drop those
             strata, or combine them with a neighbouring one, before
             calculating."
      ))
    }
  }

  x
}

.islh_check_scalar_positive <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      is.infinite(x) || x <= 0) {
    .islh_abort("{.arg {arg}} must be a single positive, finite number.")
  }
  x
}

.islh_check_conf <- function(conf) {
  if (!is.numeric(conf) || length(conf) != 1L || is.na(conf) ||
      conf <= 0 || conf >= 1) {
    .islh_abort("{.arg conf} must be a single number between 0 and 1.")
  }
  conf
}

.islh_check_threshold <- function(threshold) {
  if (missing(threshold)) {
    .islh_abort(c(
      "{.arg threshold} must be supplied.",
      i = "The right threshold depends on the data and on the release, so
           there is deliberately no default."
    ))
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || is.infinite(threshold) || threshold < 0) {
    .islh_abort(
      "{.arg threshold} must be one non-negative, finite approved threshold."
    )
  }
  threshold
}

# Disclosure-control switches must fail closed. isTRUE() alone is unsafe here:
# NA, 1 and other invalid values would silently select FALSE.
.islh_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .islh_abort("{.arg {arg}} must be a single TRUE or FALSE.")
  }
  x
}

# Labels that state a compact numeric upper bound are checked against the rule.
# Arbitrary text is treated as a neutral/custom label because its semantics
# cannot be inferred safely.
.islh_check_suppression_label <- function(
  label,
  arg = "label",
  threshold = NULL,
  inclusive = NULL,
  complementary = FALSE
) {
  if (is.null(label)) {
    return(NULL)
  }
  if (!is.character(label) || length(label) != 1L ||
      is.na(label) || !nzchar(trimws(label))) {
    .islh_abort(
      "{.arg {arg}} must be NULL or one non-missing character string."
    )
  }

  compact <- gsub("[[:space:]]+", "", label)
  match <- regexec(
    "^(<=|<|\u2264)([0-9]+(?:\\.[0-9]+)?)$",
    compact,
    perl = TRUE
  )
  pieces <- regmatches(compact, match)[[1]]

  if (length(pieces) == 0L) {
    return(label)
  }

  if (isTRUE(complementary)) {
    .islh_abort(c(
      "{.arg {arg}} must not state a numeric bound.",
      x = paste0(
        "A complementary cell can be any size, so \"", label,
        "\" may be false."
      ),
      i = "Use a neutral label such as \"Suppressed\"."
    ))
  }

  if (is.null(threshold) || is.null(inclusive)) {
    return(label)
  }

  largest <- if (inclusive) floor(threshold) else ceiling(threshold) - 1
  if (largest < 1) {
    return(label)
  }

  operator <- pieces[[2]]
  bound <- as.numeric(pieces[[3]])
  truthful <- if (operator == "<") largest < bound else largest <= bound

  if (!truthful) {
    .islh_abort(c(
      "{.arg {arg}} does not describe every count the rule suppresses.",
      x = paste0(
        "The rule can suppress ", largest, ", which is not described by \"",
        label, "\"."
      ),
      i = paste0(
        "Use \"<", largest + 1, "\", \"<=", largest,
        "\", or a neutral label such as \"Suppressed\"."
      )
    ))
  }

  label
}




# Which columns did the caller mean?
#
# Every rejection here is a case that otherwise selects the wrong column
# silently. `data[[1.5]]` truncates to column 1. `data[[TRUE]]` is also column
# 1. A factor indexes by its level code, not its label, so
# `factor("cases")` picks column 1 of a table whose first column is something
# else entirely. Each of those publishes an unsuppressed column while
# reporting success.
.islh_check_cols <- function(cols, data, arg = "cols") {
  if (missing(cols) || is.null(cols)) {
    .islh_abort(c(
      "{.arg {arg}} must name the columns to suppress.",
      i = "There is no default: suppressing every column, or none, is a
           decision the caller has to make."
    ))
  }

  if (is.factor(cols)) {
    .islh_abort(c(
      "{.arg {arg}} is a factor.",
      x = "A factor indexes by its level codes, not by the names it shows.",
      i = "Convert it first, for example {.code as.character({arg})}."
    ))
  }
  if (is.logical(cols)) {
    .islh_abort(c(
      "{.arg {arg}} is logical.",
      x = "{.code TRUE} selects the first column rather than every column.",
      i = "Give column names, or positions such as {.code c(2, 3)}."
    ))
  }
  if (!is.character(cols) && !is.numeric(cols)) {
    .islh_abort(
      "{.arg {arg}} must be column names or positions, not {.cls {class(cols)[1]}}."
    )
  }
  if (length(cols) == 0L) {
    .islh_abort(c(
      "{.arg {arg}} is empty, so nothing would be suppressed.",
      i = "Name at least one column, or do not call this function."
    ))
  }
  if (anyNA(cols)) {
    .islh_abort("{.arg {arg}} must not contain missing values.")
  }

  if (is.character(cols)) {
    if (any(!nzchar(trimws(cols)))) {
      .islh_abort("{.arg {arg}} must not contain empty column names.")
    }
    unknown <- setdiff(cols, names(data))
    if (length(unknown) > 0L) {
      .islh_abort("{.arg data} has no column{?s} named {.field {unknown}}.")
    }
    selected <- cols
  } else {
    if (any(!is.finite(cols))) {
      .islh_abort("{.arg {arg}} must be finite column positions.")
    }
    fractional <- abs(cols - round(cols)) > .Machine$double.eps^0.5
    if (any(fractional)) {
      .islh_abort(c(
        "{.arg {arg}} must be whole column positions.",
        x = "{cli::qty(sum(fractional))}Position{?s} {.val {cols[fractional]}}
             {?is/are} fractional.",
        i = "A fractional position is truncated, so it selects a column you
             did not ask for."
      ))
    }
    cols <- as.integer(round(cols))
    outside <- cols < 1L | cols > ncol(data)
    if (any(outside)) {
      .islh_abort(c(
        "{.arg {arg}} must be column positions within {.arg data}.",
        x = "{.arg data} has {ncol(data)} column{?s}; got
             {.val {cols[outside]}}."
      ))
    }
    selected <- names(data)[cols]
  }

  duplicated_cols <- unique(selected[duplicated(selected)])
  if (length(duplicated_cols) > 0L) {
    .islh_abort(c(
      "{.arg {arg}} names the same column more than once.",
      x = "Repeated: {.field {duplicated_cols}}.",
      i = "Suppressing a column twice hides a second cell that the rule never
           selected."
    ))
  }

  selected
}

# Grouping columns for complementary suppression. These identify the row sets
# that each sum to a published total, so they must exist, be distinct, and not
# be the count columns being suppressed.
.islh_check_by <- function(by, data, cols, arg = "by") {
  if (is.null(by)) {
    return(character())
  }
  if (!is.character(by)) {
    .islh_abort(
      "{.arg {arg}} must be NULL or column names, not {.cls {class(by)[1]}}."
    )
  }
  if (length(by) == 0L) {
    return(character())
  }
  if (anyNA(by) || any(!nzchar(trimws(by)))) {
    .islh_abort("{.arg {arg}} must not contain missing or empty column names.")
  }
  unknown <- setdiff(by, names(data))
  if (length(unknown) > 0L) {
    .islh_abort("{.arg data} has no column{?s} named {.field {unknown}}.")
  }
  repeated <- unique(by[duplicated(by)])
  if (length(repeated) > 0L) {
    .islh_abort("{.arg {arg}} names {.field {repeated}} more than once.")
  }
  overlap <- intersect(by, cols)
  if (length(overlap) > 0L) {
    .islh_abort(c(
      "{.arg {arg}} and {.arg cols} share {.field {overlap}}.",
      i = "A column cannot both define the groups and be suppressed."
    ))
  }
  by
}
