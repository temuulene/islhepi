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

# Populations must be positive and finite. Zero is rejected: a stratum with
# nobody in it has no rate, and dividing by it silently yields Inf.
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
  limit <- if (isTRUE(allow_zero)) 0 else .Machine$double.eps
  if (any(x < limit)) {
    .islh_abort(
      "{.arg {arg}} must be {if (isTRUE(allow_zero)) 'non-negative' else 'positive'}."
    )
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


