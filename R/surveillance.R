# Surveillance data helpers --------------------------------------------------

.islh_surv_select <- function(data, quo, arg, minimum = 0L, maximum = Inf) {
  if (rlang::quo_is_null(quo)) {
    selected <- character()
  } else {
    selected <- tryCatch(
      names(tidyselect::eval_select(quo, data = data)),
      error = function(error) {
        .islh_abort(c(
          "Could not select {.arg {arg}} from {.arg data}.",
          x = conditionMessage(error)
        ))
      }
    )
  }

  if (length(selected) < minimum || length(selected) > maximum) {
    if (minimum == 1L && maximum == 1L) {
      .islh_abort("{.arg {arg}} must select exactly one column.")
    }
    .islh_abort("{.arg {arg}} selected an unsupported number of columns.")
  }

  selected
}

.islh_surv_missing <- function(x) {
  missing <- is.na(x)
  if (is.character(x) || is.factor(x)) {
    missing <- missing | trimws(as.character(x)) == ""
  }
  missing
}

.islh_surv_date_info <- function(x) {
  n <- length(x)
  value <- rep(as.Date(NA), n)
  missing <- .islh_surv_missing(x)
  invalid <- rep(FALSE, n)

  if (inherits(x, "Date")) {
    value <- as.Date(x)
  } else if (inherits(x, "POSIXt")) {
    value <- as.Date(x)
  } else if (is.character(x) || is.factor(x)) {
    text <- trimws(as.character(x))
    shape_ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)
    parsed <- suppressWarnings(as.Date(text, format = "%Y-%m-%d"))
    round_trip <- !is.na(parsed) & format(parsed, "%Y-%m-%d") == text
    valid <- !missing & shape_ok & round_trip
    value[valid] <- parsed[valid]
    invalid <- !missing & !valid
  } else {
    invalid <- !missing
  }

  list(value = value, missing = missing, invalid = invalid)
}

.islh_surv_as_date <- function(x, arg, scalar = FALSE) {
  info <- .islh_surv_date_info(x)
  if (any(info$missing)) {
    .islh_abort(c(
      "{.arg {arg}} must not contain missing dates.",
      x = "Found {sum(info$missing)} missing date value{?s}."
    ))
  }
  if (any(info$invalid)) {
    bad <- unique(as.character(x[info$invalid]))
    .islh_abort(c(
      "{.arg {arg}} contains invalid dates.",
      x = "Invalid: {.val {bad}}.",
      i = "Use Date values or ISO dates written as YYYY-MM-DD."
    ))
  }
  if (isTRUE(scalar) && length(info$value) != 1L) {
    .islh_abort("{.arg {arg}} must be one date.")
  }
  info$value
}

.islh_surv_interval <- function(interval) {
  match.arg(
    interval,
    c("day", "week", "isoweek", "epiweek", "month", "quarter", "year")
  )
}

.islh_surv_week_start <- function(interval, week_start) {
  if (interval == "isoweek") {
    return(1L)
  }
  if (interval == "epiweek") {
    return(7L)
  }
  if (!is.numeric(week_start) || length(week_start) != 1L ||
      is.na(week_start) || week_start != round(week_start) ||
      week_start < 1L || week_start > 7L) {
    .islh_abort("{.arg week_start} must be one whole number from 1 to 7.")
  }
  as.integer(week_start)
}

.islh_surv_period_start <- function(date, interval, week_start) {
  unit <- if (interval %in% c("isoweek", "epiweek")) "week" else interval
  as.Date(lubridate::floor_date(date, unit = unit, week_start = week_start))
}

.islh_surv_period_end <- function(period_start, interval, week_start) {
  if (interval == "day") {
    return(period_start)
  }
  unit <- if (interval %in% c("isoweek", "epiweek")) "week" else interval
  as.Date(lubridate::ceiling_date(
    period_start,
    unit = unit,
    week_start = week_start,
    change_on_boundary = TRUE
  )) - 1
}

.islh_surv_period_sequence <- function(from, to, interval) {
  by <- switch(
    interval,
    day = "day",
    week = "week",
    isoweek = "week",
    epiweek = "week",
    month = "month",
    quarter = "3 months",
    year = "year"
  )
  seq.Date(from = from, to = to, by = by)
}

.islh_surv_periods_back <- function(end, periods, interval) {
  by <- switch(
    interval,
    day = "-1 day",
    week = "-1 week",
    isoweek = "-1 week",
    epiweek = "-1 week",
    month = "-1 month",
    quarter = "-3 months",
    year = "-1 year"
  )
  sort(seq.Date(from = end, by = by, length.out = periods))
}

.islh_surv_groups <- function(groups, by, observed) {
  if (length(by) == 0L) {
    if (!is.null(groups)) {
      .islh_abort("{.arg groups} can only be supplied when {.arg by} is used.")
    }
    return(data.frame(.islh_all = 1L)[FALSE, , drop = FALSE])
  }

  if (is.null(groups)) {
    return(unique(observed[by]))
  }

  if (is.data.frame(groups)) {
    missing <- setdiff(by, names(groups))
    if (length(missing) > 0L) {
      .islh_abort(c(
        "{.arg groups} is missing grouping columns.",
        x = "Missing: {.val {missing}}."
      ))
    }
    return(unique(groups[by]))
  }

  if (length(by) != 1L) {
    .islh_abort(
      "For multiple {.arg by} columns, {.arg groups} must be a data frame."
    )
  }

  out <- data.frame(groups, stringsAsFactors = FALSE)
  names(out) <- by
  unique(out)
}

.islh_surv_grid <- function(groups, periods, by) {
  period_data <- data.frame(
    period_start = periods,
    stringsAsFactors = FALSE
  )

  if (length(by) == 0L) {
    return(period_data)
  }
  if (nrow(groups) == 0L) {
    return(groups[FALSE, , drop = FALSE])
  }
  dplyr::cross_join(groups, period_data)
}

#' Find record-level problems in event data
#'
#' `islh_check_events()` returns one row for every problem it finds. It does not
#' alter the input and does not hide the affected records, making the result
#' suitable for review with a data owner before analysis continues.
#'
#' @param data A data frame containing one row per event or encounter.
#' @param id Column containing the event identifier.
#' @param date Column containing the event date. Date, POSIXt and ISO
#'   `YYYY-MM-DD` character values are supported.
#' @param required Optional tidy-select specification of fields that must be
#'   present and non-missing.
#' @param one_row_per_id Whether repeated non-missing identifiers are problems.
#' @param min_date,max_date Optional inclusive date limits. `max_date` defaults
#'   to today; use `NULL` when future-dated records are expected.
#'
#' @return A data frame with `.row`, `.id`, `.issue`, `.field` and `.value`.
#'   A valid input returns a zero-row data frame with the same columns.
#'
#' @examples
#' events <- data.frame(
#'   record = c("A1", "A1", "", "A4"),
#'   onset = c("2026-01-03", "2026-01-04", NA, "not a date"),
#'   condition = c("Influenza", "Influenza", "COVID-19", "")
#' )
#'
#' islh_check_events(
#'   events,
#'   id = record,
#'   date = onset,
#'   required = condition,
#'   max_date = NULL
#' )
#'
#' @export
islh_check_events <- function(
    data,
    id,
    date,
    required = NULL,
    one_row_per_id = TRUE,
    min_date = NULL,
    max_date = Sys.Date()) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  one_row_per_id <- .islh_check_flag(one_row_per_id, "one_row_per_id")

  id_name <- .islh_surv_select(data, rlang::enquo(id), "id", 1L, 1L)
  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  required_names <- .islh_surv_select(
    data,
    rlang::enquo(required),
    "required"
  )
  required_names <- unique(c(id_name, date_name, required_names))

  ids <- data[[id_name]]
  date_info <- .islh_surv_date_info(data[[date_name]])
  issues <- list()

  add_issue <- function(rows, issue, field, values) {
    rows <- which(rows)
    if (length(rows) == 0L) {
      return(invisible(NULL))
    }
    issues[[length(issues) + 1L]] <<- data.frame(
      .row = rows,
      .id = as.character(ids[rows]),
      .issue = issue,
      .field = field,
      .value = as.character(values[rows]),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  for (field in required_names) {
    missing <- .islh_surv_missing(data[[field]])
    issue <- if (field == id_name) {
      "missing_id"
    } else if (field == date_name) {
      "missing_date"
    } else {
      "missing_required"
    }
    add_issue(missing, issue, field, data[[field]])
  }

  add_issue(
    date_info$invalid,
    "invalid_date",
    date_name,
    data[[date_name]]
  )

  present_id <- !.islh_surv_missing(ids)
  if (isTRUE(one_row_per_id)) {
    duplicate <- present_id &
      (duplicated(ids) | duplicated(ids, fromLast = TRUE))
    add_issue(duplicate, "duplicate_id", id_name, ids)
  }

  usable_date <- !date_info$missing & !date_info$invalid
  if (!is.null(min_date)) {
    min_date <- .islh_surv_as_date(min_date, "min_date", scalar = TRUE)
    before <- usable_date & date_info$value < min_date
    add_issue(before, "date_before_minimum", date_name, data[[date_name]])
  }
  if (!is.null(max_date)) {
    max_date <- .islh_surv_as_date(max_date, "max_date", scalar = TRUE)
    after <- usable_date & date_info$value > max_date
    add_issue(after, "date_after_maximum", date_name, data[[date_name]])
  }

  if (length(issues) == 0L) {
    return(data.frame(
      .row = integer(),
      .id = character(),
      .issue = character(),
      .field = character(),
      .value = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- dplyr::bind_rows(issues)
  out[order(out$.row, out$.issue, out$.field), , drop = FALSE]
}

#' Count events in complete reporting periods
#'
#' `islh_count_events()` converts event-level or already aggregated data into a
#' consistent period table. It handles epidemiological and ISO weeks, constructs
#' missing zero-count periods and marks periods that extend beyond the reporting
#' cutoff.
#'
#' @param data A data frame.
#' @param date Date column.
#' @param id Optional identifier column. When supplied, distinct non-missing IDs
#'   are counted. When neither `id` nor `count` is supplied, rows are counted.
#' @param count Optional column of pre-aggregated whole counts. `id` and `count`
#'   cannot both be supplied.
#' @param by Optional tidy-select specification of grouping columns.
#' @param interval Reporting interval: `"day"`, `"week"`, `"isoweek"`,
#'   `"epiweek"`, `"month"`, `"quarter"` or `"year"`.
#' @param week_start Start of an ordinary week, where 1 is Monday and 7 is
#'   Sunday. ISO and epidemiological weeks always use Monday and Sunday,
#'   respectively.
#' @param from,to Optional inclusive date boundaries. Defaults to the observed
#'   date range.
#' @param fill Whether to add zero-count periods.
#' @param include_partial Whether to retain a final period whose end is after
#'   `to`.
#' @param groups Optional expected groups. For one `by` column this may be a
#'   vector; for multiple columns supply a data frame containing valid group
#'   combinations. This is how groups with no events anywhere in the selected
#'   window can still be shown.
#'
#' @return A data frame containing the grouping columns, `period_start`,
#'   `period_end`, `count` and `partial_period`.
#'
#' @examples
#' events <- data.frame(
#'   encounter = 1:6,
#'   event_date = as.Date("2026-01-01") + c(0, 0, 2, 8, 8, 15),
#'   region = c("North", "North", "South", "North", "South", "North")
#' )
#'
#' islh_count_events(
#'   events,
#'   date = event_date,
#'   id = encounter,
#'   by = region,
#'   interval = "week",
#'   week_start = 7,
#'   fill = TRUE,
#'   include_partial = TRUE
#' )
#'
#' @export
islh_count_events <- function(
    data,
    date,
    id = NULL,
    count = NULL,
    by = NULL,
    interval = c(
      "day", "week", "isoweek", "epiweek", "month", "quarter", "year"
    ),
    week_start = 1,
    from = NULL,
    to = NULL,
    fill = TRUE,
    include_partial = FALSE,
    groups = NULL) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  interval <- .islh_surv_interval(interval)
  week_start <- .islh_surv_week_start(interval, week_start)
  fill <- .islh_check_flag(fill, "fill")
  include_partial <- .islh_check_flag(include_partial, "include_partial")

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  id_name <- .islh_surv_select(data, rlang::enquo(id), "id", 0L, 1L)
  count_name <- .islh_surv_select(data, rlang::enquo(count), "count", 0L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")

  if (length(id_name) > 0L && length(count_name) > 0L) {
    .islh_abort("Supply only one of {.arg id} and {.arg count}.")
  }
  reserved <- intersect(
    by_names,
    c("period_start", "period_end", "count", "partial_period")
  )
  if (length(reserved) > 0L) {
    .islh_abort(c(
      "Grouping columns use names reserved by the result.",
      x = "Reserved: {.val {reserved}}."
    ))
  }

  work <- as.data.frame(data)
  work$.islh_date <- .islh_surv_as_date(work[[date_name]], date_name)

  if (is.null(from)) {
    if (nrow(work) == 0L) {
      .islh_abort("{.arg from} is required when {.arg data} has no rows.")
    }
    from <- min(work$.islh_date)
  } else {
    from <- .islh_surv_as_date(from, "from", scalar = TRUE)
  }
  if (is.null(to)) {
    if (nrow(work) == 0L) {
      .islh_abort("{.arg to} is required when {.arg data} has no rows.")
    }
    to <- max(work$.islh_date)
  } else {
    to <- .islh_surv_as_date(to, "to", scalar = TRUE)
  }
  if (from > to) {
    .islh_abort("{.arg from} must not be after {.arg to}.")
  }

  selected <- work$.islh_date >= from & work$.islh_date <= to
  work <- work[selected, , drop = FALSE]

  if (length(id_name) > 0L && any(.islh_surv_missing(work[[id_name]]))) {
    n_missing <- sum(.islh_surv_missing(work[[id_name]]))
    .islh_abort(c(
      "{.arg id} contains missing identifiers in the selected date range.",
      x = "Found {n_missing} affected row{?s}.",
      i = "Use {.fn islh_check_events} to identify the records."
    ))
  }
  if (length(count_name) > 0L) {
    work$.islh_count <- .islh_check_counts(
      work[[count_name]],
      count_name,
      allow_na = FALSE
    )
  }

  work$.islh_period_start <- .islh_surv_period_start(
    work$.islh_date,
    interval,
    week_start
  )
  work$.islh_period_end <- .islh_surv_period_end(
    work$.islh_period_start,
    interval,
    week_start
  )

  group_columns <- c(by_names, ".islh_period_start", ".islh_period_end")
  grouped <- dplyr::group_by(
    work,
    dplyr::across(tidyselect::all_of(group_columns))
  )
  if (length(id_name) > 0L) {
    result <- dplyr::summarise(
      grouped,
      count = dplyr::n_distinct(.data[[id_name]]),
      .groups = "drop"
    )
  } else if (length(count_name) > 0L) {
    result <- dplyr::summarise(
      grouped,
      count = sum(.data$.islh_count),
      .groups = "drop"
    )
  } else {
    result <- dplyr::summarise(grouped, count = dplyr::n(), .groups = "drop")
  }
  names(result)[names(result) == ".islh_period_start"] <- "period_start"
  names(result)[names(result) == ".islh_period_end"] <- "period_end"
  result$partial_period <- result$period_end > to

  if (!isTRUE(include_partial)) {
    result <- result[!result$partial_period, , drop = FALSE]
  }

  if (isTRUE(fill)) {
    from_period <- .islh_surv_period_start(from, interval, week_start)
    to_period <- .islh_surv_period_start(to, interval, week_start)
    periods <- .islh_surv_period_sequence(from_period, to_period, interval)
    period_ends <- .islh_surv_period_end(periods, interval, week_start)
    if (!isTRUE(include_partial)) {
      periods <- periods[period_ends <= to]
    }

    group_values <- .islh_surv_groups(groups, by_names, work)
    grid <- .islh_surv_grid(group_values, periods, by_names)
    if (nrow(grid) > 0L) {
      grid$period_end <- .islh_surv_period_end(
        grid$period_start,
        interval,
        week_start
      )
      join_columns <- c(by_names, "period_start", "period_end")
      result <- dplyr::left_join(
        grid,
        result[, c(join_columns, "count"), drop = FALSE],
        by = join_columns,
        relationship = "one-to-one",
        na_matches = "na"
      )
      result$count[is.na(result$count)] <- 0
      result$partial_period <- result$period_end > to
    } else {
      result <- result[FALSE, , drop = FALSE]
    }
  }

  order_columns <- c(by_names, "period_start")
  if (nrow(result) > 0L) {
    result <- result[do.call(order, result[order_columns]), , drop = FALSE]
  }
  result <- result[, c(
    by_names,
    "period_start",
    "period_end",
    "count",
    "partial_period"
  ), drop = FALSE]
  attr(result, "islh_interval") <- interval
  attr(result, "islh_week_start") <- week_start
  result
}

#' Calculate a descriptive surveillance baseline
#'
#' The function expects a complete period table, including real zero-count
#' periods. It calculates transparent reference statistics but does not decide
#' whether an exceedance is an outbreak or requires public-health action.
#'
#' @param data A data frame containing one row per group and reference period.
#' @param date Period date column.
#' @param value Whole-count column.
#' @param by Optional tidy-select specification of grouping columns.
#' @param reference_periods Optional vector of dates to include. When omitted,
#'   every row is used.
#' @param method One of `"mean_sd"`, `"range"` or `"quantile"`.
#' @param multiplier Number of standard deviations used by `"mean_sd"`.
#' @param prediction_adjustment Apply `sqrt(1 + 1 / k)` to the standard
#'   deviation limit, where `k` is the number of reference periods.
#' @param probs Lower and upper probabilities used by `"quantile"`.
#' @param minimum_periods Minimum reference periods required in every group.
#'
#' @return One row per group with the number of reference periods, descriptive
#'   statistics and lower and upper limits.
#'
#' @examples
#' weekly <- data.frame(
#'   site = rep(c("A", "B"), each = 8),
#'   week = rep(seq(as.Date("2026-01-04"), by = "week", length.out = 8), 2),
#'   count = c(2, 4, 3, 5, 2, 4, 3, 5, 6, 7, 5, 8, 6, 7, 5, 8)
#' )
#'
#' islh_surveillance_baseline(
#'   weekly,
#'   date = week,
#'   value = count,
#'   by = site,
#'   method = "mean_sd"
#' )
#'
#' @export
islh_surveillance_baseline <- function(
    data,
    date,
    value,
    by = NULL,
    reference_periods = NULL,
    method = c("mean_sd", "range", "quantile"),
    multiplier = 2,
    prediction_adjustment = TRUE,
    probs = c(0.05, 0.95),
    minimum_periods = 4L) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  method <- match.arg(method)
  prediction_adjustment <- .islh_check_flag(
    prediction_adjustment,
    "prediction_adjustment"
  )
  if (!is.numeric(multiplier) || length(multiplier) != 1L ||
      is.na(multiplier) || !is.finite(multiplier) || multiplier < 0) {
    .islh_abort("{.arg multiplier} must be one non-negative finite number.")
  }
  if (!is.numeric(minimum_periods) || length(minimum_periods) != 1L ||
      is.na(minimum_periods) || minimum_periods != round(minimum_periods) ||
      minimum_periods < 1L) {
    .islh_abort("{.arg minimum_periods} must be one positive whole number.")
  }
  minimum_periods <- as.integer(minimum_periods)
  if (!is.numeric(probs) || length(probs) != 2L || anyNA(probs) ||
      any(!is.finite(probs)) || probs[1] < 0 || probs[2] > 1 ||
      probs[1] >= probs[2]) {
    .islh_abort(
      "{.arg probs} must contain increasing lower and upper probabilities."
    )
  }

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  value_name <- .islh_surv_select(data, rlang::enquo(value), "value", 1L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")

  work <- as.data.frame(data)
  work$.islh_date <- .islh_surv_as_date(work[[date_name]], date_name)
  work$.islh_value <- .islh_check_counts(
    work[[value_name]],
    value_name,
    allow_na = FALSE
  )

  if (!is.null(reference_periods)) {
    reference_periods <- .islh_surv_as_date(
      reference_periods,
      "reference_periods"
    )
    work <- work[work$.islh_date %in% reference_periods, , drop = FALSE]
  }
  if (nrow(work) == 0L) {
    .islh_abort("No rows remain in the requested reference periods.")
  }

  key_columns <- c(by_names, ".islh_date")
  duplicates <- work |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(key_columns))) |>
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$.n > 1L)
  if (nrow(duplicates) > 0L) {
    .islh_abort(c(
      "{.arg data} has more than one value per group and date.",
      x = "Found {nrow(duplicates)} duplicated group-period combination{?s}.",
      i = "Aggregate the data with {.fn islh_count_events} first."
    ))
  }

  grouped <- dplyr::group_by(
    work,
    dplyr::across(tidyselect::all_of(by_names))
  )
  summary <- dplyr::summarise(
    grouped,
    reference_n = dplyr::n(),
    reference_mean = mean(.data$.islh_value),
    reference_sd = stats::sd(.data$.islh_value),
    reference_min = min(.data$.islh_value),
    reference_max = max(.data$.islh_value),
    reference_q_low = as.numeric(stats::quantile(
      .data$.islh_value,
      probs = probs[1],
      names = FALSE,
      type = 7
    )),
    reference_q_high = as.numeric(stats::quantile(
      .data$.islh_value,
      probs = probs[2],
      names = FALSE,
      type = 7
    )),
    .groups = "drop"
  )
  summary$reference_sd[is.na(summary$reference_sd)] <- 0

  insufficient <- summary$reference_n < minimum_periods
  if (any(insufficient)) {
    n_insufficient <- sum(insufficient)
    .islh_abort(c(
      "The surveillance baseline is too short.",
      x = "{n_insufficient} groups have fewer than {minimum_periods} reference periods.",
      i = "Add complete reference periods or lower {.arg minimum_periods} explicitly."
    ))
  }

  adjustment <- if (isTRUE(prediction_adjustment)) {
    sqrt(1 + 1 / summary$reference_n)
  } else {
    rep(1, nrow(summary))
  }
  summary$reference_adjustment <- adjustment
  summary$reference_method <- method

  if (method == "mean_sd") {
    spread <- multiplier * summary$reference_sd * adjustment
    summary$lower_limit <- pmax(0, summary$reference_mean - spread)
    summary$upper_limit <- summary$reference_mean + spread
  } else if (method == "range") {
    summary$lower_limit <- summary$reference_min
    summary$upper_limit <- summary$reference_max
  } else {
    summary$lower_limit <- summary$reference_q_low
    summary$upper_limit <- summary$reference_q_high
  }

  summary[, c(
    by_names,
    "reference_method",
    "reference_n",
    "reference_mean",
    "reference_sd",
    "reference_min",
    "reference_max",
    "reference_adjustment",
    "lower_limit",
    "upper_limit"
  ), drop = FALSE]
}

#' Build a current surveillance snapshot
#'
#' `islh_surveillance_snapshot()` turns complete period counts into the familiar
#' operational table: one row per group, recent periods in columns, a current
#' total and optional historical reference statistics.
#'
#' @param data A complete count table, normally returned by
#'   [islh_count_events()].
#' @param date Period date column.
#' @param value Whole-count column.
#' @param by Optional tidy-select specification of grouping columns.
#' @param end Last period to show. Defaults to the latest date in `data`.
#' @param periods Number of periods to show.
#' @param interval Period spacing used to construct the window.
#' @param week_start Start of an ordinary week, from 1 (Monday) to 7 (Sunday).
#' @param baseline Optional result from [islh_surveillance_baseline()].
#' @param include_total Add an `All` row. This is only available with one
#'   grouping column and should only be used for mutually exclusive groups.
#' @param total_label Label used for the total group.
#'
#' @return A wide data frame containing groups, `total`, optional baseline
#'   fields, `exceeds_reference`, and one column per displayed period.
#'
#' @examples
#' daily <- data.frame(
#'   site = rep(c("A", "B"), each = 7),
#'   date = rep(seq(as.Date("2026-08-01"), by = "day", length.out = 7), 2),
#'   count = c(0, 1, 2, 0, 1, 0, 2, 1, 2, 1, 3, 1, 0, 1)
#' )
#'
#' islh_surveillance_snapshot(
#'   daily,
#'   date = date,
#'   value = count,
#'   by = site,
#'   periods = 7,
#'   include_total = TRUE
#' )
#'
#' @export
islh_surveillance_snapshot <- function(
    data,
    date,
    value,
    by = NULL,
    end = NULL,
    periods = 7L,
    interval = c(
      "day", "week", "isoweek", "epiweek", "month", "quarter", "year"
    ),
    week_start = 1,
    baseline = NULL,
    include_total = FALSE,
    total_label = "All") {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  interval <- .islh_surv_interval(interval)
  week_start <- .islh_surv_week_start(interval, week_start)
  include_total <- .islh_check_flag(include_total, "include_total")
  if (!is.numeric(periods) || length(periods) != 1L || is.na(periods) ||
      periods != round(periods) || periods < 1L) {
    .islh_abort("{.arg periods} must be one positive whole number.")
  }
  periods <- as.integer(periods)
  if (!is.character(total_label) || length(total_label) != 1L ||
      is.na(total_label) || !nzchar(trimws(total_label))) {
    .islh_abort("{.arg total_label} must be one non-empty string.")
  }

  date_name <- .islh_surv_select(data, rlang::enquo(date), "date", 1L, 1L)
  value_name <- .islh_surv_select(data, rlang::enquo(value), "value", 1L, 1L)
  by_names <- .islh_surv_select(data, rlang::enquo(by), "by")
  if (isTRUE(include_total) && length(by_names) != 1L) {
    .islh_abort(
      "{.arg include_total} requires exactly one {.arg by} column."
    )
  }

  work <- as.data.frame(data)
  work$.islh_date <- .islh_surv_as_date(work[[date_name]], date_name)
  work$.islh_value <- .islh_check_counts(
    work[[value_name]],
    value_name,
    allow_na = FALSE
  )
  if (nrow(work) == 0L) {
    .islh_abort("{.arg data} must contain at least one row.")
  }

  if (is.null(end)) {
    end <- max(work$.islh_date)
  } else {
    end <- .islh_surv_as_date(end, "end", scalar = TRUE)
  }
  end <- .islh_surv_period_start(end, interval, week_start)
  target_periods <- .islh_surv_periods_back(end, periods, interval)
  work$.islh_period <- .islh_surv_period_start(
    work$.islh_date,
    interval,
    week_start
  )

  group_columns <- c(by_names, ".islh_period")
  counts <- work |>
    dplyr::filter(.data$.islh_period %in% target_periods) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_columns))) |>
    dplyr::summarise(.islh_value = sum(.data$.islh_value), .groups = "drop")

  group_values <- unique(work[by_names])
  grid <- .islh_surv_grid(group_values, target_periods, by_names)
  names(grid)[names(grid) == "period_start"] <- ".islh_period"
  counts <- dplyr::left_join(
    grid,
    counts,
    by = group_columns,
    relationship = "one-to-one",
    na_matches = "na"
  )
  counts$.islh_value[is.na(counts$.islh_value)] <- 0

  if (isTRUE(include_total)) {
    group_name <- by_names[[1]]
    counts[[group_name]] <- as.character(counts[[group_name]])
    if (total_label %in% counts[[group_name]]) {
      .islh_abort(
        "{.arg total_label} is already present in the grouping column."
      )
    }
    total <- counts |>
      dplyr::group_by(.data$.islh_period) |>
      dplyr::summarise(.islh_value = sum(.data$.islh_value), .groups = "drop")
    total[[group_name]] <- total_label
    total <- total[, c(group_name, ".islh_period", ".islh_value")]
    counts <- dplyr::bind_rows(counts, total)
  }

  counts$.islh_label <- format(counts$.islh_period, "%Y-%m-%d")
  wide <- counts |>
    dplyr::select(
      tidyselect::all_of(by_names),
      .data$.islh_label,
      .data$.islh_value
    ) |>
    tidyr::pivot_wider(
      names_from = ".islh_label",
      values_from = ".islh_value",
      values_fill = 0,
      names_sort = TRUE
    )

  date_columns <- format(target_periods, "%Y-%m-%d")
  wide$total <- rowSums(wide[, date_columns, drop = FALSE])

  reference_columns <- character()
  if (!is.null(baseline)) {
    if (!is.data.frame(baseline)) {
      .islh_abort("{.arg baseline} must be a data frame or NULL.")
    }
    missing <- setdiff(by_names, names(baseline))
    if (length(missing) > 0L) {
      .islh_abort(c(
        "{.arg baseline} is missing grouping columns.",
        x = "Missing: {.val {missing}}."
      ))
    }
    reference_columns <- setdiff(names(baseline), by_names)
    if (length(by_names) == 0L) {
      if (nrow(baseline) != 1L) {
        .islh_abort(
          "An ungrouped {.arg baseline} must contain exactly one row."
        )
      }
      wide <- cbind(wide, baseline[rep(1L, nrow(wide)), , drop = FALSE])
    } else {
      for (field in by_names) {
        baseline[[field]] <- as.character(baseline[[field]])
        wide[[field]] <- as.character(wide[[field]])
      }
      duplicate <- duplicated(baseline[by_names])
      if (any(duplicate)) {
        .islh_abort("{.arg baseline} must contain one row per group.")
      }
      wide <- dplyr::left_join(
        wide,
        baseline,
        by = by_names,
        relationship = "many-to-one",
        na_matches = "na"
      )
    }
  }

  if ("upper_limit" %in% names(wide)) {
    wide$exceeds_reference <- ifelse(
      is.na(wide$upper_limit),
      NA,
      wide$total >= wide$upper_limit
    )
  }

  output_columns <- c(by_names, "total", reference_columns)
  if ("exceeds_reference" %in% names(wide)) {
    output_columns <- c(output_columns, "exceeds_reference")
  }
  output_columns <- unique(c(output_columns, date_columns))
  wide[, output_columns, drop = FALSE]
}
