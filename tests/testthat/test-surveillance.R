test_that("event checks return affected records", {
  data <- data.frame(
    id = c("A", "A", "", "D", "E"),
    date = c("2026-01-01", "2026-01-02", NA, "bad", "2027-01-01"),
    condition = c("Flu", "Flu", "COVID", "", "Flu")
  )

  issues <- islh_check_events(
    data,
    id = id,
    date = date,
    required = condition,
    max_date = as.Date("2026-12-31")
  )

  expect_s3_class(issues, "data.frame")
  expect_true(all(c(".row", ".id", ".issue", ".field", ".value") %in% names(issues)))
  expect_equal(sum(issues$.issue == "duplicate_id"), 2)
  expect_equal(sum(issues$.issue == "missing_id"), 1)
  expect_equal(sum(issues$.issue == "missing_date"), 1)
  expect_equal(sum(issues$.issue == "invalid_date"), 1)
  expect_equal(sum(issues$.issue == "date_after_maximum"), 1)
  expect_equal(sum(issues$.issue == "missing_required"), 1)
})

test_that("valid event data returns an empty issue table", {
  data <- data.frame(
    id = c("A", "B"),
    date = as.Date(c("2026-01-01", "2026-01-02"))
  )
  issues <- islh_check_events(data, id, date, max_date = NULL)
  expect_equal(nrow(issues), 0)
  expect_identical(
    names(issues),
    c(".row", ".id", ".issue", ".field", ".value")
  )
})

test_that("event checks support POSIX dates and date limits", {
  data <- data.frame(
    id = c("A", "B"),
    date = as.POSIXct(c("2026-01-01", "2026-01-03"), tz = "UTC")
  )
  issues <- islh_check_events(
    data,
    id,
    date,
    min_date = "2026-01-02",
    max_date = NULL
  )
  expect_equal(issues$.issue, "date_before_minimum")
  expect_equal(issues$.row, 1L)
})

test_that("event checks validate their selectors and flags", {
  data <- data.frame(id = 1, date = as.Date("2026-01-01"))
  expect_error(islh_check_events(data, missing, date), "select")
  expect_error(islh_check_events(data, id, date, one_row_per_id = NA), "TRUE or FALSE")
})

test_that("count events fills missing daily groups and dates", {
  data <- data.frame(
    id = 1:4,
    date = as.Date("2026-01-01") + c(0, 0, 2, 2),
    area = c("A", "A", "A", "B")
  )

  out <- islh_count_events(
    data,
    date,
    id = id,
    by = area,
    interval = "day",
    from = "2026-01-01",
    to = "2026-01-03",
    fill = TRUE,
    groups = c("A", "B")
  )

  expect_equal(nrow(out), 6)
  expect_equal(out$count[out$area == "A"], c(2, 0, 1))
  expect_equal(out$count[out$area == "B"], c(0, 0, 1))
  expect_false(any(out$partial_period))
})

test_that("count events counts distinct identifiers", {
  data <- data.frame(
    id = c("A", "A", "B"),
    date = as.Date(c("2026-01-01", "2026-01-01", "2026-01-01"))
  )
  out <- islh_count_events(data, date, id = id, interval = "day")
  expect_equal(out$count, 2)
})

test_that("count events sums aggregated counts", {
  data <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-02")),
    n = c(3, 4)
  )
  out <- islh_count_events(
    data,
    date,
    count = n,
    interval = "week",
    week_start = 1,
    include_partial = TRUE,
    fill = FALSE
  )
  expect_equal(out$count, 7)
  expect_true(out$partial_period)
})

test_that("count events handles epidemiological year boundaries", {
  data <- data.frame(
    date = as.Date(c("2025-12-27", "2025-12-28", "2026-01-03"))
  )
  out <- islh_count_events(
    data,
    date,
    interval = "epiweek",
    from = "2025-12-27",
    to = "2026-01-03",
    include_partial = TRUE,
    fill = FALSE
  )
  expect_equal(out$period_start, as.Date(c("2025-12-21", "2025-12-28")))
  expect_equal(out$count, c(1, 2))
  expect_equal(out$period_end, as.Date(c("2025-12-27", "2026-01-03")))
})

test_that("count events drops partial periods by default", {
  data <- data.frame(date = as.Date(c("2026-01-05", "2026-01-12")))
  out <- islh_count_events(
    data,
    date,
    interval = "week",
    week_start = 1,
    from = "2026-01-05",
    to = "2026-01-14",
    fill = TRUE
  )
  expect_equal(out$period_start, as.Date("2026-01-05"))
  expect_equal(out$count, 1)
})

test_that("count events rejects unsafe inputs", {
  data <- data.frame(
    id = c("A", NA),
    date = c("2026-01-01", "2026-01-02"),
    n = c(1, 1.5)
  )
  expect_error(islh_count_events(data, date, id = id), "missing identifiers")
  expect_error(islh_count_events(data, date, count = n), "whole counts")
  expect_error(islh_count_events(data, date, id = id, count = n), "only one")
  expect_error(
    islh_count_events(data, date, interval = "week", week_start = 0),
    "1 to 7"
  )
})

test_that("baseline computes adjusted mean-SD limits", {
  data <- data.frame(
    site = rep(c("A", "B"), each = 4),
    date = rep(as.Date("2026-01-01") + 0:3, 2),
    count = c(1, 2, 3, 4, 2, 2, 2, 2)
  )
  out <- islh_surveillance_baseline(
    data,
    date,
    count,
    by = site,
    multiplier = 2,
    minimum_periods = 4
  )
  a <- out[out$site == "A", ]
  expect_equal(a$reference_n, 4)
  expect_equal(a$reference_mean, 2.5)
  expect_equal(a$reference_adjustment, sqrt(1.25))
  expect_equal(
    a$upper_limit,
    mean(1:4) + 2 * stats::sd(1:4) * sqrt(1.25)
  )
  expect_equal(out$reference_method, c("mean_sd", "mean_sd"))
})

test_that("baseline supports ranges, quantiles and selected dates", {
  data <- data.frame(
    date = as.Date("2026-01-01") + 0:7,
    count = 1:8
  )
  range <- islh_surveillance_baseline(
    data,
    date,
    count,
    reference_periods = data$date[1:4],
    method = "range"
  )
  expect_equal(range$lower_limit, 1)
  expect_equal(range$upper_limit, 4)

  quantile <- islh_surveillance_baseline(
    data,
    date,
    count,
    method = "quantile",
    probs = c(0.25, 0.75)
  )
  expect_equal(quantile$lower_limit, stats::quantile(1:8, 0.25, names = FALSE))
  expect_equal(quantile$upper_limit, stats::quantile(1:8, 0.75, names = FALSE))
})

test_that("baseline rejects duplicate and short histories", {
  duplicate <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-01", "2026-01-02", "2026-01-03")),
    count = 1:4
  )
  expect_error(
    islh_surveillance_baseline(duplicate, date, count),
    "more than one value"
  )

  short <- data.frame(date = as.Date("2026-01-01") + 0:2, count = 1:3)
  expect_error(
    islh_surveillance_baseline(short, date, count),
    "too short"
  )
})

test_that("snapshot fills dates, totals rows and joins baselines", {
  daily <- data.frame(
    site = rep(c("A", "B"), each = 7),
    date = rep(as.Date("2026-08-01") + 0:6, 2),
    count = c(0, 1, 2, 0, 1, 0, 2, 1, 2, 1, 3, 1, 0, 1)
  )
  baseline <- data.frame(
    site = c("A", "B", "All"),
    reference_n = 8L,
    upper_limit = c(5, 20, 30)
  )
  out <- islh_surveillance_snapshot(
    daily,
    date,
    count,
    by = site,
    periods = 7,
    baseline = baseline,
    baseline_interval = "week",
    include_total = TRUE
  )

  expect_equal(out$site, c("A", "B", "All"))
  expect_equal(out$total, c(6, 9, 15))
  expect_equal(out$exceeds_reference, c(TRUE, FALSE, FALSE))
  expect_true(all(format(as.Date("2026-08-01") + 0:6) %in% names(out)))
})

test_that("snapshot constructs globally missing periods", {
  daily <- data.frame(
    site = c("A", "A"),
    date = as.Date(c("2026-08-01", "2026-08-03")),
    count = c(2, 4)
  )
  out <- islh_surveillance_snapshot(
    daily,
    date,
    count,
    by = site,
    end = "2026-08-03",
    periods = 3
  )
  expect_equal(out[["2026-08-02"]], 0)
  expect_equal(out$total, 6)
})

test_that("snapshot prevents ambiguous totals and baselines", {
  daily <- data.frame(
    site = "A",
    area = "North",
    date = as.Date("2026-08-01"),
    count = 1
  )
  expect_error(
    islh_surveillance_snapshot(
      daily,
      date,
      count,
      by = c(site, area),
      include_total = TRUE
    ),
    "exactly one"
  )

  baseline <- data.frame(site = c("A", "A"), upper_limit = c(2, 3))
  expect_error(
    islh_surveillance_snapshot(
      daily,
      date,
      count,
      by = site,
      baseline = baseline,
      baseline_interval = "week"
    ),
    "one row per group"
  )
})

# Reporting-period metadata --------------------------------------------------

surv_daily <- function(n = 28, start = "2026-06-01") {
  data.frame(
    site = rep(c("A", "B"), each = n),
    date = rep(seq(as.Date(start), by = "day", length.out = n), 2),
    count = rep(c(1, 2, 0, 3, 2, 1, 4), length.out = 2 * n)
  )
}

test_that("count_events records the interval and window it was built with", {
  counts <- islh_count_events(
    surv_daily(),
    date = date,
    by = site,
    count = count,
    interval = "week",
    week_start = 1
  )

  expect_equal(attr(counts, "islh_interval"), "week")
  expect_equal(attr(counts, "islh_week_start"), 1L)
  expect_equal(attr(counts, "islh_periods"), 1L)
  expect_equal(attr(counts, "islh_from"), as.Date("2026-06-01"))
  expect_equal(attr(counts, "islh_to"), as.Date("2026-06-28"))
})

test_that("a baseline carries the duration its limits describe", {
  weekly <- islh_count_events(
    surv_daily(),
    date = date,
    by = site,
    count = count,
    interval = "week"
  )
  baseline <- islh_surveillance_baseline(weekly, period_start, count, by = site)

  expect_equal(attr(baseline, "islh_interval"), "week")
  expect_equal(attr(baseline, "islh_periods"), 1L)
})

test_that("a baseline infers its interval from date spacing alone", {
  # A table built by hand carries no metadata, but its dates are still evenly
  # spaced, so the reporting period is recoverable.
  weekly <- data.frame(
    week = seq(as.Date("2026-01-05"), by = "week", length.out = 8),
    count = c(2, 4, 3, 5, 2, 4, 3, 5)
  )
  baseline <- islh_surveillance_baseline(weekly, week, count)
  expect_equal(attr(baseline, "islh_interval"), "week")

  monthly <- data.frame(
    month = seq(as.Date("2026-01-01"), by = "month", length.out = 8),
    count = c(2, 4, 3, 5, 2, 4, 3, 5)
  )
  expect_equal(
    attr(islh_surveillance_baseline(monthly, month, count), "islh_interval"),
    "month"
  )
})

test_that("an explicit baseline interval must agree with the recorded one", {
  weekly <- islh_count_events(
    surv_daily(),
    date = date,
    by = site,
    count = count,
    interval = "week"
  )

  expect_error(
    islh_surveillance_baseline(
      weekly, period_start, count, by = site, interval = "day"
    ),
    "does not match the reporting period"
  )
  expect_silent(
    islh_surveillance_baseline(
      weekly, period_start, count, by = site, interval = "isoweek"
    )
  )
})

test_that("a daily baseline is refused for a seven-day snapshot", {
  daily <- islh_count_events(
    surv_daily(),
    date = date,
    by = site,
    count = count,
    interval = "day"
  )
  daily_baseline <- islh_surveillance_baseline(
    daily, period_start, count, by = site
  )

  # The failure the metadata exists to catch: seven daily columns total one
  # week, so limits describing a single day are seven times too small.
  expect_error(
    islh_surveillance_snapshot(
      daily, period_start, count,
      by = site, periods = 7, baseline = daily_baseline
    ),
    "different reporting period"
  )
})

test_that("a weekly baseline fits a seven-day snapshot", {
  events <- surv_daily()
  daily <- islh_count_events(
    events, date = date, by = site, count = count, interval = "day"
  )
  weekly <- islh_count_events(
    events, date = date, by = site, count = count, interval = "week"
  )
  weekly_baseline <- islh_surveillance_baseline(
    weekly, period_start, count, by = site
  )

  out <- islh_surveillance_snapshot(
    daily, period_start, count,
    by = site, periods = 7, baseline = weekly_baseline
  )

  expect_true("exceeds_reference" %in% names(out))
  expect_equal(attr(out, "islh_interval"), "day")
  expect_equal(attr(out, "islh_periods"), 7L)
})

test_that("weekly counts cannot be shown as a daily snapshot", {
  weekly <- islh_count_events(
    surv_daily(),
    date = date,
    by = site,
    count = count,
    interval = "week"
  )

  expect_error(
    islh_surveillance_snapshot(
      weekly, period_start, count, by = site, interval = "day", periods = 7
    ),
    "cannot be summarised"
  )
})

test_that("a baseline of unknown duration is refused, and can be declared", {
  daily <- islh_count_events(
    surv_daily(),
    date = date,
    by = site,
    count = count,
    interval = "day"
  )
  hand_built <- data.frame(site = c("A", "B"), upper_limit = c(10, 12))

  expect_error(
    islh_surveillance_snapshot(
      daily, period_start, count, by = site, periods = 7, baseline = hand_built
    ),
    "does not record the duration"
  )

  out <- islh_surveillance_snapshot(
    daily, period_start, count,
    by = site, periods = 7,
    baseline = hand_built, baseline_interval = "week"
  )
  expect_true("exceeds_reference" %in% names(out))

  expect_error(
    islh_surveillance_snapshot(
      daily, period_start, count,
      by = site, periods = 7,
      baseline = hand_built, baseline_interval = "day"
    ),
    "different reporting period"
  )
})

test_that("a declared baseline interval cannot contradict a recorded one", {
  events <- surv_daily()
  daily <- islh_count_events(
    events, date = date, by = site, count = count, interval = "day"
  )
  weekly <- islh_count_events(
    events, date = date, by = site, count = count, interval = "week"
  )
  baseline <- islh_surveillance_baseline(weekly, period_start, count, by = site)

  expect_error(
    islh_surveillance_snapshot(
      daily, period_start, count,
      by = site, periods = 7,
      baseline = baseline, baseline_interval = "day"
    ),
    "contradicts"
  )
})

# Alert boundary -------------------------------------------------------------

test_that("a total exactly equal to the upper limit is flagged", {
  # The documented policy is at-or-above, not strictly above. This matters for
  # method = "range", where the limit is a whole historical maximum that a
  # current total lands on regularly.
  daily <- data.frame(
    site = c("A", "B", "C"),
    date = as.Date("2026-08-01"),
    count = c(9, 10, 11)
  )
  baseline <- data.frame(
    site = c("A", "B", "C"),
    upper_limit = c(10, 10, 10)
  )

  out <- islh_surveillance_snapshot(
    daily, date, count,
    by = site, periods = 1,
    baseline = baseline, baseline_interval = "day"
  )

  expect_equal(out$total, c(9, 10, 11))
  expect_equal(out$exceeds_reference, c(FALSE, TRUE, TRUE))
})

test_that("a group with no upper limit gets a missing flag, not FALSE", {
  daily <- data.frame(
    site = c("A", "B"),
    date = as.Date("2026-08-01"),
    count = c(4, 4)
  )
  baseline <- data.frame(site = c("A", "B"), upper_limit = c(2, NA))

  out <- islh_surveillance_snapshot(
    daily, date, count,
    by = site, periods = 1,
    baseline = baseline, baseline_interval = "day"
  )
  expect_equal(out$exceeds_reference, c(TRUE, NA))
})
