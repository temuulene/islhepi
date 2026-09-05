test_that("islh_suppress_table hides small cells in the named columns only", {
  counts <- data.frame(
    area = c("North", "Central", "South"),
    cases = c(3, 42, 17),
    contacts = c(1, 55, 4)
  )

  out <- islh_suppress_table(counts, c("cases", "contacts"), threshold = 5)

  expect_true(is.na(out$cases[1]))
  expect_equal(out$cases[2:3], c(42, 17))
  expect_true(all(is.na(out$contacts[c(1, 3)])))
  expect_equal(out$contacts[2], 55)

  # Untouched columns keep their values and their type.
  expect_equal(out$area, counts$area)
})

test_that("a threshold must be supplied", {
  counts <- data.frame(cases = c(1, 20))
  expect_error(islh_suppress_table(counts, "cases"), "must be supplied")
})

test_that("zero is not a small cell", {
  # A zero is a real, publishable finding: nobody had the thing. Suppressing it
  # would hide information without protecting anyone.
  out <- islh_suppress_table(
    data.frame(cases = c(0, 3, 9)), "cases", threshold = 5
  )
  expect_equal(out$cases[1], 0)
  expect_true(is.na(out$cases[2]))
  expect_equal(out$cases[3], 9)
})

test_that("inclusive controls whether the threshold itself is suppressed", {
  counts <- data.frame(cases = c(4, 5, 6))

  inclusive <- islh_suppress_table(counts, "cases", threshold = 5)
  expect_equal(is.na(inclusive$cases), c(TRUE, TRUE, FALSE))

  exclusive <- islh_suppress_table(
    counts, "cases", threshold = 5, inclusive = FALSE
  )
  expect_equal(is.na(exclusive$cases), c(TRUE, FALSE, FALSE))
})

test_that("complementary suppression hides a second cell when one is recoverable", {
  # With a total in view, a single suppressed cell is recoverable by
  # subtraction, so a second must go.
  counts <- data.frame(cases = c(3, 42, 17))

  plain <- islh_suppress_table(counts, "cases", threshold = 5)
  expect_equal(sum(is.na(plain$cases)), 1L)

  complementary <- islh_suppress_table(
    counts, "cases", threshold = 5, complementary = TRUE
  )
  expect_equal(sum(is.na(complementary$cases)), 2L)
  # The smallest survivor is the one that goes.
  expect_true(is.na(complementary$cases[3]))
  expect_equal(complementary$cases[2], 42)
})

test_that("complementary suppression does nothing when the sum is already safe", {
  # Two cells already suppressed cannot be recovered by subtraction, so no
  # third cell should be hidden.
  counts <- data.frame(cases = c(3, 42, 2))
  out <- islh_suppress_table(
    counts, "cases", threshold = 5, complementary = TRUE
  )
  expect_equal(sum(is.na(out$cases)), 2L)
  expect_equal(out$cases[2], 42)

  # And nothing suppressed means nothing to protect.
  safe <- data.frame(cases = c(30, 42, 17))
  untouched <- islh_suppress_table(
    safe, "cases", threshold = 5, complementary = TRUE
  )
  expect_equal(untouched$cases, safe$cases)
})

test_that("a label turns the column into readable text", {
  out <- islh_suppress_table(
    data.frame(cases = c(3, 42)), "cases", threshold = 5,
    inclusive = FALSE, label = "<5"
  )
  expect_type(out$cases, "character")
  expect_equal(out$cases, c("<5", "42"))
})

test_that("unknown columns are named in the error", {
  expect_error(
    islh_suppress_table(data.frame(a = 1), "nope", threshold = 5),
    "nope"
  )
})

test_that("nearest rounding goes to the closest multiple of the base", {
  expect_equal(islh_round_base(c(0, 2, 3, 7, 12, 43), base = 5),
               c(0, 0, 5, 5, 10, 45))
  expect_equal(islh_round_base(c(4, 6, 14, 15), base = 10),
               c(0, 10, 10, 20))
  expect_true(is.na(islh_round_base(NA_real_, base = 5)))
})

test_that("random rounding lands on a neighbouring multiple and is unbiased", {
  withr::local_seed(42)

  x <- rep(12, 4000)
  rounded <- islh_round_base(x, base = 5, method = "random")

  # Every value must be one of the two neighbouring multiples.
  expect_setequal(unique(rounded), c(10, 15))

  # 12 sits 2/5 of the way from 10 to 15, so it should round up about 40% of
  # the time and the mean should come back to 12.
  expect_equal(mean(rounded == 15), 0.4, tolerance = 0.05)
  expect_equal(mean(rounded), 12, tolerance = 0.15)

  # An exact multiple never moves.
  expect_equal(islh_round_base(rep(10, 50), base = 5, method = "random"),
               rep(10, 50))
})

test_that("islh_round_base rejects a nonsensical base", {
  expect_error(islh_round_base(1:5, base = 0), "positive")
  expect_error(islh_round_base("a", base = 5), "must be counts")

  # The base is a policy decision, like the threshold, so it has no default.
  expect_error(islh_round_base(c(2, 7)), "must be supplied")
})

# Regression tests for the code review of v0.1.0.

test_that("a complementary cell never claims to be small", {
  # The bug: with label = "<5", the complementary cell (17) was displayed as
  # "<5" — a false statement about the data, in a published table.
  counts <- data.frame(cases = c(3, 42, 17))

  out <- islh_suppress_table(
    counts, "cases", threshold = 5, inclusive = FALSE,
    complementary = TRUE, label = "<5"
  )

  expect_equal(out$cases[1], "<5")        # genuinely small
  expect_equal(out$cases[2], "42")        # untouched
  expect_false(out$cases[3] == "<5")      # hidden to protect the first
  expect_equal(out$cases[3], "Suppressed")
})

test_that("the output type is decided by label alone", {
  counts <- data.frame(cases = c(3, 42, 17))

  # No label: numeric with NA, whether or not a complementary cell is hidden.
  plain <- islh_suppress_table(counts, "cases", threshold = 5)
  expect_type(plain$cases, "double")

  comp <- islh_suppress_table(
    counts, "cases", threshold = 5, complementary = TRUE
  )
  expect_type(comp$cases, "double")
  expect_equal(sum(is.na(comp$cases)), 2L)

  # Nothing suppressed at all must not change the column's type.
  safe <- islh_suppress_table(
    data.frame(cases = c(30, 42, 17)), "cases",
    threshold = 5, complementary = TRUE
  )
  expect_type(safe$cases, "double")
  expect_equal(safe$cases, c(30, 42, 17))
})

test_that("factors are refused rather than read as level codes", {
  # as.numeric() on a factor gives the level codes. factor(c("10","3","42"))
  # would have been read as 1, 2, 3 and suppressed entirely.
  expect_error(
    islh_suppress(factor(c("10", "3", "42")), threshold = 5),
    "factor"
  )
  expect_error(
    islh_suppress_table(
      data.frame(cases = factor(c("10", "3", "42"))), "cases", threshold = 5
    ),
    "factor"
  )
})

test_that("counts must be whole, non-negative and finite", {
  expect_error(islh_suppress(c(1, 2.7), threshold = 5), "whole counts")
  expect_error(islh_suppress(c(-2, 3), threshold = 5), "not be negative")
  expect_error(islh_suppress(c(1, Inf), threshold = 5), "finite")
  expect_error(islh_suppress(TRUE, threshold = 5), "must be numeric")

  # Text that is a number is still a count.
  expect_equal(islh_suppress(c("3", "42"), threshold = 5), c(NA, 42))
  expect_error(islh_suppress(c("3", "many"), threshold = 5), "not")
})

test_that("the threshold itself is validated", {
  expect_error(islh_suppress(1:5, threshold = -1), "non-negative")
  expect_error(islh_suppress(1:5, threshold = Inf), "finite")
  expect_error(islh_suppress(1:5, threshold = c(5, 10)), "one non-negative")
})

test_that("numeric suppression labels must describe the boundary truthfully", {
  expect_error(
    islh_suppress(5, threshold = 5, label = "<5"),
    "does not describe every count"
  )
  expect_error(
    islh_suppress_table(
      data.frame(cases = 5), "cases", threshold = 5, label = "<5"
    ),
    "does not describe every count"
  )

  expect_equal(
    islh_suppress(c(4, 5, 6), threshold = 5, label = "<=5"),
    c("<=5", "<=5", "6")
  )
  expect_equal(
    islh_suppress(
      c(4, 5, 6), threshold = 5, inclusive = FALSE, label = "<5"
    ),
    c("<5", "5", "6")
  )
})

test_that("disclosure-control switches fail closed", {
  counts <- data.frame(cases = c(3, 17, 42))

  expect_error(
    islh_suppress(3, threshold = 5, inclusive = NA),
    "single TRUE or FALSE"
  )
  expect_error(
    islh_suppress_table(
      counts, "cases", threshold = 5, complementary = NA
    ),
    "single TRUE or FALSE"
  )
  expect_error(
    islh_suppress_table(
      counts, "cases", threshold = 5, complementary = 1
    ),
    "single TRUE or FALSE"
  )
})

test_that("suppression labels are scalar and complementary labels are neutral", {
  counts <- data.frame(cases = c(3, 17, 42))

  expect_error(
    islh_suppress(3, threshold = 5, label = c("<5", "Suppressed")),
    "one non-missing character string"
  )
  expect_error(
    islh_suppress_table(
      counts, "cases", threshold = 5, inclusive = FALSE,
      label = "<5", complementary = TRUE, complementary_label = "<5"
    ),
    "must not state a numeric bound"
  )
})



# Column selection -----------------------------------------------------------

suppress_frame <- function() {
  data.frame(
    authority = c("Island", "Island", "Interior", "Interior"),
    area = c("North", "South", "East", "West"),
    cases = c(2, 30, 4, 25),
    contacts = c(1, 12, 3, 40),
    stringsAsFactors = FALSE
  )
}

test_that("cols rejects selections that would silently pick another column", {
  data <- suppress_frame()

  # 1.5 truncates to column 1, which is `authority`.
  expect_error(
    islh_suppress_table(data, 1.5, threshold = 5),
    "whole column positions"
  )
  # TRUE is column 1 as well, not "every column".
  expect_error(
    islh_suppress_table(data, TRUE, threshold = 5),
    "is logical"
  )
  # A factor indexes by level code, so this is column 1 too.
  expect_error(
    islh_suppress_table(data, factor("cases"), threshold = 5),
    "is a factor"
  )
})

test_that("cols rejects empty, duplicated and out-of-range selections", {
  data <- suppress_frame()

  expect_error(islh_suppress_table(data, character(0), threshold = 5), "empty")
  expect_error(islh_suppress_table(data, integer(0), threshold = 5), "empty")
  expect_error(
    islh_suppress_table(data, c("cases", "cases"), threshold = 5),
    "more than once"
  )
  expect_error(
    islh_suppress_table(data, c(3, 3), threshold = 5),
    "more than once"
  )
  expect_error(islh_suppress_table(data, 0, threshold = 5), "within")
  expect_error(islh_suppress_table(data, 99, threshold = 5), "within")
  expect_error(islh_suppress_table(data, -1, threshold = 5), "within")
  expect_error(
    islh_suppress_table(data, NA_character_, threshold = 5),
    "missing values"
  )
  expect_error(islh_suppress_table(data, NA_integer_, threshold = 5), "missing")
  expect_error(islh_suppress_table(data, NULL, threshold = 5), "must name")
  expect_error(islh_suppress_table(data, list("cases"), threshold = 5), "names or positions")
})

test_that("valid positions and names select the same columns", {
  data <- suppress_frame()
  by_name <- islh_suppress_table(data, c("cases", "contacts"), threshold = 5)
  by_position <- islh_suppress_table(data, c(3, 4), threshold = 5)
  expect_equal(by_name, by_position)
})

# Group-specific complementary suppression -----------------------------------

test_that("complementary suppression runs within each group", {
  data <- suppress_frame()

  # Across the whole column two cells are small, so the column-wide rule sees
  # no recoverable cell and stops. Within each authority there is exactly one,
  # and a reader subtracts from that authority's subtotal to recover it.
  ungrouped <- islh_suppress_table(
    data, "cases", threshold = 5,
    complementary = TRUE, label = "Suppressed"
  )
  expect_equal(ungrouped$cases, c("Suppressed", "30", "Suppressed", "25"))

  grouped <- islh_suppress_table(
    data, "cases", threshold = 5,
    complementary = TRUE, by = "authority", label = "Suppressed"
  )
  expect_equal(
    grouped$cases,
    c("Suppressed", "Suppressed", "Suppressed", "Suppressed")
  )
})

test_that("grouping accepts several columns and leaves other groups alone", {
  data <- data.frame(
    year = c(2025, 2025, 2026, 2026),
    authority = c("Island", "Island", "Island", "Island"),
    cases = c(3, 40, 20, 30),
    stringsAsFactors = FALSE
  )

  out <- islh_suppress_table(
    data, "cases", threshold = 5,
    complementary = TRUE, by = c("year", "authority"), label = "Suppressed"
  )

  # 2025 has one small cell so its larger partner goes too. 2026 has none, so
  # both of its cells stay.
  expect_equal(out$cases, c("Suppressed", "Suppressed", "20", "30"))
})

test_that("by is validated against data and cols", {
  data <- suppress_frame()

  expect_error(
    islh_suppress_table(data, "cases", threshold = 5, by = "missing"),
    "no column"
  )
  expect_error(
    islh_suppress_table(data, "cases", threshold = 5, by = "cases"),
    "share"
  )
  expect_error(
    islh_suppress_table(data, "cases", threshold = 5, by = c("area", "area")),
    "more than once"
  )
  expect_error(
    islh_suppress_table(data, "cases", threshold = 5, by = 1),
    "NULL or column names"
  )
})

# Audit record ---------------------------------------------------------------

test_that("the audit record names every hidden cell and why", {
  data <- suppress_frame()
  out <- islh_suppress_table(
    data, "cases", threshold = 5,
    complementary = TRUE, by = "authority", label = "Suppressed"
  )
  audit <- islh_suppression_audit(out)

  expect_equal(names(audit), c("column", "row", "group", "reason"))
  expect_equal(nrow(audit), 4L)
  expect_equal(audit$row, c(1L, 2L, 3L, 4L))
  expect_equal(audit$group, c("Island", "Island", "Interior", "Interior"))
  expect_equal(
    audit$reason,
    c("small", "complementary", "small", "complementary")
  )
})

test_that("the audit record holds no counts", {
  data <- suppress_frame()
  out <- islh_suppress_table(data, "cases", threshold = 5)
  audit <- islh_suppression_audit(out)

  # Carrying the hidden values would defeat the suppression the moment the
  # object was shared.
  expect_false(any(c("value", "count", "n") %in% names(audit)))
  expect_true(all(vapply(audit[c("column", "group", "reason")], is.character, logical(1))))
})

test_that("a table with nothing hidden gives a zero-row audit", {
  data <- data.frame(area = c("North", "South"), cases = c(20, 30))
  out <- islh_suppress_table(data, "cases", threshold = 5)

  audit <- islh_suppression_audit(out)
  expect_equal(nrow(audit), 0L)
  expect_equal(names(audit), c("column", "row", "group", "reason"))
})

test_that("the audit covers every suppressed column", {
  data <- suppress_frame()
  out <- islh_suppress_table(data, c("cases", "contacts"), threshold = 5)
  audit <- islh_suppression_audit(out)

  expect_equal(sort(unique(audit$column)), c("cases", "contacts"))
  expect_equal(audit$row[audit$column == "cases"], c(1L, 3L))
  expect_equal(audit$row[audit$column == "contacts"], c(1L, 3L))
})

test_that("reading an audit from an unsuppressed table is an error", {
  expect_error(
    islh_suppression_audit(data.frame(x = 1)),
    "no suppression record"
  )
  expect_error(islh_suppression_audit("not a data frame"), "data frame")
})
