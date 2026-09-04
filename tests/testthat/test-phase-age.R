test_that("five-year bands are closed at the top by an open band", {
  groups <- islh_age_group(c(0, 4, 5, 84, 85, 103))
  expect_equal(as.character(groups),
               c("0-4", "0-4", "5-9", "80-84", "85+", "85+"))
  expect_s3_class(groups, "ordered")
  expect_equal(nlevels(groups), 18L)
})

test_that("bands are left-closed, so a part-year age stays in its band", {
  # Someone aged 4.9 has not had their fifth birthday.
  expect_equal(as.character(islh_age_group(4.9)), "0-4")
  expect_equal(as.character(islh_age_group(5.0)), "5-9")
})

test_that("the published band sets have the shapes they are named for", {
  # PHAC bands start with infants separated out.
  phac <- islh_age_group(c(0, 0.5, 1, 4, 5, 82), breaks = "phac")
  expect_equal(as.character(phac),
               c("0", "0", "1-4", "1-4", "5-9", "80+"))

  broad <- islh_age_group(c(2, 19, 20, 64, 65, 90), breaks = "broad")
  expect_equal(as.character(broad),
               c("0-19", "0-19", "20-64", "20-64", "65+", "65+"))
})

test_that("a local band set can be supplied without changing the package", {
  groups <- islh_age_group(c(2, 17, 18, 42, 91), breaks = c(0, 18, 65))
  expect_equal(as.character(groups),
               c("0-17", "0-17", "18-64", "18-64", "65+"))
})

test_that("custom labels are used in order", {
  groups <- islh_age_group(
    c(2, 42, 91),
    breaks = "broad",
    labels = c("Child", "Adult", "Senior")
  )
  expect_equal(as.character(groups), c("Child", "Adult", "Senior"))
})

test_that("missing and impossible ages come back as NA", {
  groups <- islh_age_group(c(NA, -1, 30))
  expect_true(is.na(groups[1]))
  expect_true(is.na(groups[2]))
  expect_equal(as.character(groups[3]), "30-34")
})

test_that("bad arguments are rejected", {
  expect_error(islh_age_group("40"), "must be numeric")
  expect_error(islh_age_group(40, breaks = "nonsense"), "five_year")
  expect_error(islh_age_group(40, breaks = c(5, 10)), "must start at 0")
  expect_error(
    islh_age_group(40, breaks = "broad", labels = c("a", "b")),
    "one entry per band"
  )
})

test_that("grouping composes with the rate helpers", {
  # The reason these live in the same package: bands feed a standardised rate.
  set.seed(1)
  ages <- sample(0:95, 500, replace = TRUE)
  bands <- islh_age_group(ages, breaks = "broad")

  population <- as.numeric(table(bands))
  cases <- c(2, 15, 30)
  standard <- c(40000, 40000, 20000)

  out <- islh_dsr(cases, population, standard)
  expect_equal(nrow(out), 1L)
  expect_true(out$rate > 0)
  expect_true(out$lower <= out$rate && out$rate <= out$upper)
})


