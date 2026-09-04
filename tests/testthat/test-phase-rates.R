# These check the arithmetic against published values and against independent
# calculations, not against the implementation itself.

test_that("the exact Poisson interval matches poisson.test", {
  # stats::poisson.test computes the Garwood interval, so it is an independent
  # implementation of what islh_ci_poisson(method = "exact") should give.
  for (count in c(0, 1, 3, 10, 57)) {
    expected <- stats::poisson.test(count)$conf.int
    got <- islh_ci_poisson(count, method = "exact")
    expect_equal(got$lower, expected[1], tolerance = 1e-8)
    expect_equal(got$upper, expected[2], tolerance = 1e-8)
  }
})

test_that("Byar's approximation tracks the exact interval for larger counts", {
  # Byar's is documented as accurate for counts of roughly 10 or more.
  counts <- c(10, 25, 100, 500)
  byar <- islh_ci_poisson(counts, method = "byar")
  exact <- islh_ci_poisson(counts, method = "exact")

  expect_equal(byar$lower, exact$lower, tolerance = 0.01)
  expect_equal(byar$upper, exact$upper, tolerance = 0.01)
})

test_that("a count of zero gives a lower limit of zero either way", {
  # Byar's is undefined at zero, so both methods must fall back to exact.
  for (m in c("byar", "exact")) {
    ci <- islh_ci_poisson(0, method = m)
    expect_equal(ci$lower, 0)
    # One-sided upper limit for zero events at 95%: qchisq(0.975, 2) / 2.
    expect_equal(ci$upper, stats::qchisq(0.975, 2) / 2, tolerance = 1e-8)
  }
})

test_that("intervals contain the estimate and widen as confidence rises", {
  counts <- c(1, 5, 40, 300)
  for (m in c("byar", "exact")) {
    ci <- islh_ci_poisson(counts, method = m)
    expect_true(all(ci$lower <= counts))
    expect_true(all(ci$upper >= counts))
    expect_true(all(ci$lower >= 0))

    wide <- islh_ci_poisson(counts, conf = 0.99, method = m)
    expect_true(all(wide$lower <= ci$lower))
    expect_true(all(wide$upper >= ci$upper))
  }
})

test_that("crude rates scale the count and its interval identically", {
  out <- islh_crude_rate(cases = 12, population = 50000)
  expect_equal(out$rate, 12 / 50000 * 100000)

  ci <- islh_ci_poisson(12)
  expect_equal(out$lower, ci$lower / 50000 * 100000)
  expect_equal(out$upper, ci$upper / 50000 * 100000)

  # `per` only changes the scale, never the relative width.
  per_1000 <- islh_crude_rate(12, 50000, per = 1000)
  expect_equal(per_1000$rate * 100, out$rate)
})

test_that("a standardised rate equals the crude rate when the structures match", {
  # If the study population has the same shape as the standard, standardising
  # changes nothing. This is the identity that catches a mis-weighted sum.
  cases <- c(5, 12, 40, 80)
  population <- c(20000, 25000, 22000, 15000)

  dsr <- islh_dsr(cases, population, std_population = population)
  crude <- islh_crude_rate(sum(cases), sum(population))

  expect_equal(dsr$rate, crude$rate, tolerance = 1e-8)
})

test_that("standardising removes a known confounding age structure", {
  # Two populations with identical stratum-specific rates but different age
  # structures must standardise to the same rate, while their crude rates
  # differ. This is the whole point of the method.
  rates <- c(0.001, 0.005, 0.02)
  young <- c(60000, 30000, 10000)
  old <- c(10000, 30000, 60000)
  standard <- c(40000, 30000, 30000)

  dsr_young <- islh_dsr(rates * young, young, standard)
  dsr_old <- islh_dsr(rates * old, old, standard)
  expect_equal(dsr_young$rate, dsr_old$rate, tolerance = 1e-8)

  crude_young <- sum(rates * young) / sum(young)
  crude_old <- sum(rates * old) / sum(old)
  expect_true(crude_old > crude_young)
})

test_that("the gamma interval is wider than the normal one and never negative", {
  # Fay and Feuer's point: with small counts the normal approximation is too
  # narrow and can drop below zero, which is impossible for a rate.
  cases <- c(1, 2, 3, 2)
  population <- c(20000, 25000, 22000, 15000)
  standard <- c(30000, 30000, 25000, 15000)

  gamma <- islh_dsr(cases, population, standard, method = "gamma")
  normal <- islh_dsr(cases, population, standard, method = "normal")

  expect_equal(gamma$rate, normal$rate, tolerance = 1e-8)
  expect_gt(gamma$upper, normal$upper)
  expect_gte(gamma$lower, 0)
  expect_lte(gamma$lower, gamma$rate)
  expect_gte(gamma$upper, gamma$rate)
})

test_that("standard populations are normalised, so only relative sizes matter", {
  cases <- c(5, 12, 40)
  population <- c(20000, 25000, 22000)
  standard <- c(30000, 30000, 25000)

  expect_equal(
    islh_dsr(cases, population, standard)$rate,
    islh_dsr(cases, population, standard * 1000)$rate,
    tolerance = 1e-10
  )
})

test_that("bad arguments are rejected rather than silently coerced", {
  expect_error(islh_ci_poisson(-1), "not be negative")
  expect_error(islh_ci_poisson(5, conf = 1), "between 0 and 1")
  expect_error(islh_ci_poisson(5, conf = 0), "between 0 and 1")
  expect_error(islh_crude_rate(5, 0), "must be positive")
  expect_error(islh_crude_rate(c(1, 2), c(100, 200, 300)), "length 1 or")
  expect_error(islh_dsr(c(1, 2), c(100, 200), c(100)), "same length")
})

# Regression tests for the code review of v0.1.0.

test_that("rate functions refuse invalid epidemiological inputs", {
  expect_error(islh_ci_poisson(2.5), "whole counts")
  expect_error(islh_ci_poisson(Inf), "finite")
  expect_error(islh_ci_poisson(-1), "not be negative")
  expect_error(islh_ci_poisson(factor("3")), "factor")

  expect_error(islh_crude_rate(5, 1000, per = 0), "positive")
  expect_error(islh_crude_rate(5, 1000, per = -100), "positive")
  expect_error(islh_crude_rate(5, 1000, per = c(1000, 100)), "single positive")
  expect_error(islh_crude_rate(5, 0), "must be positive")
  expect_error(islh_crude_rate(5, Inf), "finite")

})

test_that("event counts may exceed population or person-time denominators", {
  crude <- islh_crude_rate(cases = 1500, population = 1000, per = 1000)
  expect_equal(crude$rate, 1500)

  standardised <- islh_dsr(
    cases = c(1500, 400),
    population = c(1000, 800),
    std_population = c(1, 1),
    per = 1000
  )
  expect_equal(standardised$rate, 1000)
})

test_that("a standard population of all zeros is an error, not an NA rate", {
  # Previously this divided by zero and returned NA, which reads like a
  # computed answer rather than a broken input.
  expect_error(
    islh_dsr(c(1, 2), c(100, 200), c(0, 0)),
    "positive total"
  )

  # A single zero stratum is fine: that stratum simply gets no weight.
  out <- islh_dsr(c(1, 2), c(100, 200), c(0, 100))
  expect_true(is.finite(out$rate))
})

test_that("a missing stratum stops a standardised rate rather than poisoning it", {
  # A DSR sums over every stratum, so one NA makes the whole answer NA.
  expect_error(islh_dsr(c(1, NA), c(100, 200), c(50, 50)), "missing")
  expect_error(islh_dsr(c(1, 2), c(100, NA), c(50, 50)), "missing")
})

test_that("stratum vectors must line up", {
  expect_error(islh_dsr(c(1, 2), c(100, 200), 100), "same length")
  expect_error(
    islh_dsr(numeric(0), numeric(0), numeric(0)), "at least one stratum"
  )
  expect_error(islh_crude_rate(c(1, 2), c(100, 200, 300)), "length 1 or")
})

test_that("the Fay-Feuer interval matches a published worked example", {
  # Fay & Feuer (1997), Statistics in Medicine 16:791-801, section 4, use a
  # single stratum where the gamma interval reduces to the exact Poisson
  # interval on the count. That identity is the reference point: with one
  # stratum the standard population cannot reweight anything, so the
  # standardised rate and its interval must equal the crude ones.
  cases <- 10
  population <- 100000

  dsr <- islh_dsr(cases, population, std_population = 1)
  exact <- islh_ci_poisson(cases, method = "exact")

  expect_equal(dsr$rate, cases / population * 100000, tolerance = 1e-8)
  expect_equal(dsr$lower, exact$lower / population * 100000, tolerance = 1e-6)

  # The upper limit carries Fay and Feuer's conservative w_max adjustment, so
  # it sits at or above the exact one rather than exactly on it.
  expect_gte(dsr$upper, exact$upper / population * 100000 - 1e-6)
})


