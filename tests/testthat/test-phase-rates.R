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



# Fay and Feuer (1997) reference example ------------------------------------
#
# Michigan birth data, 1950-1964, from Stark and Mantel via Fleiss and
# reproduced as Table II of the paper: mongoloid births by maternal age for
# mothers whose child was fifth-born or later. The standard population is the
# maternal-age distribution of all births, the second half of the same table.
#
# This is the paper's own worst case. The weights for the two youngest maternal
# age groups are enormous relative to the rest, because very few mothers have a
# fifth child before they are 25, and one stratum contributes no events at all.
# That is precisely where a multistratum implementation goes wrong quietly, and
# where the gamma interval departs from the normal approximation: Table I gives
# a published upper limit of 188.3 against a rate of 75.5.

michigan_cases <- c(0, 8, 63, 112, 262, 295)
michigan_births <- c(327, 30666, 123419, 149919, 104088, 34392)
michigan_standard <- c(319933, 931318, 786511, 488235, 237863, 61313)

test_that("dsr reproduces the published Fay-Feuer multistratum example", {
  out <- islh_dsr(
    michigan_cases,
    michigan_births,
    michigan_standard,
    per = 100000,
    conf = 0.95,
    method = "gamma"
  )

  # Fay and Feuer (1997), Table I, birth order 5+: rate 75.5, gamma interval
  # (67.7, 188.3), printed to one decimal place.
  expect_equal(round(out$rate, 1), 75.5)
  expect_equal(round(out$lower, 1), 67.7)
  expect_equal(round(out$upper, 1), 188.3)

  expect_equal(out$cases, sum(michigan_cases))
  expect_equal(out$population, sum(michigan_births))
})

test_that("the published example separates gamma from the normal approximation", {
  gamma <- islh_dsr(michigan_cases, michigan_births, michigan_standard)
  normal <- islh_dsr(
    michigan_cases,
    michigan_births,
    michigan_standard,
    method = "normal"
  )

  # The paper's point in choosing this example: with weights this uneven the
  # normal interval is far too narrow at the top. A gamma upper limit that had
  # collapsed onto the normal one would mean the w_max term was not applied.
  expect_gt(gamma$upper, normal$upper + 50)
  expect_equal(round(gamma$rate, 6), round(normal$rate, 6))
})

test_that("gamma limits are exact when the standard matches the study population", {
  # Fay and Feuer show that where the DSR reduces to a scaled Poisson variate
  # the gamma interval gives the exact solution. Weights proportional to the
  # study population are that case, so a five-stratum standardised rate must
  # land exactly on the Garwood interval for the pooled count. The expected
  # values come from the exact Poisson interval, not from re-running the DSR.
  cases <- c(3, 11, 27, 40, 6)
  population <- c(12000, 18500, 22000, 9000, 4500)
  standard <- population * 7.25

  out <- islh_dsr(cases, population, standard, per = 100000, conf = 0.95)
  exact <- islh_crude_rate(
    sum(cases),
    sum(population),
    per = 100000,
    conf = 0.95,
    method = "exact"
  )

  expect_equal(out$rate, exact$rate)
  expect_equal(out$lower, exact$lower)
  expect_equal(out$upper, exact$upper)
})

# Poisson interval method selection -----------------------------------------

test_that("auto uses exact below 10 and Byar at 10 and above", {
  out <- islh_ci_poisson(c(0, 1, 9, 10, 25), method = "auto")
  expect_equal(out$method, c("exact", "exact", "exact", "byar", "byar"))

  exact <- islh_ci_poisson(c(0, 1, 9), method = "exact")
  byar <- islh_ci_poisson(c(10, 25), method = "byar")
  expect_equal(out$lower, c(exact$lower, byar$lower))
  expect_equal(out$upper, c(exact$upper, byar$upper))
})

test_that("auto is the default and zero always takes the exact interval", {
  expect_equal(islh_ci_poisson(4), islh_ci_poisson(4, method = "auto"))

  # Byar's approximation is undefined at zero whichever method was asked for.
  expect_equal(islh_ci_poisson(0, method = "byar")$method, "exact")
  expect_equal(
    islh_ci_poisson(0, method = "byar")$upper,
    islh_ci_poisson(0, method = "exact")$upper
  )
})

test_that("crude rate reports the method used for each row", {
  out <- islh_crude_rate(c(2, 40), c(1000, 50000))
  expect_equal(out$method, c("exact", "byar"))

  fixed <- islh_crude_rate(c(2, 40), c(1000, 50000), method = "byar")
  expect_equal(fixed$method, c("byar", "byar"))
  expect_false(isTRUE(all.equal(out$lower[1], fixed$lower[1])))
})

# Zero denominators ----------------------------------------------------------

test_that("a zero denominator is refused and says which stratum", {
  expect_error(
    islh_crude_rate(c(1, 0), c(1000, 0)),
    "must be positive"
  )
  expect_error(
    islh_crude_rate(c(1, 0), c(1000, 0)),
    "Position"
  )
  expect_error(
    islh_dsr(c(1, 2), c(1000, 0), c(500, 500)),
    "must be positive"
  )
})

test_that("a zero standard population weights a stratum out rather than failing", {
  cases <- c(5, 12, 40)
  population <- c(20000, 25000, 22000)

  with_zero <- islh_dsr(cases, population, c(30000, 30000, 0))
  without <- islh_dsr(cases[1:2], population[1:2], c(30000, 30000))

  expect_equal(with_zero$rate, without$rate)
})
