test_that("function fails if given non-numeric", {
  expect_error(pp_change("five"), "Values provided are not numeric")
  expect_error(pp_change("5"), "Values provided are not numeric")
})

test_that("function returns correct commentary for single values", {
  expect_equal(pp_change(0.1), "up 10pp")
  expect_equal(pp_change(-0.02), "down 2pp")
  expect_equal(pp_change(-0.0001), "unchanged")
})

test_that("function returns correct commentary for multiple values", {
  expect_equal(pp_change(c(0.1, 0.4)), "up 10pp and 40pp respectively")
  expect_equal(pp_change(c(-0.02, -0.5)), "down 2pp and 50pp respectively")
  expect_equal(pp_change(c(0.1, 0.01, 0.2)), "up 10pp, 1pp and 20pp respectively")
  expect_equal(pp_change(c(0.1, -0.01, 0.2)), "up 10pp, down 1pp and up 20pp respectively")
  expect_equal(pp_change(c(0.1, -0.01, 0)), "up 10pp, down 1pp and unchanged respectively")
})


test_that("changing description type works", {
  expect_equal(pp_change(0.1, description = "ro"), "rose 10pp")
  expect_equal(pp_change(0.1, description = "io"), "an increase of 10pp")
  expect_equal(pp_change(0.1, description = "ab"), "10pp above")
  expect_equal(
    pp_change(c(-0.1, 0), description = "ri"),
    "a fall of 10pp and little change respectively"
  )
  expect_equal(
    pp_change(c(-0.1, 0), description = "rr"),
    "a 10pp reduction and little change respectively"
  )
  expect_equal(
    pp_change(c(-0.1, 0), description = "ub"),
    "down by 10pp and unchanged respectively"
  )
})

test_that("specifying abbreviation or not works", {
  expect_equal(pp_change(0.1, abbr = FALSE), "up 10 percentage points")
  expect_equal(pp_change(-0.01, abbr = FALSE), "down 1 percentage point")
  expect_equal(pp_change(-0.0001, abbr = FALSE), "unchanged")
})

test_that("changing rounding works", {
  expect_equal(pp_change(0.111, digits = 1), "up 11.1pp")
  expect_equal(pp_change(-0.0125, digits = 2), "down 1.25pp")
})
