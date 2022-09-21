test_that("function fails if given non-numeric", {
  expect_error(percent_change("five"), "Values provided are not numeric")
  expect_error(percent_change("5"), "Values provided are not numeric")
})

test_that("function returns correct commentary for single values", {
  expect_equal(percent_change(0.1), "up 10%")
  expect_equal(percent_change(-0.02), "down 2%")
  expect_equal(percent_change(-0.0001), "unchanged")
})

test_that("function returns correct commentary for multiple values", {
  expect_equal(percent_change(c(0.1, 0.4)), "up 10% and 40% respectively")
  expect_equal(percent_change(c(-0.02, -0.5)), "down 2% and 50% respectively")
  expect_equal(percent_change(c(0.1, 0.01, 0.2)), "up 10%, 1% and 20% respectively")
  expect_equal(percent_change(c(0.1, -0.01, 0.2)), "up 10%, down 1% and up 20% respectively")
  expect_equal(percent_change(c(0.1, -0.01, 0)), "up 10%, down 1% and unchanged respectively")
})


test_that("changing description type works", {
  expect_equal(percent_change(0.1, description = "ro"), "rose 10%")
  expect_equal(percent_change(0.1, description = "io"), "an increase of 10%")
  expect_equal(percent_change(0.1, description = "ab"), "10% above")
  expect_equal(
    percent_change(c(-0.1, 0), description = "ri"),
    "a fall of 10% and little change respectively"
  )
  expect_equal(
    percent_change(c(-0.1, 0), description = "rr"),
    "a 10% reduction and little change respectively"
  )
  expect_equal(
    percent_change(c(-0.1, 0), description = "ub"),
    "down by 10% and unchanged respectively"
  )
})

test_that("changing rounding works", {
  expect_equal(percent_change(0.111, accuracy = 0.1), "up 11.1%")
  expect_equal(percent_change(-0.0125, accuracy = 0.01), "down 1.25%")
})
