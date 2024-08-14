test_that("function fails if given non-numeric", {
  expect_error(number_change("five"), "Values provided are not numeric")
  expect_error(number_change("5"), "Values provided are not numeric")
})

test_that("function returns correct commentary for single values", {
  expect_equal(number_change(10000), "up 10,000")
  expect_equal(number_change(-200), "down 200")
  expect_equal(number_change(-85), "unchanged")
})

test_that("function returns correct commentary for multiple values", {
  expect_equal(number_change(c(1000, 4000)), "up 1,000 and 4,000 respectively")
  expect_equal(number_change(c(-200, -500)), "down 200 and 500 respectively")
  expect_equal(number_change(c(1000, 200, 2000)), "up 1,000, 200 and 2,000 respectively")
  expect_equal(number_change(c(1000, -200, 2000)), "up 1,000, down 200 and up 2,000 respectively")
  expect_equal(number_change(c(1000, -200, 0)), "up 1,000, down 200 and unchanged respectively")
})


test_that("changing description type works", {
  expect_equal(number_change(200, description = "ro"), "rose 200")
  expect_equal(number_change(200, description = "io"), "an increase of 200")
  expect_equal(number_change(200, description = "ab"), "200 above")
  expect_equal(
    number_change(c(-200, 0), description = "ri"),
    "a fall of 200 and little change respectively"
  )
  expect_equal(
    number_change(c(-200, 0), description = "rr"),
    "a 200 reduction and little change respectively"
  )
  expect_equal(
    number_change(c(-200, 0), description = "ub"),
    "down by 200 and unchanged respectively"
  )
})

test_that("changing unchanged limit works", {
  expect_equal(number_change(50, unchanged_limit = 40), "up 50")
  expect_equal(number_change(-12, unchanged_limit = 10), "down 12")
  expect_equal(number_change(-12, unchanged_limit = 15), "unchanged")
  expect_equal(number_change(c(-12, 5000), unchanged_limit = 10), "down 12 and up 5,000 respectively")
})

test_that("multiple unchanged values changes wording", {
  expect_equal(number_change(c(40, 20)), "both unchanged")
  expect_equal(number_change(c(10, 43, 87)), "all unchanged")
})
