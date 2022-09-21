test_that("smart paste can join 2 or more values correctly", {
  expect_equal(smart_paste(c(2, 3)), "2 and 3")
  expect_equal(smart_paste(c(2, 3, 4)), "2, 3 and 4")
  expect_equal(smart_paste(c(2, 2, 3, 4)), "2, 2, 3 and 4")
  expect_equal(smart_paste(c("dogs", "cats", "bees")), "dogs, cats and bees")
})
