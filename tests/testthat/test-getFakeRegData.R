test_that("built-in data can be obtained", {
  expect_equal(class(getBasisData()), "data.frame")
})
