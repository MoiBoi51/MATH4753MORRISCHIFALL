test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("the birday function works" ,  {
  expect_gt(birthday(23), 0.40)
})
