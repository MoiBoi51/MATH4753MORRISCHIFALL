test_that("myncurve returns correct components", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  # 1️⃣ mu component should be 10
  expect_equal(result$mu, 10)

  # 2️⃣ sigma component should be 5
  expect_equal(result$sigma, 5)

  # 3️⃣ probability should match pnorm()
  expect_equal(result$probability, pnorm(6, mean = 10, sd = 5))
})
