#' Central Limit Theorem Simulation with Uniform Distribution
#'
#' Simulates the sum of `n` uniform random variables over `iter` iterations.
#'
#' @param n Integer. Number of uniform random variables to sum.
#' @param iter Integer. Number of iterations.
#' @param a Numeric. Lower bound of the uniform distribution (default is 0).
#' @param b Numeric. Upper bound of the uniform distribution (default is 5).
#'
#' @return A numeric vector containing the sums of each iteration.
#' @examples
#' myclt(n = 10, iter = 1000)
#'
#' @export
myclt <- function(n = 10, iter = 1000, a = 0, b = 5) {
  y <- runif(n * iter, a, b)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  sm <- apply(data, 2, sum)

  h <- hist(sm, plot = FALSE)
  hist(sm, col = rainbow(length(h$mids)), freq = FALSE,
       main = "Distribution of the sum of uniforms")

  curve(dnorm(x, mean = n * (a + b) / 2,
              sd = sqrt(n * (b - a)^2 / 12)),
        add = TRUE, lwd = 2, col = "Blue")

  return(sm)
}
