#' @title Myncurve
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The upper bound of the shaded region
#'
#' @returns A list containing:
#' {mu},The mean of the distribution.
#' {sigma}, The standard deviation of the distribution.
#' {P_X_leq_a}, The cumulative probability
#'
#'
#' @export
#'
#' @examples
#' # Example 1: Normal distribution with mu = 0, sigma = 1, and a = 1.5
#' result <- myncurve(mu = 0, sigma = 1, a = 1.5)
#' print(result)
#'
#' # Example 2: Normal distribution with mu = 2, sigma = 2, and a = 3
#' result <- myncurve(mu = 2, sigma = 2, a = 3)
#' print(result)
myncurve = function(mu, sigma, a) {
  # Calculate probability
  prob = pnorm(a, mean = mu, sd = sigma)

  # Plot only in an interactive session
  if (interactive()) {
    curve(dnorm(x, mean = mu, sd = sigma),
          xlim = c(mu - 3 * sigma, mu + 3 * sigma))

    # Shading
    x_vals = seq(mu - 3 * sigma, a, length.out = 100)
    y_vals = dnorm(x_vals, mean = mu, sd = sigma)
    polygon(c(x_vals, a), c(y_vals, 0), col = "lightblue", border = NA)
  }

  # Return a named list
  list(mu = mu, sigma = sigma, probability = prob)
}
