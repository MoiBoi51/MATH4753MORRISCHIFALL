#' Bootstrap Confidence Interval with Visualization
#'
#' This function performs a bootstrap simulation to estimate confidence intervals
#' for a statistic of interest (e.g., mean, median, IQR) and visualizes the results.
#'
#' @param iter Number of bootstrap iterations (default is 10000).
#' @param x A numeric vector of sample data.
#' @param fun A function used to compute the statistic (default is "mean").
#' @param alpha Significance level (default is 0.05 for a 95% confidence interval).
#' @param cx A scaling factor for text size in the plot (default is 1.5).
#' @param ... Additional arguments passed to the histogram function.
#'
#' @return A list containing:
#' \describe{
#'   \item{ci}{Confidence interval as a numeric vector.}
#'   \item{fun}{The function used to compute the statistic.}
#'   \item{x}{The original sample.}
#'   \item{xstat}{The bootstrap sample statistics.}
#' }
#'
#' @examples
#' set.seed(123)
#' sam <- rnorm(30, mean = 10, sd = 4)
#' myboot2(x = sam, fun = "median", alpha = 0.10)
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))

  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics\n",
                            "alpha = ", alpha, " iter = ", iter, sep = ""), ...)

  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat))
}
