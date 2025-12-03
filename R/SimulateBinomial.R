#' @title Simulate Binomial
#'
#' @param iter How many times it is iterated
#' @param n Size
#' @param p Probability
#'
#' @returns Makes a barplot of a binomial being simulated and returns the table of said binomial
#' @export
#'
#' @examples simulate_binomial <- function(iter = 10000, n = 10, p = 0.7)
simulate_binomial <- function(iter = 10000, n = 10, p = 0.7) {

  successes <- rbinom(iter, size = n, prob = p)


  success_counts <- table(factor(successes, levels = 0:n)) / iter


  barplot(success_counts,
          main = paste("Proportion of Successes (Iterations =", iter, ")"),
          xlab = "Number of Successes",
          ylab = "Proportion",
          col = rainbow(n + 1),
          border = "black")


  return(success_counts)
}
