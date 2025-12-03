#' Calculate optimal number of tickets to sell for a flight
#'
#' @description
#' Calculates the number of tickets to sell when there are \code{N} seats,
#' each passenger shows up with probability \code{p}, and \code{gamma} is
#' the acceptable probability of overbooking (too many passengers show up).
#' It finds this number two ways:
#' - Using the **discrete binomial distribution**
#' - Using the **normal approximation**
#'
#' @param N Integer. Number of available seats on the flight.
#' @param gamma Numeric between 0 and 1. Probability the flight is truly overbooked.
#' @param p Numeric between 0 and 1. Probability that a passenger shows up.
#'
#' @details
#' The function defines an objective function that becomes zero when the
#' probability of having more show-ups than seats equals \code{gamma}.
#' It finds the ticket number \code{n} that satisfies this for both the
#' discrete (binomial) and normal approximation cases.
#'
#' @return
#' A named list containing:
#' \itemize{
#'   \item \code{nd} - Ticket number using discrete distribution
#'   \item \code{nc} - Ticket number using normal approximation
#'   \item \code{N}, \code{p}, and \code{gamma} as input parameters
#' }
#' The function also creates two plots of the objective functions.
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#'
#' @export
ntickets <- function(N, gamma, p) {
  objective_discrete <- function(n) {
    n <- floor(n)
    if (n < N) return(Inf)
    1 - gamma - pbinom(N, size = n, prob = p)
  }

  objective_continuous <- function(n) {
    if (n < N) return(Inf)
    mu <- n * p
    sigma_sq <- n * p * (1 - p)
    if (sigma_sq <= 0) return(Inf)
    sigma <- sqrt(sigma_sq)
    1 - gamma - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  nd <- optimize(function(n) abs(objective_discrete(n)),
                 interval = c(N, N * 2))$minimum
  nd <- floor(nd)

  nc <- optimize(function(n) abs(objective_continuous(n)),
                 interval = c(N, N * 2))$minimum
  nc <- floor(nc)

  n_vals <- seq(N, N * 2, length.out = 100)
  obj_discrete <- sapply(n_vals, function(n) {
    value <- objective_discrete(n)
    if (is.nan(value) || is.infinite(value)) NA else value
  })

  plot(n_vals, obj_discrete, type = 'l', col = 'blue', lwd = 2,
       xlab = 'Number of Tickets Sold', ylab = 'Objective Function',
       main = 'Objective Function vs n (Discrete)',
       ylim = range(obj_discrete, na.rm = TRUE))
  abline(h = 0, col = 'black', lty = 2)

  obj_continuous <- sapply(n_vals, function(n) {
    value <- objective_continuous(n)
    if (is.nan(value) || is.infinite(value)) NA else value
  })

  plot(n_vals, obj_continuous, type = 'l', col = 'red', lwd = 2,
       xlab = 'Number of Tickets Sold', ylab = 'Objective Function',
       main = 'Objective Function vs n (Continuous)',
       ylim = range(obj_continuous, na.rm = TRUE))
  abline(h = 0, col = 'black', lty = 2)

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)
  return(result)
}
