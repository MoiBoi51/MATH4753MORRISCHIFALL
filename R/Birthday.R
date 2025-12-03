
#' Birthday Function
#'
#' @param x
#'
#' @returns probability of 2 people in a room sharing a birthday
#' @export
#'
#' @examples birthday(20:24)
birthday <- function(x){

1 - exp(lchoose(365, x) + lfactorial(x) - x * log(365))

}
birthday(20:24)
