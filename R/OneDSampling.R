#' Single variable rejection sampling
#'
#' This function implements single variable rejection sampling for RVs with bounded support and which have bounded pdf.
#'
#' We expect to get N/maxf samples
#'
#' @param f the pdf that we are sampling from
#' @param N the number of attempted samples
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f
#' @param maxf bound of f
#'
#' @return vector containing samples from the pdf
#' @export
#'
#' @examples
#'
#' BetaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 2*x, 0)
#' }
#' hist(OneDSample(f = BetaPDF, N=100, lb = 0, ub = 1, maxf = 2))

OneDSample <- function(f, N, lb, ub, maxf) {
  ones <- runif(N, lb, ub)
  unis <- runif(N, 0, maxf)
  ones[unis < f(ones)]
}

