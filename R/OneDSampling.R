#' Single variable rejection sampling
#'
#' This function implements single variable rejection sampling for RVs with bounded support and which have bounded pdf.
#'
#' We expect to get N/maxf samples
#'
#' @param f the pdf that we are sampling from
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f
#'
#' @return vector containing samples from the pdf
#' @export
#'
#' @examples
#'
#' BetaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 2*x, 0)
#' }
#' hist(OneDSample(f = BetaPDF, lb = 0, ub = 1))

OneDSample <- function(f, lb, ub) {
sampled <- data.frame(testdata = runif(10000, lb, ub))
sampled$f <- f(sampled$testdata)
maxf <- max(sampled$f, na.rm = T)
sampled$check <- ifelse(runif(10000, lb, ub) < sampled$f / maxf, TRUE, FALSE)
hist(sampled$testdata[sampled$check], freq = F)
}
