#' Inverse Box-Cox Transformation
#'
#' This function applies Inverse Box-Cox transformation to a numerical vector.
#'
#' @param z Vector to be inverse-transformed.
#' @param lambda Power to raise inverse \code{z} by.
#' @param plot_it Display a plot of \code{z} vs the output? Use logical.
#'
#' @return The vector \code{z}, Inversely Box-Cox transformed at a power of \code{lambda}.
#' @export
#'

bctrans_inv <- function(z, lambda, plot_it=TRUE) {
    stopifnot(lambda >= 0)
    if (lambda == 0) {
        res <- exp(z)
    } else {
        res <- exp(log(z*lambda+1)/lambda)
    }
    if (plot_it) print(ggplot2::qplot(z, res))
    return(res)
}
