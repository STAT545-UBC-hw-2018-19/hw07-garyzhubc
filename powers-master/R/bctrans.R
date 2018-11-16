#' Box-Cox Transformation
#'
#' This function applies Box-Cox transformation to a numerical vector.
#'
#' @param y Vector to be transformed.
#' @param lambda Power to raise \code{y} by.
#' @param plot_it Display a plot of \code{y} vs the output? Use logical.
#'
#' @return The vector \code{y}, Box-Cox transformed to a power of \code{lambda}.
#' @export

bctrans <- function(y, lambda, plot_it=TRUE) {
    stopifnot(lambda >= 0)
    stopifnot(all(y > 0))
    if (lambda == 0) {
        res <- log(y)
    } else {
        res <- (y^lambda-1)/lambda
    }
    if (plot_it) print(ggplot2::qplot(y, res))
    return(res)
}

