#' S3 method to plot a DTDAni object by using the generic plot function.
#'
#' @title plot.DTDAni
#' @param x DTDAni object.
#' @param ecdf Whether to display the ordinary empirical cumulative distribution function or not. Default = FALSE.
#' @param ... Aditional parameters.
#'
#' @examples
#'
#' \dontrun{
#' N <- 250
#' x0 <- runif(N)             # Original data
#' u0 <- runif(N, -0.25, 0.5) # Left-truncation times
#' tau <- 0.75                # Interval width
#' v0 <- u0 + tau
#'
#' x <- x0[u0 <= x0 & x0 <= v0]
#' u <- u0[u0 <= x0 & x0 <= v0]
#' v <- v0[u0 <= x0 & x0 <= v0]
#' n <- length(x)  # Final sample size after the interval sampling
#' res <- DTDAni(x, u , tau)
#' plot(res)
#' plot(res, ecdf = TRUE)
#' }
#'
#' @section Acknowledgements:
#' \itemize{
#' \item{Jacobo de Uña-Álvarez was supported by Grant MTM2014-55966-P, Spanish Ministry of Economy and Competitiveness.}
#' \item{José Carlos Soage was supported by Red Tecnológica de Matemática Industrial (Red TMATI), Cons. de Cultura, Educación e OU, Xunta de Galicia (ED341D R2016/051) and by Grupos de Referencia Competitiva, Consolidación y Estructuración de Unidades de Investigación Competitivas del SUG, Cons. de Cultura, Educación e OU, Xunta de Galicia (GRC ED431C 2016/040).}
#' }
#'
#' @references de Uña-Álvarez J. (2018) A Non-iterative Estimator for Interval Sampling and
#' Doubly Truncated Data. In: Gil E., Gil E., Gil J., Gil M. (eds)
#' The Mathematics of the Uncertain. Studies in Systems, Decision and Control,
#' vol 142. Springer, Cham, pp. 387-400.
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#'
#' @export
plot.DTDAni <-  function(x, ecdf = FALSE, ...) {
  graphics::plot(x$x, x$cumprob, type = "s", col = "blue", xlab = "",  ylab = "")
  graphics::abline(a = 0, b = 0, col = "lightgray", lty = 2)
  graphics::abline(a = 1, b = 0, col = "lightgray", lty = 2)

  if (ecdf == TRUE){
    E <- cumsum(x$nx)/length(x)
    graphics::lines(x$x, E, type="s",
                    col="lightgray", lwd = 2)
    graphics::legend("bottomright", c("Corrected", "Non-corrected"),
                     lty = c(1, 1), lwd = c(2.5, 2.5), col = c("blue", "lightgray"))
  }
}


