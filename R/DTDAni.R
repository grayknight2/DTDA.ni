#' @title Doubly Truncated Data Analysis, Non Iterative
#'
#' @description
#' This function computes a non-iterative estimator for the cumulative distribution of a doubly truncated variable, see de Uña-Álvarez (2018).
#' The function is restricted to interval sampling.
#'
#' @param x Numeric vector corresponding the variable of ultimate interest.
#' @param u Numeric vector corresponding to the left truncation variable.
#' @param tau Sampling interval width. The right truncation values will be internally calculated as v = u + tau.
#'
#' @details The function DTDAni is adapted to the presence of ties.
#'  It can be used to compute the direct \eqn{(Fd)} and the reverse \eqn{(Fr)} estimators;
#'  see the example below. Both curves are valid estimators for the cumulative
#'  distribution (F) of the doubly truncated variable. Weighted estimators
#'  \eqn{Fw = w*Fd + (1-w)*Fr} with \eqn{0<w<1} are valid too, the choice \eqn{w=1/2} being
#'  recommended in practice (de Uña-Álvarez, 2018).
#
#' @return  A list containing:
#' \item{x}{The distinct values of the variable of interest.}
#' \item{nx}{The absloute frequency of each x value.}
#' \item{cumprob}{The estimated cumulative probability for each x value.}
#' \item{P}{The auxiliary Pi used in the calculation of the estimator.}
#' \item{L}{The auxiliary Li used in the calculation of the estimator.}
#'
#' @section Acknowledgements:
#' \itemize{
#' \item{Jacobo de Uña-Álvarez was supported by Grant MTM2014-55966-P, Spanish Ministry of Economy and Competitiveness.}
#' \item{José Carlos Soage was supported by Red Tecnológica de Matemática Industrial (Red TMATI), Cons. de Cultura, Educación e OU, Xunta de Galicia (ED341D R2016/051) and by Grupos de Referencia Competitiva, Consolidación y Estructuración de Unidades de Investigación Competitivas del SUG, Cons. de Cultura, Educación e OU, Xunta de Galicia (GRC ED431C 2016/040).}
#' }
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#' @references de Uña-Álvarez J. (2018) A Non-iterative Estimator for Interval Sampling and
#' Doubly Truncated Data. In: Gil E., Gil E., Gil J., Gil M. (eds)
#' The Mathematics of the Uncertain. Studies in Systems, Decision and Control,
#' vol 142. Springer, Cham
#'
#' @examples
#' \dontrun{
#' # Generating data which are doubly truncated:
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
#'
#' # Create an object with DTDAni function
#' res <- DTDAni(x, u, tau)
#' plot(res)
#'
#' abline(a = 0, b = 1, col = "green")  #the true cumulative distribution
#'
#' # Calculating the reverse estimator:
#' res2 <- DTDAni(-x, -u - tau, tau)
#' lines(-res2$x, 1 - res2$cumprob, type = "s", col = "blue", lty = 2)
#'
#' # Weigthed estimator (recommended):
#'
#' w <- 1/2
#'
#' k <- length(res$x)
#'
#' Fw <- w * res$cumprob + (1 - w) * (1 - res2$cumprob[k:1])
#' lines(res$x, Fw, type = "s", col = 2)
#'
#'
#' # Using res$P and res$L to compute the estimator:
#'
#' k <- length(res$x)
#' F <- rep(1, k)
#' for (i in 2:k){
#'   F[i] <- (F[i - 1] - res$P[i - 1]) / res$L[i - 1] + res$P[i - 1]
#' }
#'
#' F0 <- F/max(F)  # This is equal to res$cumprob
#' }
#'
#' @export
DTDAni <- function(x, u, tau) {
  cat("Call:", "\n")
  print(match.call())

  if (any(is.numeric(c(x, u, tau)))){
  } else {
    stop("All arguments must be numeric.")
  }

  if (missing(x) && missing(u) && missing(tau)){
    stop("Arguments 'x', 'u' and 'tau' are missing, with no default")
  }
  if (missing(u) && missing(tau)){
    stop("Arguments 'u' and 'tau' are missing")
  }
  if (missing(x) && missing(tau)){
    stop("Arguments 'x' and 'tau' are missing")
  }
  if (missing(x)){
    stop("Argument 'x' is missing")
  }
  if (missing(u)){
    stop("Argument 'u' is missing")
  }
  if (missing(tau)){
    stop("Argument 'tau' is missing")
  }

  v <- u + tau

  if (any(u > x)){
    warning("Condition 'u <= x' not matched")
  }
  if (any(x > v)) {
    warning("Condition 'x <= v' not matched")
  }

  u <- u[order(x)]
  v <- v[order(x)]
  x <- x[order(x)]

  if (any(diff(x) > tau)) {
    warning("Condition 'x(i)-x(i-1) <= tau' not matched")
  }

  vv <- unique(x)

  fvvi <- rep(1, length(vv))

  for (i in 1:length(vv)) {
    fvvi[i] <- length(x[x == vv[i]])
  }

  n.vv <- length(vv)
  FLB <- rep(1, n.vv)

  for (i in (n.vv - 1):1) {
    ui <- u[v >= vv[i + 1] & vv[i + 1] >= x]
    xi <- x[v >= vv[i + 1] & vv[i + 1] >= x]

    xiu <- unique(xi)
    niu <- length(xiu)

    ### lynden-bell propiamente dicho (ties):
    cnu <- rep(1, niu)
    for (m in 1:niu) {
      cnu[m] <- length(xi[ui <= xiu[m] & xiu[m] <= xi])
    }

    fii <- rep(1, niu)
    for (m in 1:niu) {
      fii[m] <- length(xi[xi == xiu[m]])
    }

    FLB[i] <- 1 - prod(1 - fii[xiu <= vv[i]] / cnu[xiu <= vv[i]])
    FLB[i] <- max(FLB[i] , 1/length(vv))
  }

  FF <- rep(1, n.vv)
  P <- rep(0, n.vv)
  indicator <- rep(0, n.vv)

  for (i in 2:n.vv) {

    vvv <- vv
    vvv[vv >= vv[i] - tau] = min(vv) - tau - 1

    index <- which.max(vvv)

    indicator[i] <- as.numeric(length(vv[vv < vv[i] - tau]) > 0)

    P[i] <- indicator[i] * FF[index]

    FF[i] <- (FF[i - 1] - P[i] * (1 - FLB[i - 1])) / FLB[i - 1]
  }

  FFnorm <- FF / max(FF)
  cfvvi <- cumsum(fvvi)
  k <- length(vv)
  r <- list(x = vv,  nx = fvvi, cumprob = FFnorm, P = P[-1], L = FLB[-length(FLB)])
  class(r) <- c('list', 'DTDAni')
  r
}



# https://win-builder.r-project.org/incoming_pretest/180327_133338_DTDA.ni_0009000/00check.log
