% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portEsDecomp.R
\name{portEsDecomp}
\alias{portEsDecomp}
\alias{portEsDecomp.ffm}
\title{Decompose portfolio ES into individual factor contributions}
\usage{
portEsDecomp(object, ...)

\method{portEsDecomp}{ffm}(
  object,
  weights = NULL,
  factor.cov,
  p = 0.05,
  type = c("np", "normal"),
  invert = FALSE,
  ...
)
}
\arguments{
\item{object}{fit object of class  \code{ffm}.}

\item{...}{other optional arguments passed to \code{\link[stats]{quantile}} and 
optional arguments passed to \code{\link[stats]{cov}}}

\item{weights}{a vector of weights of the assets in the portfolio, names of 
the vector should match with asset names. Default is NULL, in which case an 
equal weights will be used.}

\item{factor.cov}{optional user specified factor covariance matrix with 
named columns; defaults to the sample covariance matrix.}

\item{p}{tail probability for calculation. Default is 0.05.}

\item{type}{one of "np" (non-parametric) or "normal" for calculating Es. 
Default is "np".}

\item{invert}{a logical variable to choose if change ES to positive number, default
is False}
}
\value{
A list containing 
\item{portES}{factor model ES of portfolio returns.}
\item{mES}{length-(K + 1) vector of marginal contributions to Es.}
\item{cES}{length-(K + 1) vector of component contributions to Es.}
\item{pcES}{length-(K + 1) vector of percentage component contributions to Es.}
Where, K is the number of factors.
}
\description{
Compute the factor contributions to Expected Tail Loss or 
Expected Shortfall (ES) of portfolio returns  based on Euler's theorem, given 
the fitted factor model. The partial derivative of ES with respect to factor 
beta is computed as the expected factor return given portfolio return is less 
than or equal to its value-at-risk (VaR). Option to choose between 
non-parametric and Normal.
}
\details{
The factor model for a portfolio's return at time \code{t} has the 
form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. By 
Euler's theorem, the ES of the portfolio's return is given by:
\cr \cr \code{ES.fm = sum(cES_k) = sum(beta.star_k*mES_k)} \cr \cr
where, summation is across the \code{K} factors and the residual, 
\code{cES} and \code{mES} are the component and marginal 
contributions to \code{ES} respectively. The marginal contribution to ES is
defined as the expected value of \code{F.star}, conditional on the loss 
being less than or equal to \code{portVaR}. This is estimated as a sample 
average of the observations in that data window.
}
\seealso{
\code{\link{fitFfm}}
for the different factor model fitting functions.

\code{\link{portSdDecomp}} for factor model Sd decomposition.
\code{\link{portVaRDecomp}} for factor model VaR decomposition.
}
\author{
Douglas Martin, Lingjie Yi
}
