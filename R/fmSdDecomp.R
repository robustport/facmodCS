#' @title Decompose standard deviation into individual factor contributions
#' 
#' @description Compute the factor contributions to standard deviation (SD) of 
#' assets' returns based on Euler's theorem, given the fitted factor model.
#' 
#' @details The factor model for an asset's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. 
#' \cr \cr By Euler's theorem, the standard deviation of the asset's return 
#' is given as: \cr \cr 
#' \code{Sd.fm = sum(cSd_k) = sum(beta.star_k*mSd_k)} \cr \cr 
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cSd} and \code{mSd} are the component and marginal 
#' contributions to \code{SD} respectively. Computing \code{Sd.fm} and 
#' \code{mSd} is very straight forward. The formulas are given below and 
#' details are in the references. The covariance term is approximated by the 
#' sample covariance. \cr \cr
#' \code{Sd.fm = sqrt(beta.star''cov(F.star)beta.star)} \cr 
#' \code{mSd = cov(F.star)beta.star / Sd.fm}
#' 
#' @param object fit object of class \code{tsfm} or \code{ffm}.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param ... optional arguments passed to \code{\link[stats]{cov}}.
#' 
#' @return A list containing 
#' \item{Sd.fm}{length-N vector of factor model SDs of N-asset returns.}
#' \item{mSd}{N x (K+1) matrix of marginal contributions to SD.}
#' \item{cSd}{N x (K+1) matrix of component contributions to SD.}
#' \item{pcSd}{N x (K+1) matrix of percentage component contributions to SD.}
#' Where, \code{K} is the number of factors and N is the number of assets.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan
#' 
#' @references 
#' Hallerback (2003). Decomposing Portfolio Value-at-Risk: A General Analysis. 
#' The Journal of Risk, 5(2), 1-18.
#' 
#' Meucci, A. (2007). Risk contributions from generic user-defined factors. 
#' RISK-LONDON-RISK MAGAZINE LIMITED-, 20(6), 84. 
#' 
#' Yamai, Y., & Yoshiba, T. (2002). Comparative analyses of expected shortfall 
#' and value-at-risk: their estimation error, decomposition, and optimization. 
#' Monetary and economic studies, 20(1), 87-121.
#' 
#' @seealso \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{fmCov}} for factor model covariance.
#' \code{\link{fmVaRDecomp}} for factor model VaR decomposition.
#' \code{\link{fmEsDecomp}} for factor model ES decomposition.
#' 
#' 
#' @export  

fmSdDecomp <- function(object, factor.cov, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("fmSdDecomp")
}


#' @rdname fmSdDecomp
#' @method fmSdDecomp ffm
#' @export

fmSdDecomp.ffm <- function(object, factor.cov, ...) {
  
  # get beta.star: N x (K+1)
  beta <- object$beta
  beta.star <- as.matrix(cbind(beta, sqrt(object$resid.var)))
  colnames(beta.star)[dim(beta.star)[2]] <- "Residuals"
  
  # get cov(F): K x K
  if (missing(factor.cov)) {
    factor.cov = object$factor.cov
  } else {
    if (!identical(dim(factor.cov), dim(object$factor.cov))) {
      stop("Dimensions of user specified factor covariance matrix are not 
           compatible with the number of factors (including dummies) in the 
           fitFfm object")
    }
  }
  
  # get cov(F.star): (K+1) x (K+1)
  K <- ncol(object$beta)
  factor.star.cov <- diag(K+1)
  factor.star.cov[1:K, 1:K] <- factor.cov
  colnames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
  rownames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
  
  # compute factor model sd; a vector of length N
  Sd.fm <- sqrt(rowSums(beta.star %*% factor.star.cov * beta.star))
  
  # compute marginal, component and percentage contributions to sd
  # each of these have dimensions: N x (K+1)
  mSd <- (t(factor.star.cov %*% t(beta.star)))/Sd.fm 
  cSd <- mSd * beta.star 
  pcSd = 100* cSd/Sd.fm 
  
  fm.sd.decomp <- list(Sd.fm=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)
  
  return(fm.sd.decomp)
}
