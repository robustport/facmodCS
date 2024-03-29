#' @title Decompose portfolio standard deviation into individual factor contributions
#' 
#' @description Compute the factor contributions to standard deviation (Sd) of 
#' portfolio returns based on Euler's theorem, given the fitted factor model.
#' 
#' @importFrom stats quantile residuals cov resid qnorm
#' @importFrom zoo index as.yearmon
#' 
#' @details The factor model for a portfolio's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. 
#' \cr \cr By Euler's theorem, the standard deviation of the portfolio's return 
#' is given as: \cr \cr 
#' \code{portSd = sum(cSd_k) = sum(beta.star_k*mSd_k)} \cr \cr 
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cSd} and \code{mSd} are the component and marginal 
#' contributions to \code{Sd} respectively. Computing \code{portSd} and 
#' \code{mSd} is very straight forward. The formulas are given below and 
#' details are in the references. The covariance term is approximated by the 
#' sample covariance. \cr \cr
#' \code{portSd = sqrt(beta.star''cov(F.star)beta.star)} \cr 
#' \code{mSd = cov(F.star)beta.star / portSd}
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL, 
#' in which case an equal weights will be used.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param ... optional arguments passed to \code{\link[stats]{cov}}.
#' 
#' @return A list containing 
#' \item{portSd}{factor model Sd of portfolio return.}
#' \item{mSd}{length-(K + 1) vector of marginal contributions to Sd.}
#' \item{cSd}{length-(K + 1) vector of component contributions to Sd.}
#' \item{pcSd}{length-(K + 1) vector of percentage component contributions to Sd.}
#' Where, K is the number of factors.
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' 
#' @seealso \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portVaRDecomp}} for portfolio factor model VaR decomposition.
#' \code{\link{portEsDecomp}} for portfolio factor model ES decomposition.
#' 
#' 
#' @example
#' args(portSdDecomp)
#' @export    
                                   

portSdDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm',  or 'ffm'.")
  }
  UseMethod("portSdDecomp")
}

#' @rdname portSdDecomp
#' @method portSdDecomp ffm
#' @export

portSdDecomp.ffm <- function(object, weights = NULL, factor.cov, ...) {
  
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- unique(object$data[[object$asset.var]])
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  }  

  
  # get portfolio beta.star: 1 x (K+1)
  beta.star <- as.matrix(cbind(weights %*% beta, sqrt(sum(weights^2 * object$resid.var))))
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
  K <- ncol(beta)
  factor.star.cov <- diag(K+1)
  factor.star.cov[1:K, 1:K] <- factor.cov
  colnames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
  rownames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
  

  # compute factor model sd; a vector of length 1
  Sd.fm <- drop(sqrt(rowSums(beta.star %*% factor.star.cov * beta.star)))
  
  # compute marginal, component and percentage contributions to sd
  # each of these have dimensions: N+K
  mSd <- drop((t(factor.star.cov %*% t(beta.star)))/Sd.fm)
  cSd <- drop(mSd * beta.star)
  pcSd <- drop(100* cSd/Sd.fm) 
  
  fm.sd.decomp <- list(portSd=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)
  
  return(fm.sd.decomp)
}
