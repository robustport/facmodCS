#' @title  Factor Model R-Squared and Adj R-Squared Values
#'
#' @description Calcluate and plot the Factor Model R-Squared, Adjusted R-Squared for a portfolio of assets
#'
#' @importFrom zoo as.yearmon
#' @importFrom graphics barplot
#' @importFrom stats lm
#' @importFrom xts xts
#' @importFrom lattice panel.abline xyplot panel.xyplot strip.custom
#' 
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param rsq      logical; if \code{TRUE}, Factor Model R-squared values are computed for the portfolio. Default is \code{TRUE}.
#' @param rsqAdj   logical; if \code{TRUE}, Adjusted R-squared values are computed for the portfolio. Default is \code{FALSE}.
#' @param plt.type a number to indicate the type of plot for plotting Factor Model R-squared/Adj. R-squared values.
#'                 1 indicates barplot, 2 indicates time series xy plot. Default is 2.
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2.
#' @param isPrint  logical. if \code{TRUE}, the time series of the computed factor model values is printed along with their mean values.
#'                 Else, only the mean values are printed. Default is \code{TRUE}.
#' @param isPlot   logical. if \code{TRUE}, the time series of the output is plotted. Default is \code{TRUE}.
#' @param lwd      line width relative to the default. Default is 2.
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param title    logical. if \code{TRUE}, the plots will have the main tiltle. default is \code{TRUE}.
#' @param ...      potentially further arguments passed.
#' @author Avinash Acharya and Doug Martin
#'
#' @return \code{fmRsq} returns the sample mean values and plots the time series of corresponding R squared values
#'                         and the Variance Inflation factors depending on the values of \code{rsq}, \code{rsqAdj} and \code{VIF}.
#'                         The time series of the output values are also printed if \code{isPrint} is \code{TRUE} 
#'
#' @examples
#' \donttest{
#' #Load the data
#' # Fundamental Factor Model
#' library(PCRA)
#' 
#' dateRange <- c("2006-01-31","2010-12-31")
#' stockItems <-  c("Date", "TickerLast",  "Return","Sector")
#' factorItems <- c("BP","Beta60M","PM12M1M")
#' facDatIT <- selectCRSPandSPGMI("monthly",
#'                                dateRange = dateRange, 
#'                                stockItems = stockItems, 
#'                                factorItems = factorItems, 
#'                                outputType = "data.table")
#' asset.var="TickerLast" 
#' ret.var="Return" 
#' date.var = "Date"
#' exposure.vars= factorItems
#' asset.var="TickerLast" 
#' ret.var="Return" 
#' date.var = "Date"
#' spec1 <- specFfm(data = facDatIT,asset.var = asset.var, ret.var = ret.var, 
#'                 date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
#'                  addIntercept = TRUE, rob.stats = FALSE)
#' # fit a fundamental factor model
#' mdlFit <- fitFfmDT(spec1)
#' mdlRes <- extractRegressionStats(spec1,mdlFit)
#' fit.cross <- convert(SpecObj = spec1,FitObj = mdlFit, RegStatsObj = mdlRes)
#' #Calculate and plot the portfolio R-squared values
#' fmRsq(fit.cross)
#' 
#' #Plot and print the time series of Adj R-squared and VIF values
#' fmRsq(fit.cross, rsqAdj=TRUE, isPrint=TRUE, plt.type = 2)
#'
#' }
#' 
#' @export
#' 
fmRsq <- function(ffmObj, rsq=TRUE, rsqAdj=FALSE,plt.type= 2, digits=2, isPrint=TRUE, isPlot =TRUE, lwd =2,stripText.cex =1,axis.cex=1, title = TRUE, ...)
{
  # set defaults and check input validity
  if (!inherits(ffmObj, "ffm"))
  {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  if (!(rsq) && !(rsqAdj))
  {
    stop("Invalid arguments: Inputs rsq and rsqAdj cannot be False.")
  }
  
  n.assets <- length(ffmObj$asset.names)
  r2<- ffmObj$r2
  out<- list()
  ret<- list()
  plt.r2<- FALSE
  
  if(rsq)
  {
    if(isPlot && plt.type == 1)
    {
      barplot(r2,las=2,col=5,
              names.arg= zoo::as.yearmon(names(r2)),
              cex.names=0.5,
              main=" ")
      if(title){title("Factor Model R-squared Values")}
    }
    else if (isPlot && plt.type == 2)
    {
      r2.xts = xts(r2, order.by = zoo::as.yearmon(names(r2)))
      
      if(rsqAdj) plt.r2 = TRUE else{
        if(title) title.Rsq = "Factor Model R-squared Values" else title.Rsq = " " 
        panel =  function(...){
          panel.abline(h=0,lty = 3)
          panel.xyplot(...)}
        plt = xyplot(r2.xts,col = "blue",lwd =lwd, panel =panel, scale = list(y = list(cex=axis.cex,rot =0),x = list(cex =axis.cex)),par.strip.text = list(cex = stripText.cex),
                     main = title.Rsq, type = "h", ylim = c(0,(max(r2.xts)+0.05)), strip.left = strip.custom(var.name = "Rsq", style = 1, strip.names = TRUE,strip.levels=FALSE ))
        print(plt) 
        
      }
    }
    r2.mean<- round(mean(r2),digits = digits)
    names(r2.mean) <- "Mean R-Squared"
    out<- r2.mean
    r2<- round(r2,digits = digits)
    ret<- list("R-squared" = r2)
  }
  if(rsqAdj)
  {
    K <- length(ffmObj$factor.name)
    p <- K-1
    adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
    adj.r2.xts = xts(adj.r2, order.by = zoo::as.yearmon(names(r2)))
    if(isPlot && plt.type == 1)
    {
      barplot(adj.r2,las=2,col=5,
              names.arg= zoo::as.yearmon(names(r2)),
              cex.names=0.5,
              main=" ")
      if(title){title(" Factor Model Adjusted R-squared Values")}
    }
    else if (isPlot && plt.type == 2 && !plt.r2)
    {
      if(title) title.AdjRsq = "Factor Model Adjusted R-squared Values" else title.AdjRsq = " " 
      panel =  function(...){
        panel.abline(h=0,lty = 3)
        panel.xyplot(...)}
      plt = xyplot(adj.r2.xts,col = "blue", lwd =lwd, main = title.AdjRsq, type = "h",panel = panel,
                   scales = list(y = list(cex = axis.cex,relation="same"),x = list(cex =axis.cex)),par.strip.text = list(cex = stripText.cex),
                   strip.left = strip.custom(var.name = "AdjRsq", style = 1, strip.names = TRUE,strip.levels=FALSE ))
      print(plt) 
    }
    adj.r2.mean<- round(mean(adj.r2),digits = digits)
    names(adj.r2.mean) <- "Mean Adj R-Squared"
    out<- adj.r2.mean
    adj.r2 <- round(adj.r2,digits = digits)
    ret<- list("Adj.r-Squared" = adj.r2 )
  }
  if(rsqAdj && rsq)
  {
    if (isPlot && plt.type == 2)
    { 
      if(title) title.comb = "Factor Model R-squared Values" else title.comb = " " 
      panel =  function(...){
        panel.abline(h=0,lty = 3)
        panel.xyplot(...)}
      r2.combined = merge("Rsq" = r2.xts, "AdjRsq" =  adj.r2.xts)
      #       tsPlotMP(0.01*r2.combined,stripLeft = TRUE, scaleType = "same",
      #                color = "blue", yname = "", lwd = lwd, main = title.comb, type = "h", cex = 1.2)
      plt = xyplot(r2.combined,col = "blue", lwd =lwd, main = title.comb, type = "h",panel = panel,
                   scales = list(y = list(cex = axis.cex,relation="same"),x = list(cex =axis.cex)),par.strip.text = list(cex = stripText.cex),
                   strip.left = TRUE, strip = FALSE)
      print(plt) 
      
    }
    out<- c(r2.mean, adj.r2.mean)
    ret<- list("R-Squared"= r2, "Adj.R-Squared" = adj.r2)
  }
  
  if(isPrint)
  {
    print(c(out, ret))
  }else{
    print(out)
    invisible(c(out, ret))
  }
}

