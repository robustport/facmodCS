#' @title Time Series Plots
#' 
#' @description Plot time series with specific plotting parameters 
#'
#' @importFrom lattice panel.abline xyplot panel.xyplot
#' @importFrom xts xts
#' 
#' @param ret an time series exposure/return object 
#' @param add.grid logical variable.If 'TRUE', type = c('l', 'g'); If 'FALSE', type = c('l')
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
#' @param type character. type of the plot; 'l' denotes a line, 'p' denotes a point, and 'b' and 'o' both denote both together.deafault is 'l'.
#' @param yname character or expression giving label(s) for the y-axis 
#' @param Pct Pct controls if use the percentage value. 
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param stripLeft logical variable to choose the position of strip, 'TRUE' for drawing strips on the left of each panel, 'FALSE' for drawing strips on the top of each panel
#' @param main Typically a character string or expression describing the main title. 
#' @param lwd The line width, a positive number, defaulting to 1
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param color A specification for the default plotting color. Default is black.
#' @param zeroLine logical varible to choose add a dotted horizontal line at the zero vertical distance
#' @param panel function to add customized lines to the lattice plot. See examples.
#' @return {No return value, called for plotting}
#' @author Douglas Martin, Lingjie Yi
#' @example
#' args(tsPlotMP)
#' @export
# Lattice type time series plotting function
tsPlotMP = function (ret, add.grid = FALSE, layout = NULL, type = "l", yname = "RETURNS (%)", 
		Pct = FALSE, scaleType = "free", stripLeft = TRUE, main = NULL, lwd = 1, 
		stripText.cex = 1, axis.cex = 1, color = "black", zeroLine = TRUE, panel = NULL) 
{
	strip.left = stripLeft
	strip = !strip.left
	if (add.grid) {
		type = c("l", "g")
	}
	else {
		type = type
	}
	
	if (zeroLine) {
		panel = function(...) {
			panel.abline(h = 0, lty = 3)
			panel.xyplot(...)
		}
	}
	if(Pct) ret = ret*100
	pl = xyplot(ret, par.strip.text = list(cex = stripText.cex), 
			type = type, xlab = "", ylab = list(label = yname, cex = stripText.cex), 
			lwd = lwd, scales = list(y = list(cex = axis.cex, relation = scaleType, 
							rot = 0), x = list(cex = axis.cex)), layout = layout, 
			main = main, col = color, strip = strip, strip.left = strip.left, 
			panel = panel)
	print(pl)
}
