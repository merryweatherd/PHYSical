#' spike_slope
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to find the slope to first spike in rheobase trace.
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param ap_threshold threshold voltage for detecting action potential. default is -20mV.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#'
#' @return returns array of slope values from start of current step to first action potential in rheobase trace
#' 
#' @export
#'
spike_slope <- function(x, ap_threshold=-20, iStep_window=6564:11561, baseline_window=1:6000) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  a <- x[iStep_window,vTrace_index]
  aa <- which(a >= ap_threshold, arr.ind = TRUE)
  
  if (is.na(aa[1]) == TRUE) {
    result <- NA
  }
  else {
    rheoCol <- unique(aa[,2])[1] #getting rheobase column
    if (stats::median(a[,rheoCol]) <= stats::median(x[baseline_window,rheoCol])) {
      rheoCol <- unique(aa[,2])[2]
    }
    preSpike <- aa[1,1]-3
    j <- a[1:preSpike,rheoCol]
    result <- (j[length(j)] - j[1]) / (length(j)/10)
  }
  
  return(result)
}










