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
    #if statement that ensures rheobase trace is coming from a positive or 0 current input current step. there are intances where CCh drives cells to fire even during a negative current step
    if (stats::median(a[,rheoCol]) <= stats::median(x[baseline_window,rheoCol])) {
      rheoCol <- unique(aa[,2])[2]
    }
    preSpike <- aa[1,1]-3 #getting the timepoint just before AP occurs. subtracting 3 (0.3ms) is somewhat arbitrary, but seems to work fine. subtracting three = 0.3ms for 10kHz recordings, for 20kHz recordings it would be 0.15ms
    j <- a[1:preSpike,rheoCol] #getting vector of voltage values from the start of the current step to just before the first AP
    result <- (j[length(j)] - j[1]) / (length(j)/10) #getting slope of start of current step to just before first AP
  }
  
  return(result)
}










