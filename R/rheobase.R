#' rheobase
#' 
#' @author Derek Merryweather
#' 
#' @description function to extract rheobase value from current step experiment
#'
#' @param x dataframe containing time column, voltage trace column(s), and current trace column(s)
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordinlgy.
#' @param ap_threshold threshold voltage for detecting action potential. default is -20mV.
#'
#' @return returns a single numeric value that is the rheobase of current step experiment
#' @export
#' 
rheobase <- function(x, iStep_window=6564:11561, baseline_window=1:6000, ap_threshold=-20) {
  
  #creating variables that will be used in calculation
  #variables represent which rows i.e. voltage and current values to use in calculation
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #finds any values above -20mV
  a <- x[iStep_window,vTrace_index]
  aa <- which(a >= ap_threshold, arr.ind = TRUE)
  if (is.na(aa[1]) == TRUE) {
    result <- NA
  }
  else {
    aaa <- iTrace_index[aa[1,2]]
    result <- round((x[iStep_window[1],aaa] - x[baseline_window[1],aaa]), digits = 0)
  }
  return(result)
}