#' ap_frequency
#' 
#' @author Derek Merryweather
#' 
#' @description This function returns the action potential frequency from a user defined current step from a current step experiment.
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param step user defined current step (pA) that ap frequency will be calculated
#' @param ap_threshold user defined voltage threshold for action potentials. default it -20mV (Derek's default). adjust acorrdingly.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#' 
#' @return returns action potential frequency in Hz
#' 
#' @export
#'
ap_frequency <- function(x, step, ap_threshold=-1, iStep_window=6564:11561, baseline_window=1:6000) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #finding user defined current injection column
  #because i might switch up how many current steps I do and the delta increase with each current step i.e. 20pA current steps vs. 10pA current steps
  #switching this up will change the number of columns and therefore the index for where the user defined column is. these lines account for this issue
  #finds median value of each column's iStep_window and finds which column (trace) matches the user defined current trace
  #have to add one at end because time is always first column
  stepIndex <- which(round(matrixStats::colMedians(as.matrix(x[iStep_window,iTrace_index])) - 
                           matrixStats::colMedians(as.matrix(x[baseline_window,iTrace_index])),
                           digits = 0) >= (step-4) &
                     round(matrixStats::colMedians(as.matrix(x[iStep_window,iTrace_index])) -
                           matrixStats::colMedians(as.matrix(x[baseline_window,iTrace_index])),
                           digits = 0) <= (step+4)) +1
  
  #finding how many aps are in user defined current step
  a <- x[iStep_window,stepIndex]
  a <- a >= ap_threshold
  a <- a*a
  b <- a
  a <- data.table::shift(a,1)
  a <- b-a
  APnum <- table(a)
  if (is.na(APnum[3]==TRUE)) {
    APnum[3] <- as.numeric(0)
  }
  
  #returning firing frequency in Hz
  result <- round(APnum[3] / (length(iStep_window) / 10000), digits = 2)
  
  return(result)
}
