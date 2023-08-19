#' sag
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to find the sag ratio of a user defined current step from a current step experiment
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param step user defined current step (pA) that sag ratio will be calculated
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms, which is Derek's default. adjust your window accordingly.
#' @param vInfinity_window indices representing Vinfinity window that is used for calculation. defaults to last 100ms of iStep_window, which is Derek's default. adjust your window accordingly.
#' @param sag_window indices representing the sag window that is used for calculation. defaults to ~first 100ms of iStep_window, which is Derek's default. adjust your window accordingly.
#'
#' @return returns sag ratio value
#' 
#' @export
#'
sag <- function(x, step, iStep_window=6564:11561, baseline_window=1:6000, vInfinity_window=10001:11561, sag_window=6564:8001) {
  
  #creating variables that will be used in calculation
  #variables represent which rows i.e. voltage and current values to use in calculation
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
  
  steady_state <- getmode(x[vInfinity_window,(stepIndex)]) - getmode(x[baseline_window,(stepIndex)])
  sagPeak <- min(x[sag_window,(stepIndex)]) - getmode(x[baseline_window,(stepIndex)])
  result <- round(((sagPeak-steady_state) / sagPeak), digits = 4)
  
  return(result)
}
