#' inputR
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to find the input resistance value from a current step experiment.
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param step user defined current step (pA) used to calculate input resistance.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms, which is Derek's default. adjust your window accordingly.
#' @param vInfinity_window indices representing Vinfinity window that is used for calculation. defaults to last 100ms of iStep_window, which is Derek's default. adjust your window accordingly.
#'
#' @return returns numeric input resistance value
#' 
#' @export
#'
inputR <- function(x, step, iStep_window=6564:11561, baseline_window=1:6000, vInfinity_window=10001:11561) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #finding user defined current injection column
  #because i might switch up how many current steps I do and the delta increase with each current step i.e. 20pA current steps vs. 10pA current steps
  #switching this up will change the number of columns and therefore the index for where the user defined column is. these lines account for this issue
  #finds median value of each column's iStep_window and finds which column (trace) matches the user defined current trace
  #have to add one at end because time is always first column
  #importantly, this stepIndex alone should be used to access the voltage column from x, not the current column. must do iTrace_index[stepIndex-1] to get current column
  stepIndex <- which(round(matrixStats::colMedians(as.matrix(x[iStep_window,iTrace_index])) - 
                             matrixStats::colMedians(as.matrix(x[baseline_window,iTrace_index])),
                           digits = 0) >= (step-4) &
                       round(matrixStats::colMedians(as.matrix(x[iStep_window,iTrace_index])) -
                               matrixStats::colMedians(as.matrix(x[baseline_window,iTrace_index])),
                             digits = 0) <= (step+4)) +1
  
  #getting delta I value
  dI <- getmode(x[iStep_window, iTrace_index[stepIndex-1]]) - getmode(x[baseline_window, iTrace_index[stepIndex-1]])
  print(dI)
  
  #getting delta V value
  dV <- getmode(x[vInfinity_window, stepIndex]) - getmode(x[baseline_window, stepIndex])
  print(dV)
  
  #calculating Rin by R=deltaV/deltaI
  result <- round((dV / dI) * 1000, digits = 0)
  
  return(result)
}
