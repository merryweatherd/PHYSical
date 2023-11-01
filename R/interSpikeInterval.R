#' interSpikeInterval
#' 
#' @author Derek Merryweather
#' 
#' @description kj
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param step user defined current step to calculate ISI
#' @param ap_threshold threshold voltage for detecting action potential. default is -20mV.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#'
#' @return returns vector of ISI values in ms for user defined current step. first element will be the spike delay time
#' 
#' @export
#'
interSpikeInterval <- function(x, step, ap_threshold=-20, iStep_window=6564:11561, baseline_window=1:6000) {
  
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
  
  a <- x[iStep_window,stepIndex]
  aa <- which(a >= ap_threshold, arr.ind = TRUE)
  
  #returning NA if no aps were found/elicited
  #if ap is found, finds which column (iTrace_index) it occurred
  if (is.na(aa[1]) == TRUE) {
    result <- 'noAP'
  }
  
  else {
  
    #isolating where single APs are occuring in the trace
    #shifts array to find number of action potentials and where they occur
    t <- a >= ap_threshold #finds values above ap threshold
    t <- t*t #converts TRUE and FALSE values into 1s and 0s
    tt <- t #new variable for shift function reference frame
    t <- data.table::shift(t,1) #shifts every number one point in time. rheo_vec[1] == NA and last value in rheo_vec is lost. length(rheo_vec) == length(b)
    t <- tt-t #1 is the rising edge of the ap, -1 is the falling edge of the ap
    
    apIndex <- which(t==1) #gives index/indices where AP occurred
    
    if (length(apIndex)==1) {
      result <- NA
    }
    else if (length(apIndex>1)) {
      isi_vec <- c(apIndex[1])
      for (i in 2:(length(apIndex))) {
        isi_vec[i] <- apIndex[i] - apIndex[i-1]
      }
      result <- isi_vec / 10 #converting isi_vec values into ms
    }
  }
  return(result)
}











