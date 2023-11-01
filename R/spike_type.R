#' spike_type
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to find the spike type of a neuron (burst or regular) from the rheobase trace of a current step experiment.
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param ap_threshold user defined voltage threshold for action potentials. default it -20mV (Derek's default). adjust acorrdingly.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms, which is Derek's default. adjust your window accordingly.
#' @param vInfinity_window indices representing Vinfinity window that is used for calculation. defaults to last 100ms of iStep_window, which is Derek's default. adjust your window accordingly.
#' @param burst_width user defined threshold value indicating how far apart in time each ap is in a "burst" of aps. default set to 150 (150=15ms), which is Derek's default. adjust your window accordingly.
#'
#' @return returns character "regular" or "burst"
#' 
#' @export
#'
spike_type <- function(x, ap_threshold=-20, iStep_window=6564:11561, baseline_window=1:6000, vInfinity_window=10001:11561, burst_width=150) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #finds any values above action potential threshold (user defined)
  #aa returns the rows and columns where ap threshold was met
  a <- x[iStep_window,vTrace_index]
  aa <- which(a >= ap_threshold, arr.ind = TRUE)
  
  #returning NA if no aps were found/elicited
  #if ap is found, finds which column (iTrace_index) it occurred
  if (is.na(aa[1]) == TRUE) {
    result <- 'noAP'
  }
  
  else {
    rheoCol <- unique(aa[,2])[1] #getting rheobase column
    if (stats::median(a[,rheoCol]) <= stats::median(x[baseline_window,rheoCol])) {
      rheoCol <- unique(aa[,2])[2]
    }
    
    #gets rheobase trace values (mV)
    #shifts array to find number of action potentials and where they occur
    rheo_vec <- a[,rheoCol]
    rheo_vec <- rheo_vec >= ap_threshold #finds values above ap threshold
    rheo_vec <- rheo_vec*rheo_vec #converts TRUE and FALSE values into 1s and 0s
    b <- rheo_vec #new variable for shift function reference frame
    rheo_vec <- data.table::shift(rheo_vec,1) #shifts every number one point in time. rheo_vec[1] == NA and last value in rheo_vec is lost. length(rheo_vec) == length(b)
    rheo_vec <- b-rheo_vec #1 is the rising edge of the ap, -1 is the falling edge of the ap
    
    apIndex <- which(rheo_vec==1)
    apIndex_t <- apIndex - apIndex[1]
    
    count <- 0
    for (i in 1:length(apIndex_t)) {
      if (is.na(apIndex_t[i+1])) {
        break
      }
      if (apIndex_t[i+1]-apIndex_t[i] <= burst_width) {
        count <- count+1
      }
      else if (apIndex_t[i+1]-apIndex_t[i] > burst_width) {
        break
      }
    }
    if (count==0) {
      result <- 'regular'
    }
    else {
      result <- 'burst'
    }
    # for if you want to know how many spikes there were in the burst
    # else if (count==1) {
    #   result <- 'burst/doublet'
    # }
    # else if (count==2) {
    #   result <- 'burst/triplet'
    # }
    # else if (count==3) {
    #   result <- 'burst/quadruplet'
    # }
    # else if (count==4) {
    #   result <- 'burst/quintuplet'
    # }
    # else if (count>=5) {
    #   result <- 'inf/plateau'
    # }
  }
  
  return(result)
}
