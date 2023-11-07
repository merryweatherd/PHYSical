#' spike_width
#' 
#' @author Derek Merryweather
#' 
#' @description function that finds with average width of action potentials during a current step trace
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param ap_threshold threshold voltage for detecting action potential.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#'
#' @return returns the mean action potential width.
#' 
#' @export
#'
spike_width <- function(x, ap_threshold=-30, iStep_window=6564:11561, baseline_window=1:6000) {
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  a <- x[iStep_window, vTrace_index]
  aa <- which(a >= ap_threshold, arr.ind = TRUE)
  
  #returning NA if no aps were found/elicited
  #if ap is found, finds which column (iTrace_index) it occurred
  if (is.na(aa[1]) == TRUE) {
    result <- NA
  }
  else {
    rheoCol <- unique(aa[,2])[1] #getting rheobase column
    if (stats::median(a[,rheoCol]) <= stats::median(x[baseline_window,rheoCol])) {
      rheoCol <- unique(aa[,2])[2]
    }
    if (rheoCol==ncol(a)) {
      widthCol <- rheoCol
    }
    else if (rheoCol < ncol(a)) {
      widthCol <- rheoCol+3
      if (widthCol > ncol(a)) { #add these lines if you want to make the trace ISI is calculated from more than the next step after the rheobase step
        widthCol <- rheoCol+2
      }
      if (widthCol > ncol(a)) {
        widthCol <- rheoCol+1
      }
    }
    #isolating where single APs are occuring in the trace
    #shifts array to find number of action potentials and where they occur
    t <- a[,widthCol] >= ap_threshold #finds values above ap threshold
    t <- t*t #converts TRUE and FALSE values into 1s and 0s
    tt <- t #new variable for shift function reference frame
    t <- data.table::shift(t,1) #shifts every number one point in time. rheo_vec[1] == NA and last value in rheo_vec is lost. length(rheo_vec) == length(b)
    t <- tt-t #1 is the rising edge of the ap, -1 is the falling edge of the ap
    
    if (length(which(t==1)) != length(which(t==-1))) {
      b <- length(which(t==-1))
      bb <- which(t==1)[1:b]
      result <- mean(which(t==-1) - bb)
    }
    else {
      result <- mean(which(t==-1) - which(t==1))
      }
  }
  return(result)
}







