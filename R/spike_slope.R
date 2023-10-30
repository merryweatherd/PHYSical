#' spike_slope
#' 
#' @author Derek Merryweather
#' 
#' @description This function 
#'
#' @param x 
#' @param ap_threshold threshold voltage for detecting action potential. default is -20mV.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#' @param sliding_window how much time the rolling window covers in ms
#'
#' @return returns array of 
#' @export
#'
#' 
spike_slope <- function(x, ap_threshold=-20, iStep_window=6564:11561, baseline_window=1:6000, sliding_window=0.5) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #converting sliding window units to 10kHz
  sliding_window <- sliding_window*10
  sliding_window2 <- sliding_window
  
  a <- x[iStep_window,vTrace_index]
  aa <- which(a >= -20, arr.ind = TRUE)
  slp_array <- c()
  if (is.na(aa[1]) == TRUE) {
    result <- NA
  }
  else {
    aaa <- aa[1,2] #getting rheobase column
    j <- x[1:aa[1,1],aaa]
    remainder <- length(j)%%sliding_window
    count <- 1
    for (i in 1:((length(j)-remainder)/sliding_window)) {
      slp_array[i] <- j[sliding_window2] / j[count]
      count <- count + sliding_window2
      sliding_window2 <- sliding_window2+sliding_window2
    }
    result <- as.data.frame(slp_array)
  }
  
  result$time_ms <- seq(from=0, to=(length(j)-remainder)-sliding_window, by=sliding_window)
  result$time_ms <- result$time_ms/10
  
  return(result)
  
}










