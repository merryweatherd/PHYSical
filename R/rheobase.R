#' rheobase
#' 
#' @author Derek Merryweather
#' 
#' @description This function extracts the rheobase value from a current step experiment
#'
#' @param x data frame containing time column, voltage trace column(s), and current trace column(s)
#' @param ap_threshold threshold voltage for detecting action potential. default is -20mV.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#' 
#'
#' @return returns a single numeric value that is the rheobase of current step experiment
#' @export
#' 
rheobase <- function(x, ap_threshold=-1, iStep_window=6564:11561, baseline_window=1:6000) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #finds any values above action potential threshold (user defined)
  #j and jj are variables that make this calculation NOT run on negative current steps. should run on 0pA and positive pA current steps
  #aa returns the rows and columns where ap threshold was met
  j <- colMeans(x[iStep_window,iTrace_index]) - colMeans(x[baseline_window,iTrace_index])
  jj <- which(j>-7)[1]
  a <- x[iStep_window, vTrace_index[jj:length(vTrace_index)]]
  aa <- which(a >= ap_threshold, arr.ind = TRUE)

  #returning NA if no aps were found/elicited
  #if ap is found, finds which column (iTrace_index) it occurred. then gets the current step value.
  if (is.na(aa[1]) == TRUE) {
    result <- NA
  }
  
  #calculating rheobase value (result)
  #jj needs to be subtracted by 1 because indexing from iTrace_index without subtraction will make you one current step ahead. especially bad when rheobase is the final current step in the experiment, this will cause function to throw a NA or NAN
  else {
    aaa <- iTrace_index[aa[1,2] + (jj-1)]
    result <- getmode(x[iStep_window, aaa]) - getmode(x[baseline_window,aaa])
    
    #returns a rounded current step value. reason for this is because i am calculating values directly from clampex data, which may return, for example: 121 instead of 120
    #this ensures a rounded number with correct significant figures
    if (floor(log10(result)) + 1 == 3) {
      result <- signif(result, digits = 2)
    }
    else if (floor(log10(result)) + 1 == 2) {
      result <- signif(result, digits = 1)
    }
  }
  return(result)
}

