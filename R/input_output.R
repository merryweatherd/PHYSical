#' input_output
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to look at the input / output relationship of a current step experiment.
#'
#' @param x data frame containing a time column, voltage trace column(s), and current trace column(s).
#' @param ap_threshold user defined voltage threshold for action potentials. default it -20mV (Derek's default). adjust acorrdingly.
#' @param iStep_window indices representing the entire current step. defaults to ~650ms - 1250ms window, which is Derek's default. adjust your window accordingly.
#' @param baseline_window indices representing the baseline window that is used for calculation. defaults to first 600ms of each sweep, which is Derek's default. adjust your window accordingly.
#'
#' @return returns a data frame with two columns: input (pA) and output (APnum).
#' 
#' @export
#'
input_output <- function(x, ap_threshold=-1, iStep_window=6564:11561, baseline_window=1:6000) {
  
  #these variables are vectors containing index of voltage trace columns and current trace columns in data frame x
  vTrace_index <- c(2:(((ncol(x)-1)/2)+1))
  iTrace_index <- c((((ncol(x)-1)/2)+2):ncol(x))
  
  #creating empty vectors where values will be appended to result in for loop
  input <- c()
  output <- c()
  
  #counts for for loop
  #step, in this case, is not a user defined current step. it is a way to count and loop over all current steps
  count <- 0
  step <- 1
  
  #getting number of action potentials for each current step for output
  #getting the current step (pA) associated with output for input
  for (i in 1:length(iTrace_index)) {
    a <- x[iStep_window,(step+1)]
    a <- a >= -20
    a <- a*a
    b <- a
    a <- data.table::shift(a,1)
    a <- b-a
    APnum <- table(a)
    if (is.na(APnum[3]==TRUE)) {
      APnum[3] <- as.numeric(0)
    }
    
    output[i] <- signif(APnum[3], digits = 2)
    input[i] <- round((getmode(x[iStep_window, (iTrace_index[count+1])])) - (getmode(x[baseline_window, (iTrace_index[count+1])])), digits = 0)
    
    #cleaning input value to make it a round whole number i.e. 20pA instead of 22pA
    if (input[i] < 100 & input[i] > -100) {
      if (input[i] == -9 | input[i] == -11) {
        input[i] <- -10
      }
      else if (input[i] == 0 | input[i] == -1 | input[i] == 1) {
        input[i] <- 0
      }
      else if (input[i] == 9 | input[i] == 11 | input[i] == 8 | input[i] == 10) {
        input[i] <- 10
      }
      else {
        input[i] <- signif((getmode(x[iStep_window, (iTrace_index[count+1])])) - (getmode(x[baseline_window, (iTrace_index[count+1])])), digits = 1)
      }
    }
    
    else if (input[i] >= 100) {
      input[i] <- signif((getmode(x[iStep_window, (iTrace_index[count+1])])) - (getmode(x[baseline_window, (iTrace_index[count+1])])), digits = 2)
    }
    
    else if (input[i] <= -100) {
      input[i] <- signif((getmode(x[iStep_window, (iTrace_index[count+1])])) - (getmode(x[baseline_window, (iTrace_index[count+1])])), digits = 2)
    }
    
    if (input[i] == 9) {
      input[i] <- 10
    }
    
    count <- count+1
    step <- step+1
  }
  
  #remember to add Cell_ID, Cell_type, Bath_solution, myHolding_current, and mySpike_type to dataframe after you return this
  result <- data.frame(input, output)
  return(result)
}
