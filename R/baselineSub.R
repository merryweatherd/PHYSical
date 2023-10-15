#' baselineSub
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to get baseline subtracted values from a vector of numeric values. Usually used for getting change in resting/baseline membrane potential across time.
#'
#' @param x data frame or vector containing numeric values that are to be subtracted
#' @param baseline vector of numeric values that will make up the baseline value
#'
#' @return returns vector of subtracted values from a user determined baseline
#' @export
#'
baselineSub <- function(x, baseline) {
  
  if (is.data.frame(baseline)==TRUE) {
    a <- reshape2::melt(baseline, value.name = 'value')
    aa <- stats::median(a$value)
    return(x - aa)
  }
  
  else if (is.vector(baseline)==TRUE) {
    a <- stats::median(baseline)
    return(x - a)
  }
  
}