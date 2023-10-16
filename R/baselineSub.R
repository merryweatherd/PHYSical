#' baselineSub
#' 
#' @author Derek Merryweather
#' 
#' @description This function is used to subtract values from a defined baseline window
#'
#' @param x Either a data frame or vector of values to be normalized
#' @param baseline_window Vector of the baseline window
#'
#' @return returns the baseline subtracted x
#' 
#' @export
#'
baselineSub <- function(x, baseline_window) {
  
  a <- stats::median(baseline_window)
  aa <- x-a
  return(aa)
  
}