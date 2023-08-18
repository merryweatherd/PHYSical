#' getmode
#' 
#' @author Derek Merryweather
#' 
#' @description This function gets the mode of a vector x
#' 
#' @param x x must be a numeric vector
#'
#' @return returns a single value that is the mode of vector x
#' @export
#'
getmode <- function(x) {
  uniqv <- unique(x)
  result <- uniqv[which.max(tabulate(match(x, uniqv)))]
  
  return(result)
}