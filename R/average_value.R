#' average_value
#'
#' @return Define a function to calculate the average value of the spline function
#' @export
#'
#' @examples
# Define a function to calculate the average value of the spline function
average_value <- function(spline, from, to) {
  integral <- integrate(spline, lower = from, upper = to)$value
  return(integral / (to - from))
}
