#' Returns the range of a series of bands.
#'
#' Given a vector of band start points and a bandnumber, will return the start-point and end-point of that band.
#' @param bands A vector of bands.
#' @param num The bandnumber to be returned.
#' @return A vector with min and max values for the range
#' @export
#' @examples
#' bands <- c(10,20,30)
#' bandRange(bands, 1)

bandRange <- function(bands, num) {
  return (c(bands[num], bands[num+1]))
}
