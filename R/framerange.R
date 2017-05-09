#' Returns the range of frames with frameno, assuming each frame has width 'width'.
#'
#' @param width the width of each frame
#' @param frameno which frame to return the range for
#' @return will return the range that starts at the start of the frame and ends just before the next frame.
#' @export
#'
#'
framerange <- function(width, frameno) {
  return(c((width*frameno), width*(frameno+1)))
}
