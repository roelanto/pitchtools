#' determine the averages, to facilitate noise profile creation.
#'
#' @param object the WAV-object
#' @param bands the bands in which to compute the spectral envelope
#' @param freqrange the frequency range to analyze.
#' @param start the start position in seconds
#' @param end the end position in seconds
#' @export
#'
determineAverages <- function(object, bands, freqrange=c(0.4, 200), start=-1, end=-1) {
  return(findValInSpec(object=object, type="mean", bands=bands, freqrange=freqrange, start=start, end=end))
}
