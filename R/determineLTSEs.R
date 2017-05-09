#' determine the Long Term Spectral Envelope profile for a given framenumber in a given object.
#'
#' @param object the WAV-object
#' @param frameno the frameno
#' @param freqrange the frequency range to analyze.
#' @param framewindowsize
#' @param ltsewindow
#' @param width
#' @param bands the bands in which to compute the spectral envelope
#' @param debugoutput should debugging information be logged to the console?
#' @export

determineLTSEs <- function(object, frameno, freqrange, framewindowsize=6, ltsewindow=9, width, bands, debugoutput=FALSE) {
  range_min <- framerange(frameno=(max(0, frameno-ltsewindow)), width=width)
  if(debugoutput == TRUE) {message("Range min: ", range_min[1], " ", range_min[2])}
  if (is.na(range_min[1])) {
    message("determineLTSEs() was called with a frameno/framewindowsize that is out of range for the object")
    return(0)
  }
  if(debugoutput == TRUE) {
    message("Determine max for frame ", (min((length(object)/44100)/width, frameno+ltsewindow)))
  }
  range_max <- framerange(
    frameno=(min((length(object)/44100)/width, frameno+ltsewindow)),
    width=width)
  range <- c(min(range_min), max(range_max))

  if (debugoutput) {
    message("Range max: ", range_max[1], " ", range_max[2])
    message(".. determineLTSEs(frameno=", frameno,"), range: ", min(range)," - ", max(range))
  }
  ltses <- ltse(object = object, start=min(range), end=max(range), bands=bands, freqrange=freqrange)
  return(ltses)
}
