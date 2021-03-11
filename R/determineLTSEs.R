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

determineLTSEs <- function(object, frameno, freqrange, framelengthInSeconds=0.01, windowlengthInFrames=6, width, bands, debugoutput=FALSE) {
  framelengthInSamples <- object@samp.rate * framelengthInSeconds
  rangeInSamples <- c((frameno - windowlengthInFrames) * framelengthInSamples, (frameno + windowlengthInFrames) * framelengthInSamples)
  rangeInSeconds <- rangeInSamples / object@samp.rate


#  range_min <- framerange(frameno=(max(0, frameno-ltsewindow)), width=width)
#  if(debugoutput == TRUE) {message("Range min: ", range_min[1], " ", range_min[2])}
#  if (is.na(range_min[1])) {
#    message("determineLTSEs() was called with a frameno/framewindowsize that is out of range for the object")
#    return(0)
#  }
#  if(debugoutput == TRUE) {
#    message("Determine max for frame ", (min((length(object)/44100)/width, frameno+ltsewindow)))
#  }
#  range_max <- framerange(
#    frameno=(min((length(object)/44100)/width, frameno+ltsewindow)),
#    width=width)
#  range <- c(min(range_min), max(range_max))#

  start <- max(0, min(length(object)/object@samp.rate, rangeInSeconds))
  end <- min(length(object)/object@samp.rate, max(rangeInSeconds))
  if (debugoutput) {
  #  message("Range max: ", range_max[1], " ", range_max[2])
    message(".. determineLTSEs(frameno=", frameno,"), range in samples: ", min(rangeInSamples)," - ", max(rangeInSamples), " in seconds: ", min(rangeInSeconds), " - ", max(rangeInSeconds))
    message(".. computed start/end: ", start, " - ", end)
  }
  ltses <- ltse(object = object, start=start, end=end, bands=bands, freqrange=freqrange)
  return(ltses)
}
