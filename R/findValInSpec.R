findValInSpec <- function(object, start=-1, end=-1, type="mean", bands, freqrange) {
  # Calculate the number of cores
  vals <- vector()
  if (start == -1) {
    start <- 0
    end <- length(object)/object@samp.rate
  }
  #message("Call specs with start ", start, " end ", end)
  #message("Object ", length(object))
  #message("Freqrange ", freqrange)
  specs <- as.data.frame(spec(object, flim=freqrange, from=start, to=end, plot=FALSE, norm=FALSE, scaled=FALSE))
  #browser()
  vals <- sapply(c(1:(length(bands)-1)), function(i, type, bands) {
    range <- bandRange(bands, i)
    #  message("i: ", i, " range: ", min(range), " - ", max(range))
    if (type=="mean") {
      mean(specs[(specs$x > min(range)) & (specs$x < max(range)), ]$y)
    } else if (type == "max") {
      max(specs[(specs$x > min(range)) & (specs$x < max(range)), ]$y)
    }

  }, type=type, bands=bands)
  return(vals)
}
