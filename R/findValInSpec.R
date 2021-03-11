aggregateSpecsInRange <- function(range, type, specs) {
  minrange <- min(range)
  maxrange <- max(range)
  specsofinterest <- specs[(specs$x > minrange) & (specs$x < maxrange), ]
  #  message("i: ", i, " range: ", min(range), " - ", max(range))
  if (type=="mean") {
    mean(specsofinterest$y)
  } else if (type == "max") {
    suppressWarnings(max(specsofinterest$y))
  }
}


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
  specsmatrix <- spec(object, flim=freqrange, fftw=TRUE, from=start, to=end, plot=FALSE, norm=FALSE, scaled=FALSE)
  specs <- as.data.frame(specsmatrix)
  #browser()
  vals <- sapply(c(1:(length(bands)-1)), function(i, type, bands) {
    range <- bandRange(bands, i)
    aggregateSpecsInRange(range, type, specs)
  }, type=type, bands=bands)
  return(vals)
}
