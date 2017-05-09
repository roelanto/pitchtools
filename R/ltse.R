ltse <- function(object, start, end, freqrange, bands) {
  return(findValInSpec(object=object, type="max", bands=bands, freqrange=freqrange, start=start, end=end))
}
