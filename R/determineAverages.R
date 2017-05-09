determineAverages <- function(wav, bands, freqrange=c(0.4, 200), start=-1, end=-1) {
  return(findValInSpec(object=wav, type="mean", bands=bands, freqrange=freqrange, start=start, end=end))
}
