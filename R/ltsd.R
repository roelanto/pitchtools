#' @export
ltsd <- function(object, frameno, nfft, freqrange, avgs, ltses, framewindowsize=6, ltsewindow=9, bands, debugoutput=FALSE) {
  # browser()
  valsum <- 0
  ltse_sum <- 0

  for (bandnum in  c(1:(length(bands)-1))) {
    avg <- avgs[bandnum]
    if (avg > 0) {
      ltse <- ltses[bandnum]
      if (debugoutput) {

        message("  bandnum: ", bandnum, " (", bands[bandnum], "-", bands[bandnum+1], "), avg: ", round(avg,3)," ltse: ", round(ltse,3))
      }
      valsum <- valsum + ((ltse^2)/ (avg^2))
      # message("  ", round((ltse^2),3), "(LTSE) / ", round((avg^2),3), "(AVG) = ",  ((ltse^2)/ (avg^2)), "; valsum: ", valsum)
    }
  }
  if (debugoutput) {

    message(".. returns valsum ", valsum)
  }

  return (10 * log(((1/sum(avgs>0))*valsum), base=10))
}
