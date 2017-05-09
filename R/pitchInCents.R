pitchInCents <- function(hz) {
  return(3986*1.0005778 * (log(hz, base=10)/1))
}