computeTrimmedMean <- function(dataframe, z_crit) {
  # determine window but now throw out outliers
  H_n_dagger <- vector()
  for (i in 1:nrow(dataframe)) {
    hn <- dataframe[i,]$h
    lefttail <- ifelse((i-window.tailwidth) > 1, i-window.tailwidth, 1)
    righttail <- ifelse((i+window.tailwidth) <nrow(dataframe), i+window.tailwidth,nrow(dataframe))
    window <- dataframe[c(lefttail:righttail),]
    #    window.modz <- dataframe[c(lefttail:righttail),]$mod_z
    trimmed.window <- window[abs(window$mod_z) < abs(z_crit),]
    H_n_dagger[i] <- mean(trimmed.window$h)
  }
  return(H_n_dagger)
}