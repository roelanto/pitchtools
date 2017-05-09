# rename to computeModZInWindow
computeModifiedZScore<- function(x, windowsize=31) {
  window.tailwidth <- (windowsize-1)/2
  mod_z <- list()
  for (i in 1:length(x)) {
    hn <-x[i]
    lefttail <- ifelse((i-window.tailwidth) > 1, i-window.tailwidth, 1)
    righttail <- ifelse((i+window.tailwidth) <length(x), i+window.tailwidth,length(x))
    window <- x[c(lefttail:righttail)]
    mod_z[[i]] <- computeModZ(hn, window)
  }
  return(as.vector(unlist(mod_z)))
}
# input <- c(3.2, 3.4, 3.7, 3.7, 3.8, 3.9, 4, 4, 4.1, 4.2, 4.7, 4.8, 14, 15)
# computeModifiedZScore(input, windowsize=5) 
# should return: 1.7986667 -1.3490000 -0.6745000 -0.6745000 -0.4496667 -0.2248333  0.0000000  0.0000000  0.2248333  0.4496667  1.5738333  1.7986667 22.4833333 24.7316667