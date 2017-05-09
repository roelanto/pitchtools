get_outliers <- function(population=crit_pop, z=z) {
  mod_z_scores <- computeModZ(population, population) 
  is_outlier <- abs(mod_z_scores) >= abs(z)
  retval <- list()
  j<-1
  for (i in 1:length(is_outlier)) {
    if (is_outlier[i] == TRUE) {
      retval[[j]] <- i
      j <- 1+j
    }
  }
  return( as.vector(unlist(retval)))
}