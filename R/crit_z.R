# replace: the correction on to be introduced values, -1200 in the Olness-article
crit_z <- function(population, replace.val=-1200, replace.pct = 20, threshold = 95, window.size=31) {
  crit_pop <- rep(population)
  crit_pop.modz <- computeModifiedZScore(crit_pop, windowsize = window.size)
  replace <- sample(1:length(crit_pop), length(crit_pop)/(100/replace.pct), replace = FALSE)
  if (length(replace) > 0) {
    crit_pop[replace] <- (population[replace]+replace.val)
    crit_pop.modz<-computeModifiedZScore(crit_pop, windowsize = window.size)
#    crit_pop.modz[replace] <- computeModZ(crit_pop[replace], crit_pop)
  } else {
    message(paste("in crit_z: specified replace.pct is so small that no random candidates have been selected for replacement by the replacement value. Crit_z will be very large. Increase replace.pct."))
  }
  sortorder <- order(abs(crit_pop.modz), decreasing = TRUE)
  #print(crit_pop.modz[sortorder])
  #print(sortorder)
  #  crit_pop <- crit_pop[sortorder]
  #  crit_pop.modz <- crit_pop.modz[sortorder]
  sortorder_counter <- 0
  for (i in sortorder) {
    sortorder_counter <- 1+sortorder_counter
    outliers <- get_outliers(population=crit_pop, z=crit_pop.modz[i])
#    print(paste("When z is ", crit_pop.modz[i], "the following values are outliers:"))
#    print(outliers)
    incommon <- Reduce(intersect, list(v1=replace, v2=outliers))
#    print(paste("There are ", length(incommon), "in common, ", 100.0*(length(incommon) / length(replace))))
    if (100.0*(length(incommon) / length(replace)) >= threshold) {
 #     print(paste("returning i=", i+1, "in sortorder", sortorder[i+1]))
      return(abs(0.1+crit_pop.modz[sortorder[sortorder_counter-1]]))
   #   return(abs(crit_pop.modz[sortorder[sortorder_counter]]))
    }
  }
}
