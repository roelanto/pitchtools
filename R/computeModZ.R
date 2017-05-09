computeModZ <- function(x, population) {
  return(((x - median(population)) * 0.6745) / mad(population))
}