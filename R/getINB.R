## FUNCTION to get lambda
getINB <- function(data, lambda){
  # Create NB_i variable
  data <- data %>%
    mutate(NBi = effect*lambda - cost)
  m <- lm(NBi~tx, data)
  INB <- m$coefficients[2]
  return(c(lambda = lambda, INB = INB))
}