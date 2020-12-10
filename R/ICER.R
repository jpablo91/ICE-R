ICER <- function(data){
  # regression for the cost
  mC <- lm(cost~tx, data = data)
  dC <- mC$coefficients[2] # difference in cost
  # regression for the effect
  mE <- lm(effect~tx, data = data)
  dE <- mE$coefficients[2] # difference in effect
  # ICER:
  icer <- c(ICER = dC/dE)
  
  return(icer)
}
