#### FUNCTION for regression of the samples ####
bs_reg <- function(Bs, formula){
  DF <- lapply(Bs, function(x){
    lm(formula, data = x) %>%
      .$coefficients
  }) %>%
    do.call(rbind,.) %>%
    data.frame()
  return(DF)
}
