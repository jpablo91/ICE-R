#' Function to calculate regression from the bootstraps
#' 
#' @param Bs A list resulting from the function Boot_sample
#' @param formula The formula to use for the regression
#' @export
bs_reg <- function(Bs, formula){
  DF <- lapply(Bs, function(x){
    lm(formula, data = x) %>%
      .$coefficients
  }) %>%
    do.call(rbind,.) %>%
    data.frame()
  return(DF)
}
