#' Function to plot the CEAC
#' 
#' @param data A data.frame containing the information of cost and effect.
#' @param strata The name of the column that indicates the stratification groups.
#' @param N_Sim Number of bootstraps.
#' @param NB a vector containing the values for the willingness to pay
#' @param full if False only a plot is returned, if T the data from bootstraps is also returned
#' @export
CEAC <- function(data, strata, N_Sim = 1000, NB, full = F){
  d <- bs_CI(data = data, strata = strata, N_Sim = N_Sim, full = T)
  inb <- lapply(NB, function(x){
    sum((x*d$BootRegression$delta_e - d$BootRegression$delta_c) > 0) / N_Sim
  }) %>% unlist()
  df <- data.frame(NB, inb)
  
  P <- df %>%
    ggplot(aes(x=NB, y=inb, label = round(inb, 4))) +
    geom_path()+
    geom_label()+
    ylab("Probability cost-effective") + xlab(expression(lambda))+
    ylim(0, 1)
  
  # P <- plot(inb~NB, type = 'o')
  
  if(full==T){
    return(list(INB = df, plot = P))
  }else{
    return(P)
  }
}
