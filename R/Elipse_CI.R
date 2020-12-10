# Create the function
Elipse_CI <- function(data, plot = F, confidence = 0.95){
  # confidence = 0.9
  data.c <- Fiellers_CI(data, T)
  
  N0 <- nrow(subset(data, tx == 0))
  N1 <- nrow(subset(data, tx == 1))
  N <- N0+N1
  theta <- pi*2*(1:N-1)/(N0 + N1 -1)
  part1_c95 <- sqrt(-2*log(1-confidence))*data.c$se_dc
  part1_e95 <- sqrt(-2*log(1-confidence))*data.c$se_de
  part2_c <- cos(theta-acos(data.c$corr)/2)
  part2_e <- cos(theta+acos(data.c$corr)/2)
  delta_c95 <- part1_c95*part2_c + data.c$dc
  delta_e95 <- part1_e95*part2_e + data.c$de
  
  df <- data.frame(delta_c95, delta_e95)
  
  if(plot == T){
    df %>%
      ggplot() +
      geom_path(aes(delta_e95, delta_c95), col = 'blue4') +
      geom_point(aes(x=de, y=dc), data = data.c, col = 'red4') +
      geom_label(aes(x=de, y=dc, label = paste("ICER: ", round(dc/de, 4))), nudge_y = data.c$dc*0.25, data = data.c) +
      geom_hline(yintercept = 0, col = 'red3') +
      geom_vline(xintercept = 0, col = 'red3')
    #
    # plot(delta_e95, delta_c95, type = 'l', col = 'blue4')
    # points(dc~de, data = data.c, pch = 18, col = 'red4')
    # abline(v = 0, col = 'red3')
    # abline(h = 0, col = 'red3')
  } else{
    return(df)
  }
}