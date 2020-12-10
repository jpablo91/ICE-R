plotINB <- function(data, lambdas){
  INB <- lapply(lambdas, getINB, data = data) %>%
    do.call(rbind, .) %>% data.frame()

    # Create NB_i variable
    P <- ggplot(aes(x = lambdas, y = INB.tx), data = INB) +
      geom_path(col = 'grey30') +
      geom_hline(yintercept = 0, col = 'red2', linetype="dashed") +
      geom_label(aes(label = paste0("NB:", round(INB.tx, 2))), size= 3, parse = TRUE) +
      ylab("INB") + xlab(expression(lambda))

  
  return(P)
}
