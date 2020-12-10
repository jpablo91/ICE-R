Parametric_CEAC <- function(data, wtp, tx, robust =T){
  # tx = 'loop'; data = data3; wtp = wtp
  df <- lapply(wtp, function(x){
    lambda <- x*data$effect - data$cost
    tx <- data %>%
      pull(eval(parse(text = tx)))
    
    if(robust == T){
      m <- lm(lambda~tx) %>%
        lmtest::coeftest(., vcov = sandwich::vcovHC(., "HC1"))
      beta <- m[2,1]
      pval <- m[2,4]
      pval <- ifelse(beta < 0 , (pval/2), 1 - (pval/2))
      c(beta = beta, pval =pval, wtp = x)
    } else{
      m <- lm(lambda~tx) %>%
        summary(.)
      beta <- m$coefficients[2,1]
      pval <- m$coefficients[2,4]
      pval <- ifelse(beta < 0 , (pval/2), 1 - (pval/2))
      c(beta = beta, pval =pval, wtp = x)
    }
    
    
  }) %>%
    do.call(rbind,.) %>% data.frame()
  return(df)
}