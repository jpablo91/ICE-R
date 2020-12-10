Fiellers_CI <- function(data, full = F){
  # regression on cost
  mC <- lm(cost~tx, data) %>%
    lmtest::coeftest(., vcov = sandwich::vcovHC(., "HC1")) # allowing variables to be different (No Homoestadisticity assumed)
  # regression on effect
  mE <- lm(effect~tx, data) %>%
    lmtest::coeftest(., vcov = sandwich::vcovHC(., "HC1"))
  
  # Create the variables
  data <- data %>% 
    mutate(dc = mC[2,1], # Coefficient for cost
           # dc2 = dc^2, # coeff ^2
           se_dc = mC[2,2], # get SE of the coeff
           de = mE[2,1], # Coefficient for effect
           # de2 = de^2,
           se_de = mE[2,2],
           df = n() - 2) %>%
    group_by(tx) %>%
    mutate(rho = cor(cost, effect), 
           se_c = sd(cost),
           se_e = sd(effect),
           Ni = n())
  
  data.c <- data %>% # correlation test
    distinct(tx, dc, de, df, se_dc, se_de, rho, se_c, se_e, Ni) %>%
    ungroup() %>%
    mutate(D = (rho*se_c*se_e)/Ni) %>% # cov = (rho_1*se_c1*se_e1) / n1 + (rho_0*se_c0*se_e0) / n0
    mutate(cov = D[1] + D[2],
           corr = cov/(se_dc*se_de),
           t = qt(p = 1 - 0.025, df = df), # gen corr = cov / (se_dc*se_de)
           MM = de*dc - t^2*corr*se_de*se_dc,
           NN = de^2 - t^2*se_de^2,
           OO = dc^2 - t^2*se_dc^2,
           ll = (MM - (MM^2 - NN*OO)^0.5) / NN,
           ul = (MM + (MM^2 - NN*OO)^0.5) / NN) %>%
    distinct(dc, de, se_dc, se_de, ll, ul, cov, corr)
  if(full == T){
    return(data.c)
  }else{
    data.c %>%
      select(ll, ul) %>%
      return()
  }
}