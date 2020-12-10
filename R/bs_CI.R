### Function for bootstrapping CIs ########
bs_CI <- function(data, strata, N_Sim, full = F){
  Bs <- Boot_sample(data = data, strata = strata, N_Sim = N_Sim)
  
  BSReg <- cbind(bs_reg(Bs = Bs, formula = cost~tx),
                 bs_reg(Bs = Bs, formula = effect~tx)) %>%
    data.frame() %>%
    rename(delta_c = tx, delta_e = tx.1) %>%
    mutate(ICER = delta_c/delta_e)  %>%
    select(delta_c, delta_e, ICER)
  
  CI <- BSReg %>%
    arrange(ICER) %>%
    slice(floor(N_Sim*.025), floor(N_Sim*0.975))
  
  rownames(CI) <- c('LB', 'UB')
  if(full == T){
    return(list(CI = CI, BootRegression = BSReg))
  } else{
    return(CI)
  }
}
