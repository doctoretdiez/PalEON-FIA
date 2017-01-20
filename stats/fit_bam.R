fit <- function(fulldata, grid, k_occ, k_pot, taxon, unc = 'bayes') {
# note that this code is basically the same as in twostagemodelup_bam.R for PLS biomass fitting, but with preprocessing to prepare inputs to bam() different

    library(mgcv)
    
    data <- fulldata %>% filter(PalEON == taxon)
    grid <- grid / 8000
    
  ############
  #  stage 1 #
  ############

    
    z <- cbind(data$count, data$total - data$count)
    x <- data$x/8000
    y <- data$y/8000
    
    print(system.time(occ <- bam(z ~ s(x, y, k = k_occ), family = 'binomial')))
    
    
  #  occsum <- print(summary(occ))
    pr_occ <- predict(occ, newdata = grid, type = 'response')
  
 
  ############
  #  stage 2 #
  ############

    data <- data %>% filter(count > 0)
    z <- log(data$biomass_avg)
    x <- data$x/8000
    y <- data$y/8000
    nsites <- data$count
    
    print(system.time(pot_w <- bam(z ~ s(x,y, k = k_pot), weights = nsites)))
    
  #  potsum<- print(summary(pot_w))
    pr_w <- exp(predict(pot_w, newdata= grid, type='response'))
     
    print(warnings())
  
  #####################
  # predicted biomass #
  #####################
  
    mass <- pr_occ * pr_w
  
    if(unc == 'bayes') {  # implement approx. Bayesian posterior draws following Wood (2004) section 4.8
        # added by CJP 3/5/16
        
        rmvn <- function(n, mu, sig) { ## MVN random deviates
            L <- mroot(sig); m <- ncol(L);
            t(mu + L %*% matrix(rnorm(m*n), m, n)) 
        }
        
        n <- 10000

        # posterior draws of (log) occupancy
        Xp <- predict(occ, newdata = grid, type="lpmatrix")
        draws_coef <- rmvn(n , coef(occ), occ$Vp) 
        draws_linpred <- Xp %*% t(draws_coef)
        draws_logocc <- draws_linpred - log(1 + exp(draws_linpred)) # log scale to add to log pot biomass
        # best to construct CIs on log scale and exponentiate endpoints
        
        # posterior draws of (log) potential biomass
        Xp <- predict(pot_w, newdata = grid, type="lpmatrix")
        draws_coef <- rmvn(n , coef(pot_w), pot_w$Vp) 
        draws_logpot <- Xp %*% t(draws_coef)

        draws_logb <- draws_logocc + draws_logpot
        # will want to save these posterior draws, either on log scale or biomass scale but
        # output not coded yet
        pp.sd <- apply(exp(draws_logb), 1, sd)

         # zero out uncertainty outside of range boundary where weird things are happening
        pp.sd[pp.sd/mass > 2 & mass < 1] <- 0

        cat("Note that the Bayesian uncertainty in areas well outside of the range of a taxon has unreasonably large uncertainty, likely due to numerical issues in estimating very small probabilities. Uncertainty in these locations has been set to zero artificially.")
    }

    preds <- data.frame(x = grid$x, y = grid$y, mean = mass, sd = pp.sd)
    return(preds)
}
