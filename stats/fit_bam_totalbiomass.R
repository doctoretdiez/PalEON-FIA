#to estimate the total biomass per cell, we are using the following biomass_by_cell code from the end of the preprocess_biomass.R 
#run the following code after running lines 124-141 in preprocess_biomass.R - these are the lines that calculate
#average biomass for all the taxa per cell.


library(mgcv)

data <- fulldata
grid <- grid/8000


############
#  stage 2 #
############

data <- data %>% filter(count > 0)
z <- log(data$avg_biomass)
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

mass <- pr_w #removed the "* pr_occ"

if(unc == 'bayes') {  # implement approx. Bayesian posterior draws following Wood (2004) section 4.8
  # added by CJP 3/5/16
  
  rmvn <- function(n, mu, sig) { ## MVN random deviates
    L <- mroot(sig); m <- ncol(L);
    t(mu + L %*% matrix(rnorm(m*n), m, n)) 
  }
  
  n <- 10000
  
  # posterior draws of (log) occupancy
  #Xp <- predict(occ, newdata = grid, type="lpmatrix")
  #draws_coef <- rmvn(n , coef(occ), occ$Vp) 
  #draws_linpred <- Xp %*% t(draws_coef)
  #draws_logocc <- draws_linpred - log(1 + exp(draws_linpred)) # log scale to add to log pot biomass
  # best to construct CIs on log scale and exponentiate endpoints
  
  # posterior draws of (log) potential biomass
  Xp <- predict(pot_w, newdata = grid, type="lpmatrix")
  draws_coef <- rmvn(n , coef(pot_w), pot_w$Vp) 
  draws_logpot <- Xp %*% t(draws_coef)
  
  draws_logb <- draws_logpot #removed "draws_logocc +" 
  # will want to save these posterior draws, either on log scale or biomass scale but
  # output not coded yet
  pp.sd <- apply(exp(draws_logb), 1, sd)
  
  # zero out uncertainty outside of range boundary where weird things are happening
  pp.sd[pp.sd/mass > 2 & mass < 1] <- 0
  
  cat("Note that the Bayesian uncertainty in areas well outside of the range of a taxon has unreasonably large uncertainty, likely due to numerical issues in estimating very small probabilities. Uncertainty in these locations has been set to zero artificially.")
}

preds <- data.frame(x = grid$x*8000, y = grid$y*8000, mean = mass, sd = pp.sd)
#return(preds)

preds2 = merge(preds,albers,by=c('x','y'))

write.csv(preds2[, c('x', 'y', 'cell','mean')], paste0("C:/Users/jmurray7/Desktop/PalEON-FIA/stats/output/total.prediction_v0.1.csv"))
write.csv(preds2[, c('x', 'y', 'cell', 'sd')], paste0("C:/Users/jmurray7/Desktop/PalEON-FIA/stats/output/total.uncertainty_v0.1.csv"))



png(filename = paste("./stats/output/figures/total-biomass-sdhist_v0.1.png", sep=''),  height = 768, width=1024)
hist(pp.sd,nclass=50,main = paste('Standard Deviation'),xlab = paste('St.dev of total biomass per cell'))
dev.off()

#makes continuous/smooth maps of predicted biomass
library(ggplot2)
p1a = ggplot(preds, aes(x=x, y=y,colour=mass))+ geom_point(shape=15, solid=TRUE,cex=3)+
  scale_color_gradient("Predicted", low = "white", high = "blue",           
                       limits=range(preds$mean)
                       #breaks=brk
  ) + 
  geom_point(shape=15, solid=TRUE,cex=3)  + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
                                                  axis.text.y = element_text(size=25), 
                                                  axis.title.x = element_text(size=25),
                                                  axis.title.y = element_text(size=25)) + ggtitle(paste("total biomass per cell"))

png(paste("./stats/output/figures/total-biomasspercell-mean-smooth_v0.1.png", sep = " "),   height = 768, width=1024)
print(p1a)
dev.off()