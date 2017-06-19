fit <- function(fulldata, grid, k_occ, k_pot, taxon, unc = 'bayes') {
# note that this code is basically the same as in twostagemodelup_bam.R for PLS biomass fitting, but with preprocessing to prepare inputs to bam() different

    library(mgcv)
    
    data <- fulldata %>% filter(PalEON == taxon)
    write.csv(data[, c('PalEON','cell','total','x', 'y', 'biomass_avg','count')], paste0("C:/Users/paleolab/Desktop/PalEON-FIA/stats/output/",taxon, ".observations_v0.1.csv"))
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

    preds <- data.frame(x = grid$x*8000, y = grid$y*8000, mean = mass, sd = pp.sd)
    #return(preds)
    
    preds2 = merge(preds,albers,by=c('x','y'))
    
    write.csv(preds2[, c('x', 'y', 'cell','mean')], paste0("C:/Users/paleolab/Desktop/PalEON-FIA/stats/output/",taxon, ".prediction_v0.1.csv"))
    write.csv(preds2[, c('x', 'y', 'cell', 'sd')], paste0("C:/Users/paleolab/Desktop/PalEON-FIA/stats/output/",taxon, ".uncertainty_v0.1.csv"))
    
    
    
    png(filename = paste("./stats/output/figures/",taxon, "-biomass-sdhist.png", sep=''),  height = 768, width=1024)
    hist(pp.sd,nclass=50,main = paste('Standard Deviation'),xlab = paste('St.dev of', taxon, 'biomass'))
    dev.off()
    
    
    #gs.pal <- colorRampPalette(c("white","blue"),bias=.1,space="rgb")
    
    #makes continuous/smooth maps of observed biomass
   # dat2$obs <- b[,taxon]
  #  dat2$mass <- mass
    
    
   # p2a = ggplot(dat2, aes(x=x, y=y,colour=obs))+ geom_point(shape=15, solid=TRUE,cex=3)+
    #  scale_color_gradient("Observed", low = "white", high = "blue",            
     #                      limits=range(dat2$obs)
                           #breaks=brk
    #  ) + 
     # geom_point(shape=15, solid=TRUE,cex=3)  + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
      #                                                axis.text.y = element_text(size=25), 
       #                                               axis.title.x = element_text(size=25),
        #                                              axis.title.y = element_text(size=25)) + ggtitle(paste(taxon))
    
    #png(paste("./output/figures/",taxon,"-biomass-obs-smooth.png", sep = " "),   height = 768, width=1024)
    #print(p2a)
    #dev.off()
    
    
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
                                                      axis.title.y = element_text(size=25)) + ggtitle(paste(taxon))
    
    png(paste("./stats/output/figures/",taxon,"-biomass-mean-smooth.png", sep = " "),   height = 768, width=1024)
    print(p1a)
    dev.off()
    
    
    #make map of observed with the same color scale as the predicted smooth maps and any value over the max predicted value is red
  #  dat2$outlier <- paste(">",max(dat2$mass, na.rm = TRUE))
  #  dat2$outlier[dat2$obs <= max(dat2$mass, na.rm = TRUE)] <- NA
    
    
   # p2b = ggplot(data = subset(dat2, obs < max(dat2$mass, na.rm = TRUE)), aes(x=x, y=y,colour=obs))+ geom_point(shape=15, solid=TRUE,cex=3)+
  #    scale_color_gradient("Observed", low = "white", high = "blue",             
  #                         limits=range(dat2$mass)
                           #breaks=brk
   #   ) + 
  #    geom_point(shape=15, solid=TRUE,cex=3)  +
  #    geom_point(data = subset(dat2, obs > max(dat2$mass, na.rm = TRUE)), aes(x=x, y=y, fill=outlier), shape = 15, cex = 3, colour = "red") + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
     #                                                                                                                                               axis.text.y = element_text(size=25), 
      #                                                                                                                                              axis.title.x = element_text(size=25),
       #                                                                                                                                             axis.title.y = element_text(size=25)) + ggtitle(paste(taxon))
    
   # png(paste("./output/figures/",taxon,"-biomass-obs-smooth-scaled.png", sep = " "),   height = 768, width=1024)
  #  print(p2b)
  #  dev.off()
    
    
    
    #Plots standard deviation map (discrete)
   # SD = rep(0,N)
  #  SD[which(sd>0 & sd<=10)] = 1
  #  SD[which(sd>10 & sd<=50)] = 2
    
   # SD[which(sd>50 & sd<=100)] = 3
  #  SD[which(sd>100) ] = 4
    
  #  SD = factor(SD,levels=0:4)
  #  levels(SD) = c('0','(0,10]','(10,50]','(50, 100]','(100,200]')
    
   # preds$Sd <- SD
  #  p2 = ggplot(preds, aes(x=x, y=y,colour=Sd))+ geom_point(shape=15, solid=TRUE,cex=3)+
   #   scale_colour_manual(values=gs.pal(5),drop=FALSE) + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
    #                                                           axis.text.y = element_text(size=25), 
     #                                                          axis.title.x = element_text(size=25),
      #                                                         axis.title.y = element_text(size=25)) + ggtitle(paste(taxon))
    
    
  #  png(paste("./output/figures/",taxon,"-biomass-sd-discrete-freqbootstrap.png", sep=''),   height = 768, width=1024)
  #  print(p2)
  #  dev.off()
    
    
    #plots the observed vs. predicted biomass for the taxon
    
   # png(paste("./output/figures/",taxon, "-biomass-comp-subset.png", sep=''),  height = 768, width=1024)
    #plot(dat2$obs, mass, main = paste(taxon),
     #    xlab='Observed',ylab = 'Predicted', cex = 3, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
    #abline(0,1,lty=2,col=2)
    #dev.off()
}
