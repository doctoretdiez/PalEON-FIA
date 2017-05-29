#Observed biomass vs predicted biomass maps

setwd('C:/Users/jmurray7/Desktop/PalEON-FIA/')

preds3 = read.csv("stats/output/total.prediction_v0.1.csv", stringsAsFactors = FALSE, header = TRUE)
obs = avgbiomass_data_complete

#pred and obs have different numbers of cells
obs_notpreds = as.data.frame(setdiff(obs$cell,preds3$cell)) #tells what cells are in obs that are not in preds
obs_notpreds

#looking at the entries for some of the cells which are in obs but not in preds
obs[which(obs$cell == "324"),] #count = 1
obs[which(obs$cell == "898"),] #1
obs[which(obs$cell == "1198"),] #2
obs[which(obs$cell == "1199"),] #2
obs[which(obs$cell == "1500"),] #1
obs[which(obs$cell == "1502"),] #0
obs[which(obs$cell == "2694"),] #1
obs[which(obs$cell == "3289"),] #4

obs.preds = merge(obs, preds3, by = "cell")

#get just the cell, x, y, avg_biomass(which is the observed biomass) and mean (which is the predicted biomass)
newobs.pred = obs.preds[,1:4]
newpred = obs.preds[,9]
obs.preds2 = as.data.frame(cbind(newobs.pred,newpred))
colnames(obs.preds2) = c("cell","x","y","obs_biomass","pred_biomass")
head(obs.preds2)

write.csv(obs.preds2,paste0("C:/Users/jmurray7/Desktop/PalEON-FIA/stats/output/total-obs-pred_v0.1.csv"), row.names = FALSE)


#look at the data a bit
summary(obs.preds2$obs_biomass)
summary(obs.preds2$pred_biomass)

png(paste("./stats/output/figures/BoxPlot-total-biomass-Observed-v0.1.png", sep = " "),   height = 768, width=1024)
boxplot(obs.preds2$obs_biomass)
dev.off()

png(paste("./stats/output/figures/BoxPlot-total-biomass-Predicted-v0.1.png", sep = " "),   height = 768, width=1024)
boxplot(obs.preds2$pred_biomass)
dev.off()

#scatter plot of observed vs. predicted biomass
png(paste("./stats/output/figures/obs_v_preds-biomass.png", sep=''),  height = 768, width=1024)
plot(obs.preds2$obs_biomass, obs.preds2$pred_biomass, main = "Total Biomass",
     xlab='Observed',ylab = 'Predicted', cex = 3, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
abline(0,1,lty=2,col=2)
dev.off()


library(ggplot2)
#terrain.colors plots of observed biomass values and predicted biomass values
#total biomass observations
p3obs = ggplot(obs.preds2, aes(x=x, y=y,colour=obs_biomass))+ geom_point(shape=15, cex=3)+
  scale_color_gradientn(colours = terrain.colors(7)) + 
  geom_point(shape=15, cex=3)  + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
                                                  axis.text.y = element_text(size=25), 
                                                  axis.title.x = element_text(size=25),
                                                  axis.title.y = element_text(size=25)) + ggtitle(paste("Observed Total Biomassv0.1")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p3obs

png(paste("./stats/output/figures/Total-biomass-Observed-terraincolors-v0.1.png", sep = " "),   height = 768, width=1024)
print(p3obs)
dev.off()


#total biomass observations with NAs removed
fia = read.csv("stats/output/total-obs-pred_v0.1.csv")
fia_na = fia[complete.cases(fia),]

p3obs_na = ggplot(fia_na, aes(x=x, y=y,colour=obs_biomass))+ geom_point(shape=15, cex=3)+
  scale_color_gradientn(colours = terrain.colors(7)) + 
  geom_point(shape=15, cex=3)  + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
                                       axis.text.y = element_text(size=25), 
                                       axis.title.x = element_text(size=25),
                                       axis.title.y = element_text(size=25)) + ggtitle(paste("Observed Total Biomass no NAs v0.1")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p3obs_na

png(paste("./stats/output/figures/Total-biomass-Observed-noNAs-terraincolors-v0.1.png", sep = " "),   height = 768, width=1024)
print(p3obs_na)
dev.off()




p3preds = ggplot(obs.preds2, aes(x=x, y=y,colour=pred_biomass))+ geom_point(shape=15, cex=3)+
  scale_color_gradientn(colours = terrain.colors(7)) + 
  geom_point(shape=15, cex=3)  + theme(text = element_text(size=25), axis.text.x = element_text(size=25), 
                                                  axis.text.y = element_text(size=25), 
                                                  axis.title.x = element_text(size=25),
                                                  axis.title.y = element_text(size=25)) + ggtitle(paste("Predicted Total Biomass v0.1")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3preds

png(paste("./stats/output/figures/Total-biomass-Predicted-terraincolors-v0.1.png", sep = " "),   height = 768, width=1024)
print(p3preds)
dev.off()


#make map of observed with the same color scale as the predicted smooth maps and any value over the max predicted value is red
obs.preds2$outlier <- paste(">",max(obs.preds2$pred_biomass, na.rm = TRUE))
obs.preds2$outlier[obs.preds2$obs_biomass <= max(obs.preds2$pred_biomass, na.rm = TRUE)] <- NA


p3outlier = ggplot(data = subset(obs.preds2, obs.preds2$obs_biomass < max(obs.preds2$pred_biomass, na.rm = TRUE)), aes(x=x, y=y,colour=obs_biomass))+ geom_point(shape=15, cex=3)+
    scale_color_gradientn(colours = terrain.colors(7))  +
    geom_point(data = subset(obs.preds2, obs.preds2$obs_biomass > max(obs.preds2$pred_biomass, na.rm = TRUE)), aes(x=x, y=y, fill=outlier), shape = 15, cex = 3, colour = "red") + 
  theme(text = element_text(size=25), axis.text.x = element_text(size=25), axis.text.y = element_text(size=25), 
      axis.title.x = element_text(size=25), axis.title.y = element_text(size=25)) + ggtitle(paste("Total Biomass with Outliers"))

p3outlier

png(paste("./stats/output/figures/Total-Observed-Biomass-Outlier-v0.1.png", sep = " "),   height = 768, width=1024)
print(p3outlier)
dev.off()

