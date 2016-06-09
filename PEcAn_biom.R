# PEcAn_biom.R
# Adapted from FIA_biomass.R
# Created by Sean DuBois 6/3/16

# Load packages
if(!require(PEcAn.allometry)){
  library(devtools)
  install_github("PecanProject/pecan", subdir = "modules/allometry")
  require(PEcAn.allometry)
}

#Import tree names and codes
setwd("C:/Users/sgdubois/Dropbox/FIA_work/FIA_Rscript_imports/")
spp.codes <- read.csv('FIA_conversion_v02-SGD.csv', header=TRUE)
spp.codes.paleon <- spp.codes[,c('spcd', 'PalEON')]
spp.codes.paleon_nodups <- spp.codes.paleon[-which(duplicated(spp.codes.paleon$spcd)),]
tree.spp <- read.csv("plss.pft.conversion-SGD.csv", header=TRUE)

# Conversion factors
ac2ha   <- 0.404686   #conversion from 1 acre to hectares

surv.current <- read.csv('full_fia_long.csv', header=TRUE)
tree_data <- surv.current
tree_data2 <- merge(tree_data, spp.codes.paleon_nodups, by="spcd")
tree_data3 <- merge(tree_data2, tree.spp, by.x="PalEON", by.y="PLSS")

plt_cn <- unique(as.character(tree_data3$plt_cn))
spcd_unique <- unique(tree_data3$spcd)

# ---------------Calculate PEcAn allometry------------------
# Create spp.codes table with only used spcd
spp.codes.current <- spp.codes[spp.codes$spcd %in% spcd_unique,]

# Add pft column to FIA data
tree_data3$PFT <- NULL
spcd.pft <- spp.codes.current[,c('spcd', 'pft')]
tree_data4 <- merge(tree_data3, spcd.pft, by="spcd")

# # Create model using only species with FIA data
# mypfts <- as.character(unique(spp.codes.current$pft))
# pfts <- list()
# for(i in 1:length(mypfts)){
#   pfts[[i]] <- spp.codes.current[spp.codes.current$pft==mypfts[i],c('acronym', 'spcd')]
# }
# names(pfts) <- mypfts
# 
# setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/PEcAn_allom/")
# mindbh <- floor(min(tree_data4$dbh))
# maxdbh <- ceiling(max(tree_data4$dbh))
# 
# allom.stats = AllomAve(pfts,ngibbs=10, components = 2, dmin=mindbh, dmax=maxdbh)
# allom.fit = load.allom(getwd())

# Create model using all species from FIA_conversion
pfts <- list()
for(i in unique(spp.codes$pft)){
  pfts[[i]] <- spp.codes[spp.codes$pft==i,c('acronym', 'spcd')]
}
outdir <- "~/PEcAn_allom/"
allom.stats <- AllomAve(pfts , components = 2, outdir=outdir, ngibbs=10000, dmin=17, dmax=maxdbh) # dmin>17 causes error

allom.fit <- load.allom(outdir)

# Run allom.predict by tree (may be too memory intensive to run every tree_data row at once)
# Remove PFTs that don't have an allometry model
tree_data_pecan <- tree_data4[-which(tree_data4$pft %in% c("Hydric", "UNK")),]

pb <- txtProgressBar(min = 1, max = length(tree_data_pecan$tree_cn), style = 3)
for(i in 1:length(tree_data_pecan$tree_cn)){
  pred <- allom.predict(allom.fit, dbh=tree_data_pecan$dbh[i], pft=tree_data_pecan$pft[i])
  conf <- allom.predict(allom.fit, dbh=tree_data_pecan$dbh[i], pft=tree_data_pecan$pft[i], interval = "confidence")
  PI = apply(pred,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  CI = apply(conf,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  tree_data_pecan$pred_mean[i] <- colMeans(pred)
  tree_data_pecan$conf_mean[i] <- colMeans(conf)
  tree_data_pecan$pred_025[i] <- PI[1,]
  tree_data_pecan$pred_500[i] <- PI[2,]
  tree_data_pecan$pred_975[i] <- PI[3,]
  tree_data_pecan$conf_025[i] <- CI[1,]
  tree_data_pecan$conf_500[i] <- CI[2,]
  tree_data_pecan$conf_975[i] <- CI[3,]
  
  setTxtProgressBar(pb, i)
}
setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/data/")
write.csv(tree_data_pecan, "tree_data_pecan.csv", row.names=FALSE)

# plot mean and intervals by PFT
mypfts <- as.character(unique(tree_data_pecan$pft))
for (i in mypfts){
  tree_data.tmp <- tree_data_pecan[tree_data_pecan$pft==i,]
  tree_data.tmp <- tree_data.tmp[order(tree_data.tmp$dbh),]
  setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/PEcAn_allom_entire_FIA_conversion_10000/figures")
  png(paste("Allom_estimates_", i, ".png",sep=""),width=5,height=5,units="in",res=200)
  plot(tree_data.tmp$dbh, tree_data.tmp$conf_025,type='l',lty=2, col="blue",ylim=c(min(tree_data.tmp$pred_025), max(tree_data.tmp$pred_975)), 
       ylab="Biomass (kg)", xlab="DBH (cm)", main=i)
  lines(tree_data.tmp$dbh,tree_data.tmp$conf_500,lty=2,col="blue")
  lines(tree_data.tmp$dbh,tree_data.tmp$conf_975,lty=2,col="blue")
  lines(tree_data.tmp$dbh,tree_data.tmp$conf_mean,lty=1,col="blue", lwd=2)
  
  lines(tree_data.tmp$dbh,tree_data.tmp$pred_025,lty=2,col="red")
  lines(tree_data.tmp$dbh,tree_data.tmp$pred_500,lty=2,col="red")
  lines(tree_data.tmp$dbh,tree_data.tmp$pred_975,lty=2,col="red")
  lines(tree_data.tmp$dbh,tree_data.tmp$conf_mean,lty=1,col="red", lwd=2)
  
  legend("topleft", legend = c("Prediction", "Confidence", "Quantile (0.025, 0.5, 0.975)", "Mean"), text.col = c("red", "blue", "black", "black"),
         lty = c(0, 0, 2, 1))
  dev.off()
}
# Currently, the Evergreen PFT provides poor estimates, so should remove
tree_data_pecan <- tree_data_pecan[-which(tree_data_pecan$pft=="Evergreen"),]
tree_data_pecan$PEcAn_Biomass <- tree_data_pecan$pred_mean * 6.018046 * (1/(ac2ha*1000)) # units: Mg/ha
tree_data_pecan_lite <- tree_data_pecan[,c("tree_cn", "PEcAn_Biomass")]
tree_data5 <- merge(tree_data4, tree_data_pecan_lite, by="tree_cn", all.x=TRUE)

write.csv(tree_data5, 'data/biom_fia_pecan.csv', row.names = FALSE)