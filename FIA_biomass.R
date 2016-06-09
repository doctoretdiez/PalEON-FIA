# Estimate biomass using FIA, Jenkins, and PEcAn allometry
#
# Created by Sean G. DuBois 4/15/16 (sgdubs@gmail.com)


options(scipen = 999)
# Install packages
library(plyr)
# library(RPostgreSQL)
library(raster)
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

# Load FIA data. This can be the output from SQL_Query4.R, or source(SQL_Query4.R)
# source('PalEON-FIA/SQL_Query4.R')
setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/data/output")
surv.current <- read.csv('full_fia_long.csv', header=TRUE)
# -----------------Calculate Jenkins biomass----------------
tree_data <- surv.current
tree_data2 <- merge(tree_data, spp.codes.paleon_nodups, by="spcd")
tree_data3 <- merge(tree_data2, tree.spp, by.x="PalEON", by.y="PLSS")

plt_cn <- unique(as.character(tree_data3$plt_cn))
spcd_unique <- unique(tree_data3$spcd)

# Jenkins biomass function
calcbiomass <- function(beta1, beta2, dia, tpa){
  bm <- (exp(beta1 + beta2 * log(dia)))*tpa
  return(bm)
}

# Calculate biomass using Jenkins equation and convert to Mg/hanb
tree_data3$Jenkins_Biomass <- calcbiomass(tree_data3$biomass_b1, tree_data3$biomass_b2, tree_data3$dbh, tree_data3$tpa_unadj) * (1/(ac2ha*1000))

# ---------------Calculate FIA biomass----------------------
tree_data3$FIA_total_biomass <- rowSums(tree_data3[,c('drybio_bole', 'drybio_top', 'drybio_stump')]) * 453.592*10^-6 * 6.018046 * (1/ac2ha)

# ---------------Calculate PEcAn allometry------------------
# Create spp.codes table with only used spcd
spp.codes.current <- spp.codes[spp.codes$spcd %in% spcd_unique,]
# # Remove pfts not found in Allom project
# spp.codes.current <- spp.codes.current[-which((spp.codes.current$pft %in% c("Hydric", "UNK", "Evergreen"))),]

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
outdir <- "C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/PEcAn_allom_entire_FIA_conversion_10000/"
allom.stats <- AllomAve(pfts , components = 2, outdir=outdir, ngibbs=10000, dmin=17, dmax=maxdbh) # dmin>17 causes error

allom.fit <- load.allom("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/PEcAn_allom_entire_FIA_conversion_10000/")

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
setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/data/")
write.csv(tree_data5, 'tree_data5.csv', row.names = FALSE)

# --------------Calculate plot level biomass estimates--------
species_FIA_bio <- aggregate(FIA_total_biomass ~ spcd + plt_cn, tree_data5, FUN=sum)
species_Jenkins_bio <- aggregate(Jenkins_Biomass ~ spcd + plt_cn, tree_data5, FUN=sum)
species_PEcAn_bio <- aggregate(PEcAn_Biomass ~ spcd + plt_cn, tree_data5, FUN=sum)

plot(species_Jenkins_bio$Jenkins_Biomass, species_FIA_bio$FIA_total_biomass)
abline(0,1,col="red")

species_plot_bio <- join_all(list(species_FIA_bio, species_Jenkins_bio, species_PEcAn_bio), type = "full")

setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/data/")
write.csv(species_plot_bio, 'species_plot_bio.csv', row.names=FALSE)
species_plot_bio <- read.csv('species_plot_bio.csv', header=TRUE)
# ---------------------Rasterize biomass---------------------
inDir <- "C:/Users/sgdubois/Desktop"
plt_cn <- read.csv(paste(inDir, "plt_cn_values.csv", sep="/"))
plt_cn$STATECD <- NULL
# SPATIAL CONVERSIONS OF DATASET
# Convert from FIA lon,lat to the Albers projection
spp.spatial <- merge(species_plot_bio, plt_cn, by.x="plt_cn", by.y="CN")
colnames(spp.spatial)[names(spp.spatial)=="LON_ACTUAL_NAD83"] <- "x"
colnames(spp.spatial)[names(spp.spatial)=="LAT_ACTUAL_NAD83"] <- "y"

coordinates(spp.spatial)=~x+y
proj4string(spp.spatial)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
spp.albers <- spTransform(spp.spatial,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection
# pts.albers <- unique(coordinates(spp.albers))

#2. Loop over dataset and separate by species
spp.unique <- unique(species_plot_bio$spcd)

# ALBERS base raster bounding box from paleon.geography.wisc.edu
gridxmax <- 2297000
gridxmin <- -71000
gridymin <- 58000
gridymax <- 1498000

base.rast <- raster(xmn = gridxmin, xmx = gridxmax, 
                    ymn = gridymin, ymx = gridymax, resolution = c(8000,8000),
                    crs = "+init=epsg:3175")

# Create Raster for Jenkins
bio.rast.jenkins <- list()
bio.rast.fia <- list()
bio.rast.pecan <- list()
pb <- txtProgressBar(min = 1, max = length(spp.unique), style = 3) 
for(s in 1:length(spp.unique)){
    spp.name <- unique(as.character(spp.codes$acronym[which(spp.codes$spcd==spp.unique[s])])) #get spp code
    bio.raw <- spp.albers[which(spp.albers$spcd==spp.unique[s]),]

    bio.rast.jenkins[[s]] <- rasterize(bio.raw, base.rast, 'Jenkins_Biomass', fun=mean)
    names(bio.rast.jenkins[[s]]) <- paste(spp.name) 
    bio.rast.fia[[s]] <- rasterize(bio.raw, base.rast, 'FIA_total_biomass', fun=mean)
    names(bio.rast.fia[[s]]) <- paste(spp.name) 
    bio.rast.pecan[[s]] <- rasterize(bio.raw, base.rast, 'PEcAn_Biomass', fun=mean)
    names(bio.rast.pecan[[s]]) <- paste(spp.name) 
   
  setTxtProgressBar(pb, s) 
}
bio.stack.jenkins <- do.call("stack", bio.rast.jenkins)
bio.stack.fia <- do.call("stack", bio.rast.fia)
bio.stack.pecan <- do.call("stack", bio.rast.pecan)
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/') 
writeRaster(bio.stack.fia,filename="BIO.STACK.FIA/bio_fia_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(bio.stack.jenkins,filename="BIO.STACK.JENKINS/bio_jenkins_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(bio.stack.pecan,filename="BIO.STACK.PECAN/bio_pecan_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')

