# validate output from fia2PalEONv1-SGD.R with Public Land Survey data
#
# Written by SGD 12/21/15

# Load necessary libraries
library(rgdal)
library(raster)
library(MASS)



# ------------------Import Data Files-------------------
# Import conversion csv
setwd('C:/Users/sgdubois/Dropbox/FIA_work/')
spptable <- read.csv("FIA_Rscript_imports/FIA_conversion-SGD_remove_dups.csv",header=TRUE)
# class(spptable$acronym) <- "character"

# Import Raster Files
# Import only one parameter per run

# Import basal area data
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/BAS.STACK.RAST/')
ras_list <- list.files(pattern='*.tif')
ras_files <- lapply(ras_list, raster)
setwd('C:/Users/sgdubois/Dropbox/FIA_work/ValidationData/albers/basal_area/')
valid_ras_list <- list.files(pattern='*.tif')
valid_ras_files <- lapply(valid_ras_list, raster)

# Import biomass data
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/BIO.STACK.RAST/')
ras_list <- list.files(pattern='*.tif')
ras_files <- lapply(ras_list, raster)
setwd('C:/Users/sgdubois/Dropbox/FIA_work/ValidationData/albers/biomass/')
valid_ras_list <- list.files(pattern='*.tif')
valid_ras_files <- lapply(valid_ras_list, raster)

# Import dbh data
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/DBH.STACK.RAST/')
ras_list <- list.files(pattern='*.tif')
ras_files <- lapply(ras_list, raster)
setwd('C:/Users/sgdubois/Dropbox/FIA_work/ValidationData/albers/diameter/')
valid_ras_list <- list.files(pattern='*.tif')
valid_ras_files <- lapply(valid_ras_list, raster)

# Import density data
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/DEN.STACK.RAST/')
ras_list <- list.files(pattern='*.tif')
ras_files <- lapply(ras_list, raster)
setwd('C:/Users/sgdubois/Dropbox/FIA_work/ValidationData/albers/density/')
valid_ras_list <- list.files(pattern='*.tif')
valid_ras_files <- lapply(valid_ras_list, raster)





GETDF_FROMLIST <- function(DF_LIST, ITEM_LOC){
  DF_SELECTED <- DF_LIST[[ITEM_LOC]]
  return(DF_SELECTED)
}

#------------------Compare and Plot----------------------
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/Figures/')
pdf('FIA_Sydne_Validation_v3_density.pdf', width=8.5, height=120)
par(mfrow=c(length(ras_list), 4))
nsims <- length(ras_list) #SGD ADDITION
pb <- txtProgressBar(min = 1, max = nsims, style = 3) #SGD ADDITION
for (i in 1:length(ras_list)){
  data_file <- GETDF_FROMLIST(ras_files, i)
  sppcode <- data.frame(strsplit(names(ras_files[[i]]), split = "_", fixed=TRUE))
  sppsciname <- subset(spptable, acronym == as.character(sppcode[4,1]), select = scientific_name)
  if (length(sppsciname)>1){stop(paste(sppcode[4,1], sppsciname[[1]], "Error, i=", i, sep=" "))}
  valid_data_number <- which(grepl(sppsciname[[1]], valid_ras_list))
  if (length(valid_data_number)>1){stop(paste(sppcode[4,1], sppsciname[[1]], "Error, i=", i, sep=" "))}
  if (length(valid_data_number)==1){
    valid_file <- GETDF_FROMLIST(valid_ras_files, valid_data_number)
    resamplefia <- resample(data_file, valid_file)
    comp.rast <- resamplefia-valid_file
    #plot data
    par(mar=c(2.1,3,1.6,2))
    plot(resamplefia, main=sppcode[4,1])
    plot(valid_file, main=sppsciname[[1]])
    plot(comp.rast)
    
    plot(resamplefia, valid_file)
    #hist(comp.rast, main=NULL, xlab=NULL, ylab=NULL)
  } else {
    plot(data_file, main=sppcode[4,1])
    frame()
    frame()
    frame()
  }
  setTxtProgressBar(pb, i) #SGD ADDITION
}
dev.off()