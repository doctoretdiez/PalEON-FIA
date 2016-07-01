# Rasterize_biom.R
# Adapted from FIA_biomass.R
# Created by Sean DuBois 6/23/16

# Load required library
library(raster)

# Load required tables
spp.codes <- read.csv('converstion_tables/FIA_conversion_v02-SGD.csv', header=TRUE)
species_plot_bio <- read.csv('data/output/species_plot_bio.csv', header=TRUE)
# ---------------------Rasterize biomass---------------------
plt_cn <- read.csv("data/plt_cn_values.csv", header=TRUE)
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
# Write rasters for each biomass method, for each species
writeRaster(bio.stack.fia,filename="data/output/BIO.STACK.FIA/bio_fia_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(bio.stack.jenkins,filename="data/output/BIO.STACK.JENKINS/bio_jenkins_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(bio.stack.pecan,filename="data/output/BIO.STACK.PECAN/bio_pecan_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')

