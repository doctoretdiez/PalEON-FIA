# Rasterize_parameters
# Adapted from fia2PalEON_v1-SGD.R
# Created by Sean DuBois 6/23/16

# Load required library
library(raster)

# Load required tables
if(!("spp.codes" %in% ls())){
  spp.codes <- read.csv('Conversion_tables/FIA_conversion_v02-SGD.csv', header=TRUE)
}
if(!("species_plot_parameters" %in% ls())){
  species_plot_parameters <- read.csv('data/output/species_plot_parameters.csv', header=TRUE)
}
# ---------------------Rasterize DBH, density and basal area---------------------
plt_cn <- read.csv("data/plt_location/plt_cn_values.csv", header=TRUE)
plt_cn$STATECD <- NULL
# SPATIAL CONVERSIONS OF DATASET
# Convert from FIA lon,lat to the Albers projection
spp.spatial <- merge(species_plot_parameters, plt_cn, by.x="plt_cn", by.y="CN")
colnames(spp.spatial)[names(spp.spatial)=="LON_ACTUAL_NAD83"] <- "x"
colnames(spp.spatial)[names(spp.spatial)=="LAT_ACTUAL_NAD83"] <- "y"

coordinates(spp.spatial)=~x+y
proj4string(spp.spatial)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
spp.albers <- spTransform(spp.spatial,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection
# pts.albers <- unique(coordinates(spp.albers))

#2. Loop over dataset and separate by species
spp.unique <- unique(species_plot_parameters$spcd)

# ALBERS base raster bounding box from paleon.geography.wisc.edu
gridxmax <- 2297000
gridxmin <- -71000
gridymin <- 58000
gridymax <- 1498000

base.rast <- raster(xmn = gridxmin, xmx = gridxmax, 
                    ymn = gridymin, ymx = gridymax, resolution = c(8000,8000),
                    crs = "+init=epsg:3175")

# Create Rasters for DBH, density and basal area
dbh.rast <- list()
den.rast <- list()
bas.rast <- list()
pb <- txtProgressBar(min = 1, max = length(spp.unique), style = 3) 
for(s in 1:length(spp.unique)){
  spp.name <- unique(as.character(spp.codes$acronym[which(spp.codes$spcd==spp.unique[s])])) #get spp code
  param.raw <- spp.albers[which(spp.albers$spcd==spp.unique[s]),]
  
  dbh.rast[[s]] <- rasterize(param.raw, base.rast, 'dbh', fun=mean)
  names(dbh.rast[[s]]) <- paste(spp.name) 
  den.rast[[s]] <- rasterize(param.raw, base.rast, 'density', fun=mean)
  names(den.rast[[s]]) <- paste(spp.name) 
  bas.rast[[s]] <- rasterize(param.raw, base.rast, 'basal_area', fun=mean)
  names(bas.rast[[s]]) <- paste(spp.name) 
  
  setTxtProgressBar(pb, s) 
}
dbh.stack <- do.call("stack", dbh.rast)
den.stack <- do.call("stack", den.rast)
bas.stack <- do.call("stack", bas.rast)
# Write rasters for each biomass method, for each species
writeRaster(dbh.stack,filename="data/output/DBH.STACK/dbh_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(den.stack,filename="data/output/DEN.STACK/den_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(bas.stack,filename="data/output/BAS.STACK/bas_8km_v01.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')

