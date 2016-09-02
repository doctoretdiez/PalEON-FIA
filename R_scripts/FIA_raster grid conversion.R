#read in unfuzzed FIA points
setwd("F:/NRS_actual_coords_forested_plots")
x <- read.csv("NRS_actual_coords_forested_plots.csv")
library(rgdal)
library(sp)
library(raster)


x2 <- SpatialPointsDataFrame(coords=data.frame(x=x$LON_ACTUAL_NAD83, y=x$LAT_ACTUAL_NAD83),data=data.frame(CN=x$CN, STATECD=x$STATECD), proj4string=CRS("+init=epsg:4269"))
x3 <-spTransform(x2, CRS("+init=epsg:3175"))

#Getting this error after running x3:
#Error in spTransform(x2, CRS("+init=espg:3175")) : 
#  error in evaluating the argument 'CRSobj' in selecting a method for function 'spTransform': Error in CRS("+init=espg:3175") : no system list, errno: 2

#create base raster in albers projection
base.rast <- raster(xmn = -71000, xmx = 2297000, ncols = 296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')
numbered.rast <- setValues(base.rast, 1:ncell(base.rast))

numbered.rast

#change the raster into a data frame
df <- as.data.frame(numbered.rast, xy=TRUE)

head(df)

#original function from Simon
get_cells <- function(x, y = NULL){
  ### This function is the basic function where we get xy coordinates of the points and then 
  ### either create a raster with the focal dataset (y) or just the cell numbers:
  
  base.rast <- raster(xmn = -71000, xmx = 2297000, ncols = 296,
                      ymn = 58000,  ymx = 1498000, nrows = 180,
                      crs = '+init=epsg:3175')
  numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
  
  cells <- extract(numbered.rast, x)
  
  if (is.null(y)) {
    return(cells)
  }
  
  null.rast <- setValues(numbered.rast, NA)
  
  null.rast[cells] <- y
  
  return(null.rast)
  
}

#get cell numbers for the fia unfuzzed points spatial object
fia_cell <- get_cells(x3@coords)

head(fia_cell)

y <- as.data.frame(cbind(x3@coords,x3@data))
y$cell <- fia_cell
head(y)

#merge the y <- x3@coords and x3@data dataframe with the df <- paleon albers raster. Merge by cell number from the get_cells function
#and the layer number from the raster
#save as new file with the FIA plot original lat and longs, the albers lat and longs, and the x,y coorindates from the 
#centroid of the PalEON grid rasters
df2 <- merge(x=y, y=df, by.x='cell', by.y='layer')
head(df2)
#head(df2) has columns cell, x.x (that is albers longitude). y.x (that is albers latitude), CN, STATECD, x.y (that is raster grid centroid longitude), y.y (that is raters grid centroid latitude)
colnames(df2) <- c("cell", "LON_ALBERS", "LAT_ALBERS","CN", "STATECD", "x", "y")
write.csv(df2, paste0("FIA_raster_cell_albers.csv"), row.names=F)

#read the full raster_cell file back in and then select all the columns without the acutal lat/long values. 
#save as a new file with the state codes, CN (plot numebrs), x/y (PalEON grid centroids coordinates), and grid cell numbers
cell <- read.csv("FIA_raster_cell_albers.csv")
head(cell)

df3 <- data.frame(cell$STATECD, cell$CN, cell$x, cell$y, cell$cell)
colnames(df3) <- c("STATECD", "CN", "x", "y", "cell")
write.csv(df3, paste0("FIA_paleongrid_albers.csv"), row.names=F)
head(df3)


#merge the grid cell numbers with the tree_data5.csv biomass values from Sean.  plt_cn from Sean's file is
#the same as the CN values from the unfuzzed coordinates
grid <- read.csv("FIA_paleongrid_albers.csv")
biomass <- read.csv("C:/Users/jmurray7/Dropbox/PalEON2/FIAData/Biomass_June_29_2016/tree_data5.csv")
biomassfull <- merge(x=biomass, y=grid, by.x='plt_cn', by.y='CN')
write.csv(biomassfull, paste0("FIA_treedata5_paleongrid.csv"), row.names=F)


#merge the grid cell numbers with the species_plot_parameters.csv which has density values from Sean.  plt_cn from Sean's file is
#the same as the CN values from the unfuzzed coordinates
setwd("C:/Users/jmurray7/Dropbox/PalEON2/FIAData")
grid <- read.csv("FIA_paleongrid_albers.csv")
density <- read.csv("species_plot_parameters.csv")
densityfull <- merge(x=density, y=grid, by.x='plt_cn', by.y='CN')
write.csv(densityfull, paste0("FIA_species_plot_parameters_paleongrid.csv"), row.names=F)

#merge the grid cell numbers with the full_fia_long_v0.1.csv which has biomass values from Sean.  plt_cn from Sean's file is  
#the same as the CN values from the unfuzzed coordinates
setwd("C:/Users/jmurray7/Dropbox/PalEON2/FIAData")
grid <- read.csv("FIA_paleongrid_albers.csv")
fullfia <- read.csv("full_fia_long_v0.1.csv")
fullfiafull <- merge(x=fullfia, y=grid, by.x='plt_cn', by.y='CN')
write.csv(fullfiafull, paste0("full_fia_long_v0.1_paleongrid.csv"), row.names=F)
