# Estimate biomass using FIA, Jenkins, and PEcAn allometry
#
# Created by Sean G. DuBois 4/15/16 (sgdubs@gmail.com)

# Query4:
# CREATE TABLE fiaout_v2.query4 AS
# SELECT p.measyear as mytime, p.plot, p.cycle, p.subcycle, p.statecd, p.lon_actual_nad83 as lon, p.lat_actual_nad83 as lat, p.cn as plt_cn, 
# t.cn as tree_cn, t.dia, t.statuscd, t.spcd as spcd, t.tpa_unadj, t.drybio_bole, t.drybio_top, t.drybio_stump 
# FROM fiaout_v2.tree t, fiaout_v2.plot_corrected p
# WHERE p.cn=t.plt_cn;
options(scipen = 999)
# Install packages
library(plyr)
library(RPostgreSQL)
library(raster)

#Import tree names and codes
setwd("C:/Users/Sean/Dropbox/FIA_work/FIA_Rscript_imports/")
spp.codes <- read.csv('FIA_conversion_v02-SGD.csv', header=TRUE)
spp.codes.paleon <- spp.codes[,c('spcd', 'PalEON')]
spp.codes.paleon_nodups <- spp.codes.paleon[-which(duplicated(spp.codes.paleon$spcd)),]
tree.spp <- read.csv("plss.pft.conversion-SGD.csv", header=TRUE)

# Conversion factors
ac2ha   <- 0.404686   #conversion from 1 acre to hectares

# ---------------Connect to SQL-----------------
fia.database <- 'postgres'

# create an PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL")

# open the connection using user, passsword, etc., as root user
con <- dbConnect(drv,dbname='postgres', user='postgres')

#constants
# year    <- 2015       #find surveys closest to present year


# --------------Import SQL data----------------

# # Import data from SQL
# query4 <- "SELECT * FROM fiaout_v2.query4"
# 
# css.query3 <- dbSendQuery(con, query4)
# surv.all  <- fetch(css.query3, n = -1)
# # Next line only works if all columns are imported as character, need to change to numeric
# surv.all[] <- lapply(surv.all, function(x) as.numeric(x))
# lapply(surv.all, class) #check
# # Manage changes not made in PSQL
# # These need to be in the correct column, as code occasionally calls by column not name
# names(surv.all)[names(surv.all) == 'dia'] <- 'dbh'
# surv.all$dbh <- surv.all$dbh*2.54 # SI Units conversion
# names(surv.all)[names(surv.all) == 'mytime'] <- 'time'
# 
# #only select data from most current cycle, subcycle 
# survey.states <- unique(surv.all$statecd)
# surv.current  <- matrix()
# # state.mat from fia2PalEON_v1-SGD.R
# for(s in 1:length(survey.states)){
#   state.code      <- survey.states[s]
#   state.year      <- state.mat[which(state.mat[,1]==state.code),2]
#   state.cycle     <- state.mat[which(state.mat[,1]==state.code),3]
#   state.subcycle  <- state.mat[which(state.mat[,1]==state.code),4]
#   
#   if(s==1){
#     surv.current    <- surv.all[which((surv.all$statecd==state.code) & (surv.all$time %in% state.year) & 
#                                         (surv.all$subcycle %in% state.subcycle)),]
#   } else{
#     surv.tmp     <- surv.all[which((surv.all$statecd==state.code) & (surv.all$time %in% state.year) & 
#                                      (surv.all$subcycle %in% state.subcycle)),]
#     surv.current <- rbind(surv.current,surv.tmp)
#   }  
# }
# 
# # Do some more filtering 
# surv.current <- surv.current[surv.current$statuscd==1,]                      #live trees only
# surv.current <- surv.current[surv.current$dbh>=20.32,]                        #only trees with DBH greater than or equal to 8 inches
# surv.current <- surv.current[complete.cases(surv.current),]
# surv.current[1:10,] #print first 10 lines to make sure stuff looks OK
# row.names(surv.current) <- NULL 
# # Save surv.current without lat/lon
# surv.current.out <- surv.current
# surv.current.out$lon <- NULL
# surv.current.out$lat <- NULL
# write.csv(surv.current.out, 'data/surv.current.out.treecn.nolatlon.csv', row.names = FALSE)
setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/data/")
surv.current <- read.csv('surv.current.out.treecn.nolatlon.csv', header=TRUE)
# -----------------Calculate Jenkins biomass----------------
tree_data <- surv.current
tree_data2 <- merge(tree_data, spp.codes.paleon_nodups, by="spcd")
tree_data3 <- merge(tree_data2, tree.spp, by.x="PalEON", by.y="PLSS")

plt_cn <- unique(as.character(tree_data3$plt_cn))

calcbiomass <- function(beta1, beta2, dia, tpa){
  bm <- (exp(beta1 + beta2 * log(dia)))*tpa
  return(bm)
}

tree_data3$Jenkins_Biomass <- calcbiomass(tree_data3$biomass_b1, tree_data3$biomass_b2, tree_data3$dbh, tree_data3$tpa_unadj) * (1/(ac2ha*1000))

# ---------------Calculate FIA biomass----------------------
tree_data3$FIA_total_biomass <- rowSums(tree_data3[,c('drybio_bole', 'drybio_top', 'drybio_stump')]) * 453.592*10^-6 * 6.018046 * (1/ac2ha)

# ---------------Calculate PEcAn allometry------------------
library(devtools)
install_github("PecanProject/pecan", subdir = "modules/allometry")
# --------------Calculate plot level biomass estimates--------
# species_plot <- unique(tree_data3[,c("plt_cn", "spcd")])
# species_plot$extra <- NA

species_FIA_bio <- aggregate(FIA_total_biomass ~ spcd + plt_cn, tree_data3, FUN=sum)
species_Jenkins_bio <- aggregate(Jenkins_Biomass ~ spcd + plt_cn, tree_data3, FUN=sum)

plot(species_Jenkins_bio$Jenkins_Biomass, species_FIA_bio$FIA_total_biomass)
abline(0,1,col="red")

species_plot_bio <- join_all(list(species_FIA_bio, species_Jenkins_bio))

write.table(species_plot_bio, 'species_plot_bio.csv', row.names=FALSE, col.names=TRUE, sep=",", quote=F)

# ---------------------Rasterize biomass---------------------
plt_cn <- read.csv('C:/Users/sgdubois/Desktop/plt_cn_values.csv')
plt_cn$STATECD <- NULL
  # SPATIAL CONVERSIONS OF DATASET
# Convert from FIA lon,lat to the Albers projection
spp.spatial <- merge(species_plot_bio, plt_cn, by.x="plt_cn", by.y="CN")
colnames(spp.spatial)[6] <- "x"
colnames(spp.spatial)[5] <- "y"

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
nsims <- length(spp.unique) 
pb <- txtProgressBar(min = 1, max = nsims, style = 3) 
for(s in 1:length(spp.unique)){
  if(length(spp.albers$x[which(spp.albers$spcd==spp.unique[s])])>100){ #interpolate only if >10 sites have the spp
    # spp.name <- as.character(spp.codes$scientific_name[which(spp.codes$spcd==spp.unique[s])]) #get spp name
    spp.name <- as.character(spp.codes$acronym[which(spp.codes$spcd==spp.unique[s])]) #get spp code
    bio.raw <- spp.albers[which(spp.albers$spcd==spp.unique[s]),]
#     bio.raw[[s]] <- (cbind(x=spp.albers$x[which(spp.albers$spcd==spp.unique[s])],
#                            y=spp.albers$y[which(spp.albers$spcd==spp.unique[s])],
#                            biomass=spp.albers$FIA_total_biomass[which(spp.albers$spcd==spp.unique[s])]))
#     

#     p <- data.frame(x=bio.raw[[s]][,1],y=bio.raw[[s]][,2],bio=bio.raw[[s]][,3])
#     coordinates(p)<- ~x+y
    bio.rast.jenkins[[s]] <- rasterize(bio.raw, base.rast, 'Jenkins_Biomass', fun=mean)
    names(bio.rast.jenkins[[s]]) <- paste(spp.name) 
    bio.rast.fia[[s]] <- rasterize(bio.raw, base.rast, 'FIA_total_biomass', fun=mean)
    names(bio.rast.fia[[s]]) <- paste(spp.name) 
    
    
    
    
#     if(s==1){
#       bio.stack <- bio.rast[[s]]
#       } else{
#         bio.stack <- stack(bio.stack,bio.rast[[s]])
#         }
#     
#   } else{
#     spp.abv[s]    <- NA
#     bio.grid[[s]] <- 0
   }
  setTxtProgressBar(pb, s) 
}
bio.stack.jenkins <- do.call("stack", bio.rast.jenkins)
bio.stack.fia <- do.call("stack", bio.rast.fia)
setwd('C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/') 
writeRaster(bio.stack.fia,filename="FIA_spp_biomass_fia.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
writeRaster(bio.stack.jenkins,filename="FIA_spp_biomass_jenkins.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')


