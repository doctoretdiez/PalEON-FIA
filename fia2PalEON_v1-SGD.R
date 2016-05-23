# fia2PalEON_v1-SGD.R
# Extract tree-level variables per plot for PalEON domain from FIA database.
# Aggregate tree data by unique species per plot.
# Spatial interpolation of plot-level data to PalEON domain.
# 
# Jaclyn Hatala Matthes, 11/27/13
# email: jaclyn.hatala.matthes@gmail.com
# Updated by Sean G. DuBois 11/6/2015 (sgdubs@gmail.com)

#-------------Install Packages-----------------------
# install.packages('RMySQL')
install.packages('geoR')
install.packages('sp')
install.packages('rgdal')
install.packages('raster')
install.packages('fields')
install.packages('ncdf')	
install.packages('plyr')
install.packages('RPostgreSQL')

# library(RMySQL)
library(geoR)
library(sp)
library(rgdal)
library(raster)
library(fields)
library(ncdf)	
library(plyr)
library(RPostgreSQL)

options(scipen = 999)
##FYI, to clear MySQL result set
#dbClearResult(dbListResults(con)[[1]])

fia.database <- 'postgres' 

#constants
year    <- 2015       #find surveys closest to present year
ac2ha   <- 0.404686   #conversion from 1 acre to hectares

# create a PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL") 

# open the connection using user, passsword, etc., as root user
con <- dbConnect(drv,dbname='postgres', user='postgres')


# -------------Query DB for all states and years---------------
query       <- "SELECT invyr, statecd, stateab, statenm, cycle, subcycle from fiaout_v2.SURVEY"
state.query <- dbSendQuery(con, query)
state.surv  <- fetch(state.query, n = -1)
class(state.surv$invyr) <- "numeric"
class(state.surv$statecd) <- "numeric"
class(state.surv$cycle) <- "numeric"
class(state.surv$subcycle) <- "numeric"
states <- sort(unique(state.surv$statecd))

#NEW METHOD: Chooses the cycle and subcycle closest to the specified year 
#in order to get a complete 5-year survey across all state plots

#first need to find max. subcycle year for each state (some states took > 5 years to sample all plots)
state.max.subcycle  <- list()

for(s in states){
  sel <- which(state.surv$statecd == s)
  
  state.max.subcycle[s] <- max(state.surv$subcycle[sel][state.surv$subcycle[sel]!=99]) 
}

state.cycle <- list() #list to hold most recent year, cycle, subcycle per state

#have to loop everything into a list because no a priori knowledge about how long matrix would be
for(s in states){
  sel <- which(state.surv$statecd == s)
  closest.year.ind <- which(abs(state.surv$invyr[sel]-year)==min(abs(state.surv$invyr[sel]-year))) #find the closest survey year to selected year
  max.rep.cycle <- min(state.surv$cycle[sel[which(abs(state.surv$invyr[sel]-year)==min(abs(state.surv$invyr[sel]-year)))]]) #find the newest cycle
  max.subcycle  <- 1:max(state.surv$subcycle[sel[state.surv$cycle[sel]==max.rep.cycle&state.surv$subcycle[sel]!=99]]) #find the max subcycle within that cycle
  
  if(max(max.subcycle)<state.max.subcycle[s]){ #if the cycle isn't complete
    
    #get older chunk of 5-year survey to make complete set
    old.cycle    <- max.rep.cycle-1
    s.m.c.s <- as.numeric(state.max.subcycle[s]) 
    old.subcycle <- sort(seq(s.m.c.s,1,by=-1)[which(seq(s.m.c.s,1,by=-1)-max(max.subcycle)>0)])
    match.old.years <- state.surv$invyr[sel[which(state.surv$cycle[sel]==old.cycle&(state.surv$subcycle[sel] %in% old.subcycle))]]
    match.old.cycle <- state.surv$cycle[sel[which(state.surv$cycle[sel]==old.cycle&(state.surv$subcycle[sel] %in% old.subcycle))]]
    match.old.subcycle <- state.surv$subcycle[sel[which(state.surv$cycle[sel]==old.cycle&(state.surv$subcycle[sel] %in% old.subcycle))]]
    old.dat <- cbind(match.old.years,match.old.cycle,match.old.subcycle) #make matrix
    
    #get newest set of the 5-year survey
    match.years <- state.surv$invyr[sel[which(state.surv$cycle[sel]==max.rep.cycle&(state.surv$subcycle[sel] %in% max.subcycle))]]
    match.cycle <- state.surv$cycle[sel[which(state.surv$cycle[sel]==max.rep.cycle&(state.surv$subcycle[sel] %in% max.subcycle))]]
    match.subcycle <- state.surv$subcycle[sel[which(state.surv$cycle[sel]==max.rep.cycle&(state.surv$subcycle[sel] %in% max.subcycle))]]

    if(sum(diff(match.years))==(length(match.years)-1)){ #if there were no other years with same cycle #
      state.cycle[[s]] <- cbind(rep(s,s.m.c.s),rbind(old.dat,cbind(match.years,match.cycle,match.subcycle)))
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    } else{ #sometimes there is an older survey w/ same cycle number, but is always only a single instance
      match.years <- match.years[2:length(match.years)] #should fix this, but first return is always an old annual survey, so clip it dumb way for now  
      match.cycle <- match.cycle[2:length(match.cycle)]
      match.subcycle <- match.subcycle[2:length(match.subcycle)]
      state.cycle[[s]] <- cbind(rep(s,s.m.c.s),rbind(old.dat,cbind(match.years,match.cycle,match.subcycle)))
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    }
      
  } else{ #if the cycle is complete
    #get newest set of the 5-year survey
    match.years <- state.surv$invyr[sel[which(state.surv$cycle[sel]==max.rep.cycle&(state.surv$subcycle[sel] %in% max.subcycle))]]
    match.cycle <- state.surv$cycle[sel[which(state.surv$cycle[sel]==max.rep.cycle&(state.surv$subcycle[sel] %in% max.subcycle))]]
    match.subcycle <- state.surv$subcycle[sel[which(state.surv$cycle[sel]==max.rep.cycle&(state.surv$subcycle[sel] %in% max.subcycle))]]
    
    if(sum(diff(match.years))==(s.m.c.s-1)){ #if there were no other years with same cycle #
      state.cycle[[s]] <- cbind(rep(s,s.m.c.s),match.years,match.cycle,match.subcycle)
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    } else{ #sometimes there is an older survey w/ same cycle number, but is always only a single instance
      match.years <- match.years[2:length(match.years)] #should fix this, but first return is always an old annual survey, so clip it dumb way for now  
      match.cycle <- match.cycle[2:length(match.cycle)]
      match.subcycle <- match.subcycle[2:length(match.subcycle)]
      state.cycle[[s]] <- cbind(rep(s,s.m.c.s),match.years,match.cycle,match.subcycle)
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    }
  }
}

#make list into matrix of state code, year, current cycle, current subcycle for easier manipulation
state.mat <- state.cycle[[1]]
for(i in 2:length(state.cycle)){
  if(length(state.cycle[[i]])>0){
    state.mat <- rbind(state.mat,state.cycle[[i]])
  } else{
    state.mat <- rbind(state.mat,rep(NA,4))
  }
}
state.mat <- state.mat[complete.cases(state.mat),]
colnames(state.mat) <- c("statecd","time","cycle","subcycle")

# ------------------QUERY 3-------------------
# Query3 code for SQL:
# CREATE TABLE fiaout.query3 AS
# SELECT p.measyear as mytime, p.plot, p.cycle, p.subcycle, p.statecd, p.lon_actual_nad83 as lon, p.lat_actual_nad83 as lat, p.cn, t.dia, t.statuscd, t.spcd as spcd, t.tpa_unadj, t.drybio_bole, t.drybio_top, t.drybio_stump 
# FROM fiaout.tree t, fiaout.plot_corrected p
# WHERE p.cn=t.plt_cn;

query3 <- "SELECT * FROM fiaout.query3"

css.query3 <- dbSendQuery(con, query3)
surv.all  <- fetch(css.query3, n = -1)
# write.table(surv.all, file='surv.all.csv', row.names=FALSE, col.names=TRUE, sep=",", quote=F)
# next line only works if all columns are imported as character, need to change to numeric
surv.all[] <- lapply(surv.all, function(x) as.numeric(x))
lapply(surv.all, class) #check
# Manage changes not made in PSQL
# Note: These need to be in the correct column, as code occasionally calls by column not name
names(surv.all)[names(surv.all) == 'dia'] <- 'dbh'
surv.all$dbh <- surv.all$dbh*2.54 # SI Units conversion
names(surv.all)[names(surv.all) == 'mytime'] <- 'time'
# Save surv.all after correctly formatted
write.table(surv.all, file='surv.all.csv', row.names=FALSE, col.names=TRUE, sep=",", quote=F)
#only select data from most current cycle, subcycle 
survey.states <- unique(surv.all$statecd)
surv.current  <- matrix()
for(s in 1:length(survey.states)){
  state.code      <- survey.states[s]
  state.year      <- state.mat[which(state.mat[,1]==state.code),2]
  state.cycle     <- state.mat[which(state.mat[,1]==state.code),3]
  state.subcycle  <- state.mat[which(state.mat[,1]==state.code),4]
  
  if(s==1){
    surv.current    <- surv.all[which((surv.all$statecd==state.code) & (surv.all$time %in% state.year) & 
                                        (surv.all$subcycle %in% state.subcycle)),]
  } else{
    surv.tmp     <- surv.all[which((surv.all$statecd==state.code) & (surv.all$time %in% state.year) & 
                                        (surv.all$subcycle %in% state.subcycle)),]
    surv.current <- rbind(surv.current,surv.tmp)
  }  
}

# Do some more filtering 
surv.current <- surv.current[surv.current$statuscd==1,]                      #live trees only
surv.current <- surv.current[surv.current$dbh>=20.32,]                        #only trees with DBH greater than or equal to 8 inches
surv.current <- surv.current[complete.cases(surv.current),]
surv.current[1:10,] #print first 10 lines to make sure stuff looks OK
row.names(surv.current) <- NULL 

####CALCULATE SPP-LEVEL VARIABLES####
#1. Loop over unique plots and aggregate unique species responses
avg.list <- list()
uniq.pts <- surv.current[!duplicated(surv.current[,c("lat","lon")]),6:7]
nsims <- nrow(uniq.pts)
pb <- txtProgressBar(min = 1, max = nsims, style = 3)
for(p in 1:nrow(uniq.pts)){
  lon.ind  <- which(surv.current$lon==uniq.pts[p,1]&surv.current$lat==uniq.pts[p,2])
  
  #grab plot-level attributes
  lon.tmp  <- rep(surv.current$lon[lon.ind[1]],times=length(unique(surv.current$spcd[lon.ind])))
  lat.tmp  <- rep(surv.current$lat[lon.ind[1]],times=length(unique(surv.current$spcd[lon.ind])))
  spp.tmp  <- sort(unique(surv.current$spcd[lon.ind]))
  sta.tmp  <- rep(unique(surv.current$statecd[lon.ind]),times=length(spp.tmp))
  
  dat.tmp <- data.frame(surv.current[lon.ind,])
  # dat.tmp columns: 1-year, 2-plot, 3-cycle, 4-subcycle, 5-statecd, 6-lon, 7-lat, 8-expns, 9-dbh, 10-statuscd, 
  # 11-spcd, 12-tpa_unadj, biomass: 13-bole, 14-top, 15-stump (16-18 are removed, see above)
  #calculate plot-level variables
  dbh.avg  <- tapply(dat.tmp[,9], dat.tmp[,11], mean)
  spp.tpa  <- tapply(dat.tmp[,12], dat.tmp[,11], mean)
  
  spp.uncorr <- ddply(dat.tmp, .(dat.tmp$spcd, dat.tmp$tpa_unadj), nrow)[,3]
  spp.cnt    <- ddply(dat.tmp, .(dat.tmp$spcd, dat.tmp$tpa_unadj), nrow)[,3] * ddply(dat.tmp, .(dat.tmp$spcd, dat.tmp$tpa_unadj), nrow)[,2] * (1/ac2ha)
  

  spp.bio <- tapply(apply(dat.tmp[,13:15],1,sum),dat.tmp[,11],sum) * (spp.tpa) * 453.592*10^-6 * (1/ac2ha) #biomass = lbs/ac to Mg/ha
  
  spp.bas  <- tapply(pi*(dat.tmp[,9]/100/2)^2, dat.tmp[,11], sum) * (spp.tpa) * (1/ac2ha) #basal area = m2/ha
    
  avg.plot <- cbind(statecd=sta.tmp,lon=lon.tmp,lat=lat.tmp,spcd=spp.tmp,count_uncorr=spp.uncorr,
                    dbh=dbh.avg,density=spp.cnt,basalarea=spp.bas,biomass=spp.bio)
  
  avg.list[[p]] <- avg.plot
  
  setTxtProgressBar(pb, p)
}

all.avg <- do.call("rbind",avg.list)

#####-----------SPATIAL CONVERSIONS OF DATASET------------#####
#1. Convert from FIA lon,lat to the Albers projection
spp.spatial <- as.data.frame(all.avg)
colnames(spp.spatial)[2] <- "x"
colnames(spp.spatial)[3] <- "y"
spp.spatial <- spp.spatial[complete.cases(spp.spatial),] 

coordinates(spp.spatial)=~x+y
proj4string(spp.spatial)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
spp.albers <- spTransform(spp.spatial,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection
pts.albers <- unique(coordinates(spp.albers))

write.csv(spp.albers,"FIA_spp_albers_orig.csv")

#2. Loop over dataset and separate by species
spp.table  <- read.csv("FIA_conversion-SGD_remove_dups.csv",header=TRUE)
spp.unique <- unique(all.avg[,4])



# ALBERS base raster bounding box from paleon.geography.wisc.edu
gridxmax <- 2297000
gridxmin <- -71000
gridymin <- 58000
gridymax <- 1498000

base.rast <- raster(xmn = gridxmin, xmx = gridxmax, 
                    ymn = gridymin, ymx = gridymax, resolution = c(8000,8000),
                    crs = "+init=epsg:3175")

# If comparing output to validation raster, use that as base.rast
# base.rast <- raster('ValidationData/rasters-SG/basal_area/Abies_balsamea_basal_area_alb.tif')

#------------Generate Output Files-------------
#extract each species-level dbh response and extrapolate to the whole grid
dbh.grid <- list()
dbh.raw  <- list()
dbh.rast <- list()
spp.abv  <- vector()
nsims <- length(spp.unique) 
pb <- txtProgressBar(min = 1, max = nsims, style = 3) 
for(s in 1:length(spp.unique)){
  if(length(spp.albers$x[which(spp.albers$spcd==spp.unique[s])])>100){ #interpolate only if >10 sites have the spp
    spp.abv[s] <- unique(as.character(spp.table$acronym[which(spp.unique[s]==spp.table$spcd)]))       #get spp abbreviation
    dbh.raw[[s]] <- (cbind(x=spp.albers$x[which(spp.albers$spcd==spp.unique[s])],
                           y=spp.albers$y[which(spp.albers$spcd==spp.unique[s])],
                           dbh=spp.albers$dbh[which(spp.albers$spcd==spp.unique[s])]))

#     #fill plots with no spp present as zero
#     empty.pts <- pts.albers[!(pts.albers %in% coordinates(dbh.raw[[s]][,1:2]))]
#     zero.pts  <- cbind(empty.pts[1:(length(empty.pts)/2)],empty.pts[(length(empty.pts)/2+1):length(empty.pts)]) #dumb way
#     zero.thin <- zero.pts[seq(1,nrow(zero.pts),by=25),] #thin these or the spline will take forever and crash
#     dbh.raw[[s]]  <- rbind(dbh.raw[[s]],cbind(zero.thin,rep(0,length=nrow(zero.thin)))) #add in plots with no spp
    
#     tps <- Tps(dbh.raw[[s]][,1:2], dbh.raw[[s]][,3]) #thin plate spline fit
#     dbh.grid[[s]] <- interpolate(base.rast, tps)     #interpolate spline to new grid
#     dbh.grid[[1]][dbh.grid[[1]]<0] <- NA             #fill zeros with NA
    # names(dbh.grid[[s]]) <- paste(spp.abv[s])
    #To aggregate data by 8km grid cells:
    p <- data.frame(x=dbh.raw[[s]][,1],y=dbh.raw[[s]][,2],dbh=dbh.raw[[s]][,3])
    coordinates(p)<- ~x+y
    dbh.rast[[s]] <- rasterize(p, base.rast, 'dbh', fun=mean)
    names(dbh.rast[[s]]) <- paste(spp.abv[s])
#     
    if(s==1){
      # dbh.stack <- dbh.grid[[s]]
      dbh.stack <- dbh.rast[[s]]
    } else{
      # dbh.stack <- stack(dbh.stack,dbh.grid[[s]])
      dbh.stack <- stack(dbh.stack,dbh.rast[[s]])
    }
    
  } else{
    spp.abv[s]    <- NA
    dbh.grid[[s]] <- 0
  }
  setTxtProgressBar(pb, s) 
}
# Sys.sleep(300)
#names(dbh.stack) <- spp.abv[!is.na(spp.abv)]
# writeRaster(dbh.stack,filename="FIA_spp_dbh.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix=value) 

writeRaster(dbh.stack,filename="FIA_spp_dbh.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
# dbh.stack <- NULL
# Sys.sleep(300)

#nlayers(dbh.stack)

#extract each species-level density response and extrapolate to the whole grid
den.grid <- list()
den.raw  <- list()
den.rast <- list()
nsims <- length(spp.unique) 
pb <- txtProgressBar(min = 1, max = nsims, style = 3) 
for(s in 1:length(spp.unique)){
  if(length(spp.albers$x[which(spp.albers$spcd==spp.unique[s])])>100){ #interpolate only if >10 sites have the spp
    spp.abv[s] <- unique(as.character(spp.table$acronym[which(spp.unique[s]==spp.table$spcd)]))       #get spp abbreviation
    den.raw[[s]] <- (cbind(x=spp.albers$x[which(spp.albers$spcd==spp.unique[s])],
                           y=spp.albers$y[which(spp.albers$spcd==spp.unique[s])],
                           density=spp.albers$density[which(spp.albers$spcd==spp.unique[s])]))
    
#     #fill plots with no spp present as zero
#     empty.pts <- pts.albers[!(pts.albers %in% coordinates(den.raw[[s]][,1:2]))]
#     zero.pts  <- cbind(empty.pts[1:(length(empty.pts)/2)],empty.pts[(length(empty.pts)/2+1):length(empty.pts)]) #dumb way
#     zero.thin <- zero.pts[seq(1,nrow(zero.pts),by=25),] #thin these or the spline will take forever and crash
#     den.raw[[s]]  <- rbind(den.raw[[s]],cbind(zero.thin,rep(0,length=nrow(zero.thin)))) #add in plots with no spp
    
#     tps <- Tps(den.raw[[s]][,1:2], den.raw[[s]][,3]) #thin plate spline fit
#     den.grid[[s]] <- interpolate(base.rast, tps)     #interpolate spline to new grid
#     den.grid[[1]][den.grid[[1]]<0] <- NA             #fill zeros with NA
    # names(den.grid[[s]]) <- paste(spp.abv[s])
        # plot(den.grid[[s]], main=s)
        p <- data.frame(x=den.raw[[s]][,1],y=den.raw[[s]][,2],den=den.raw[[s]][,3])
        coordinates(p)<- ~x+y
        den.rast[[s]] <- rasterize(p, base.rast, 'den', fun=mean)
        names(den.rast[[s]]) <- paste(spp.abv[s])    
    #     
    #     dbh.grid[[s]] <- cbind(x=akima.si$x,y=akima.si$y,dbh=akima.si$z)
    #     p <- data.frame(x=dbh.grid[[s]][,1],y=dbh.grid[[s]][,2],dbh=dbh.grid[[s]][,3])
    #     coordinates(p)<- ~x+y
    #     dbh.rast <- rasterize(p, base.rast, 'dbh', fun=mean)
    #     
    if(s==1){
      # den.stack <- den.grid[[s]]
      den.stack <- den.rast[[s]]
    } else{
      # den.stack <- stack(den.stack,den.grid[[s]])
      den.stack <- stack(den.stack,den.rast[[s]])
    }
    
  } else{
    spp.abv[s]    <- NA
    den.grid[[s]] <- 0
  }
  setTxtProgressBar(pb, s) 
}

# Sys.sleep(300)
# writeRaster(den.stack,filename="FIA_spp_density.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix=value)

writeRaster(den.stack,filename="FIA_spp_density.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
# den.stack <- NULL
# Sys.sleep(300)
#extract each species-level basal area response and extrapolate to the whole grid
bas.grid <- list()
bas.raw  <- list()
bas.rast <- list()
nsims <- length(spp.unique) 
pb <- txtProgressBar(min = 1, max = nsims, style = 3) 
for(s in 1:length(spp.unique)){
  if(length(spp.albers$x[which(spp.albers$spcd==spp.unique[s])])>100){ #interpolate only if >10 sites have the spp
    spp.abv[s] <- unique(as.character(spp.table$acronym[which(spp.unique[s]==spp.table$spcd)]))       #get spp abbreviation
    bas.raw[[s]] <- (cbind(x=spp.albers$x[which(spp.albers$spcd==spp.unique[s])],
                           y=spp.albers$y[which(spp.albers$spcd==spp.unique[s])],
                           basalarea=spp.albers$basalarea[which(spp.albers$spcd==spp.unique[s])]))
    
#     #fill plots with no spp present as zero
#     empty.pts <- pts.albers[!(pts.albers %in% coordinates(bas.raw[[s]][,1:2]))]
#     zero.pts  <- cbind(empty.pts[1:(length(empty.pts)/2)],empty.pts[(length(empty.pts)/2+1):length(empty.pts)]) #dumb way
#     zero.thin <- zero.pts[seq(1,nrow(zero.pts),by=25),] #thin these or the spline will take forever and crash
#     bas.raw[[s]]  <- rbind(bas.raw[[s]],cbind(zero.thin,rep(0,length=nrow(zero.thin)))) #add in plots with no spp
    
#     tps <- Tps(bas.raw[[s]][,1:2], bas.raw[[s]][,3]) #thin plate spline fit
#     bas.grid[[s]] <- interpolate(base.rast, tps)     #interpolate spline to new grid
#     bas.grid[[1]][bas.grid[[1]]<0] <- NA             #fill zeros with NA
    # names(bas.grid[[s]]) <- paste(spp.abv[s])
    #    plot(dbh.grid[[s]])
    p <- data.frame(x=bas.raw[[s]][,1],y=bas.raw[[s]][,2],bas=bas.raw[[s]][,3])
    coordinates(p)<- ~x+y
    bas.rast[[s]] <- rasterize(p, base.rast, 'bas', fun=mean)
    names(bas.rast[[s]]) <- paste(spp.abv[s])    
    #     
    #     dbh.grid[[s]] <- cbind(x=akima.si$x,y=akima.si$y,dbh=akima.si$z)
    #     p <- data.frame(x=dbh.grid[[s]][,1],y=dbh.grid[[s]][,2],dbh=dbh.grid[[s]][,3])
    #     coordinates(p)<- ~x+y
    #     dbh.rast <- rasterize(p, base.rast, 'dbh', fun=mean)
    #     
    if(s==1){
#       bas.stack <- bas.grid[[s]]
      bas.stack <- bas.rast[[s]]
      
    } else{
#       bas.stack <- stack(bas.stack,bas.grid[[s]])
      bas.stack <- stack(bas.stack,bas.rast[[s]])
      
    }
    
  } else{
    spp.abv[s]    <- NA
    bas.grid[[s]] <- 0
  }
  setTxtProgressBar(pb, s) 
  
}
# Sys.sleep(300)
# writeRaster(bas.stack,filename="FIA_spp_basalarea.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix=value) 
#writeRaster(bas.stack,filename="FIA_spp_basalarea.nc",format="CDF") 

writeRaster(bas.stack,filename="FIA_spp_basalarea.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
# bas.stack <- NULL
# Sys.sleep(300)
#extract each species-level biomass response and extrapolate to the whole grid
bio.grid <- list()
bio.raw  <- list()
bio.rast <- list()
nsims <- length(spp.unique) 
pb <- txtProgressBar(min = 1, max = nsims, style = 3) 
for(s in 1:length(spp.unique)){
  if(length(spp.albers$x[which(spp.albers$spcd==spp.unique[s])])>100){ #interpolate only if >10 sites have the spp
    spp.abv[s] <- unique(as.character(spp.table$acronym[which(spp.unique[s]==spp.table$spcd)]))       #get spp abbreviation
    bio.raw[[s]] <- (cbind(x=spp.albers$x[which(spp.albers$spcd==spp.unique[s])],
                           y=spp.albers$y[which(spp.albers$spcd==spp.unique[s])],
                           biomass=spp.albers$biomass[which(spp.albers$spcd==spp.unique[s])]))
    
#     #fill plots with no spp present as zero
#     empty.pts <- pts.albers[!(pts.albers %in% coordinates(bio.raw[[s]][,1:2]))]
#     zero.pts  <- cbind(empty.pts[1:(length(empty.pts)/2)],empty.pts[(length(empty.pts)/2+1):length(empty.pts)]) #dumb way
#     zero.thin <- zero.pts[seq(1,nrow(zero.pts),by=25),] #thin these or the spline will take forever and crash
#     bio.raw[[s]]  <- rbind(bio.raw[[s]],cbind(zero.thin,rep(0,length=nrow(zero.thin)))) #add in plots with no spp
    
#     tps <- Tps(bio.raw[[s]][,1:2], bio.raw[[s]][,3]) #thin plate spline fit
#     bio.grid[[s]] <- interpolate(base.rast, tps)     #interpolate spline to new grid
#     bio.grid[[1]][bio.grid[[1]]<0] <- NA             #fill zeros with NA
    # names(bio.grid[[s]]) <- paste(spp.abv[s])
    #    plot(dbh.grid[[s]])
    p <- data.frame(x=bio.raw[[s]][,1],y=bio.raw[[s]][,2],bio=bio.raw[[s]][,3])
    coordinates(p)<- ~x+y
    bio.rast[[s]] <- rasterize(p, base.rast, 'bio', fun=mean)
    names(bio.rast[[s]]) <- paste(spp.abv[s])    
    #     
    #     dbh.grid[[s]] <- cbind(x=akima.si$x,y=akima.si$y,dbh=akima.si$z)
    #     p <- data.frame(x=dbh.grid[[s]][,1],y=dbh.grid[[s]][,2],dbh=dbh.grid[[s]][,3])
    #     coordinates(p)<- ~x+y
    #     dbh.rast <- rasterize(p, base.rast, 'dbh', fun=mean)
    #     
    if(s==1){
#       bio.stack <- bio.grid[[s]]
      bio.stack <- bio.rast[[s]]
      
    } else{
#       bio.stack <- stack(bio.stack,bio.grid[[s]])
      bio.stack <- stack(bio.stack,bio.rast[[s]])
      
    }
    
  } else{
    spp.abv[s]    <- NA
    bio.grid[[s]] <- 0
  }
  setTxtProgressBar(pb, s) 
}

writeRaster(bio.stack,filename="FIA_spp_biomass.tif",format="GTiff",overwrite=TRUE,bylayer=TRUE,suffix='names')
