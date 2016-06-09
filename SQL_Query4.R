# Connect to the SQL database and fetch data
# Adapted from FIA_biomass.R
# Written by SGD 5/23/16

# Install packages
library('RPostgreSQL')

# Load state.mat if not already in global environment
state.mat <- read.csv('data/state_mat.csv')
# ---------------Connect to SQL-----------------
fia.database <- 'postgres'

# create an PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL")

# open the connection using user, passsword, etc., as root user
con <- dbConnect(drv,dbname='postgres', user='postgres')

# --------------Import SQL data----------------

# Query4 code for SQL:
# CREATE TABLE fiaout.query4 AS
# SELECT p.measyear as mytime, p.plot, p.cycle, p.subcycle, p.statecd, p.lon_actual_nad83 as lon, p.lat_actual_nad83 as lat, p.cn as plt_cn,
# t.cn as tree_cn, t.dia, t.statuscd, t.spcd as spcd, t.tpa_unadj, t.drybio_bole, t.drybio_top, t.drybio_stump
# FROM fiaout.tree t, fiaout.plot_corrected p
# WHERE p.cn=t.plt_cn;

# Import data from SQL
query4 <- "SELECT * FROM fiaout.query4"

css.query4 <- dbSendQuery(con, query4)
surv.all  <- fetch(css.query4, n = -1)
# Next line only works if all columns are imported as character, need to change to numeric
surv.all[] <- lapply(surv.all, function(x) as.numeric(x))
lapply(surv.all, class) #check
# Manage changes not made in PSQL
# These need to be in the correct column, as code occasionally calls by column not name
names(surv.all)[names(surv.all) == 'dia'] <- 'dbh'
surv.all$dbh <- surv.all$dbh*2.54 # SI Units conversion
names(surv.all)[names(surv.all) == 'mytime'] <- 'time'

#only select data from most current cycle, subcycle
survey.states <- unique(surv.all$statecd)
surv.current  <- matrix()
# state.mat from fia2PalEON_v1-SGD.R
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
# Save surv.current without lat/lon
surv.current.out <- surv.current
surv.current.out$lon <- NULL
surv.current.out$lat <- NULL
write.csv(surv.current.out, 'data/full_fia_long.csv', row.names = FALSE)