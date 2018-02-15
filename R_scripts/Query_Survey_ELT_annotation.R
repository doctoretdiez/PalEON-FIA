# Query_Survey.R
# Retrieve survey data from SQL and create matrix of most recent cycle and subcycle
# Adapted from fia2PalEON_v1-SGD.R

# ELT don't we need to set a working directory here?
library(RPostgreSQL)

options(scipen = 999)
##FYI, to clear MySQL result set
#dbClearResult(dbListResults(con)[[1]]) #ELT This might come in handy later

fia.database <- 'postgres' 
# ELT try testdb?

#constants
year    <- 2015       #find surveys closest to present year #ELT or 2018?

# create a PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL") 

# open the connection using user, password, etc., as root user
# password not necessary as there is a valid pgpass.conf file
con <- dbConnect(drv, dbname=fia.database, user='postgres') 
# ELT be sure to open postgrs app on computer before continuing with this.pgadmin with elephant!!!
# am I on the wrong host: 5433? how do i input password through R?

# -------------Query DB for all states and years---------------
# ELT must manually input how to classify each piece of db we want to use
query       <- "SELECT invyr, statecd, stateab, statenm, cycle, subcycle from fiaout.SURVEY"
state.query <- dbSendQuery(con, query)
state.surv  <- fetch(state.query, n = -1)
class(state.surv$invyr) <- "numeric"
class(state.surv$statecd) <- "numeric"
class(state.surv$cycle) <- "numeric"
class(state.surv$subcycle) <- "numeric"
states <- sort(unique(state.surv$statecd))

#NEW METHOD: Chooses the cycle and subcycle closest to the specified year 
#in order to get a complete 5-year survey across all state plots
# ELT Why the above? Maybe not everything is sampled every year?

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
write.csv(state.mat, 'data/output/state_mat.csv')
