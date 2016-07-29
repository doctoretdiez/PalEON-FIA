# Plot_parameters_agg.R
# Adapted from fia2PalEON_v1-SGD.R
# Created by SGD 07/28/16

options(scipen = 999)

# Load packages
if(!require(plyr)){
  install.packages('plyr')
  require(plyr)
} else {require(plyr)}

if(!require(dplyr)){
  install.packages('dplyr')
  require(dplyr)
} else {require(dplyr)}

# Constants
tpa <- 6.018046
ac2ha   <- 0.404686   #conversion from 1 acre to hectares

# Read in tree data
surv.current <- read.csv('data/output/full_fia_long.csv', header=TRUE)

# ----------Estimate FIA parameters at the species level-------------------
# Mean dbh
species_FIA_dbh <- aggregate(dbh ~ spcd + plt_cn, surv.current, FUN=mean)
# Species count
tree_count <- surv.current %>% dplyr::count(plt_cn, spcd)
# Species density
tree_count$density <- tree_count$n * tpa* (1/ac2ha)
# Basal area
surv.current$basal_area <- (surv.current$dbh/2/100)^2 * pi * tpa * (1/ac2ha) # units m^2/ha
species_FIA_basalarea <- aggregate(basal_area ~ spcd + plt_cn, surv.current, FUN=sum)

avg.plot <- join_all(list(species_FIA_dbh, species_FIA_basalarea, tree_count), type = "full")

write.csv(avg.plot, 'data/output/species_plot_parameters.csv', row.names=FALSE)
