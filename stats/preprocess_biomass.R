library(readr)
library(magrittr)
library(dplyr)
library(ncdf4)

rm(list=ls())


# download files from Wiki:
# biom_fia_pecan_v0.1.zip (unzip this)
# fia_paleongrid_albers.csv
# paleonMask.nc

# decide if use Jenkins or Pecan allometry. 
#### NOTE:5-26-17 JP decided to use Jenkins since the biomass manuscript is comparing FIA biomass to PLS biomass and the PLS biomass was estimated with the Jenkin's allometries. ###
# for now if you use Pecan, just use point estimate, namely biomass_pecan_pred_mean
# CJP believes that 'pred' are the columns to use as these should incorporate tree-to-tree variability

UMW <- c(26, 27, 55) # upper midwest FIPS (MN, WI, MI)
LMW <- c(17, 18)     # lower midwest FIPS (IL and IN)
states <- c(UMW, LMW)

data_dir <- './stats/'

data_file <- 'biom_fia_pecan_v0.1.csv'
grid_file <- 'fia_paleongrid_albers.csv'
# taxa translation not needed as already built into data_file
# taxa_file <- 'fia_conversion_v02-sgd.csv'

biomass_col <- 'biomass_jenk'

# using character string here for safety because FIA plot ids are near the maximum length of integers in 16-bit numeric representation

setwd(data_dir)

grid <- read_csv(grid_file, col_types = 'dcddd') %>%
  filter(STATECD %in% states) 
# taxa <- read_csv(taxa_file)
data <- read_csv(data_file,
                 col_types = 'dddddccdddddddcddcddcdddddddd')
#                 col_types = 'cdcdddddcddddddddddcd')
# next lines are gymnastics so can treat the biomass column programmatically within dplyr syntax
nm <- names(data)
wh <- which(nm == biomass_col)
nm[wh] <- 'biomass_use'
names(data) <- nm
data <- data %>% select(plt_cn, PalEON, time, biomass_use)
#change Cedar/juniper to Cedar.juniper
data$PalEON = gsub("/",".",data$PalEON) 


albers <- grid %>% select(x,y,cell) %>% unique()

# total number of fia points per grid cell
total <- grid %>% group_by(cell) %>% summarize(total = n()) 

# note some of these taxa were not modeled in PLS analyses
all_taxa <- expand.grid(PalEON = sort(unique(data$PalEON)),
                        cell = sort(unique(grid$cell)),
                        stringsAsFactors = FALSE) %>%
  left_join(total, by = c('cell' = 'cell')) %>%
  inner_join(albers, by = c('cell' = 'cell'))

data_by_plot <- data  %>% group_by(plt_cn, PalEON) %>%
  summarize(biomass_total = sum(biomass_use)) %>%
  inner_join(grid, by = c('plt_cn' = 'CN')) 

# average biomass (for occupied cells) and number of non-zero fia plots by cell
data_by_cell <- data_by_plot %>% group_by(cell, PalEON) %>%
  summarize(biomass_avg = mean(biomass_total), count = n())

data_complete <- all_taxa %>% left_join(data_by_cell, by = c('PalEON' = 'PalEON',
                                                             'cell' = 'cell'))
data_complete$count[is.na(data_complete$count)] <- 0

### set up grid cells on which to do prediction

mask <- nc_open('paleonMask.nc')
regions <- ncvar_get(mask, 'subregion', c(1,1),c(-1,-1))

x <- matrix(ncvar_get(mask, 'x', c(1),c(-1)), nrow = nrow(regions),
            ncol = ncol(regions))
y <- matrix(ncvar_get(mask, 'y', c(1),c(-1)),nrow = nrow(regions),
            ncol = ncol(regions), byrow = TRUE) 

west_regions <- c(2,3,5,6,11,12 )

wh <- c(regions %in% west_regions)
pred_grid <- data.frame(x = c(x), y = c(y))
pred_grid <- pred_grid[wh, ]

k_occ <- 1000
k_pot <- 100


#  example of fitting from CJP
#results <- fit(data_complete,pred_grid, k_occ = k_occ, k_pot = k_pot,
#               taxon = 'Poplar')


i <- 1:16
taxa <- c("Ash", "Basswood", "Beech", "Birch", "Cedar.juniper", "Cherry", "Elm", "Fir",  "Hemlock", "Hickory", "Maple","Oak", "Other hardwood", "Pine", "Poplar", "Spruce", "Tamarack", "Tulip poplar", "Walnut")
#these taxa are >1% of the total number of FIA trees, except Tamarack and Walnut. Below are each taxa's percentage
#Maple 24%, Oak 13%, Pine 10%, Poplar 7%, Birch 6%, Cedar/juniper 5%, Ash 5%, Hemlock 5%, Spruce 4%, Cherry 3%,
#Beech 2%, Fir 2%, Basswood 2%, Hickory 2%, Other hardwood 1%, Elm 1%, Tulip poplar 1%, Tamarck 0.92%, Walnut 0.70%,
#Black gum 0.33%, Hackberry 0.30%, Sweet gum 0.23%, Sycamore 0.23%, Ironwood 0.17%, Willow 0.17%, Buckeye 0.06%,
#Douglas fir 0.008%, Dogwood 0.002%, Uknown 0.001%, Alder 0.0007%.

for(i in 1:length(taxa)){
  source('./stats/fit_bam.R')
  fit(fulldata = data_complete, grid = pred_grid, k_occ = k_occ, k_pot = k_pot, taxon=taxa[i], unc = 'bayes')
}


# code to run just one taxon at a time,
# run the next 4 lines, then run the code within the function in the fit_bam.R file.
fulldata = data_complete
grid = pred_grid
taxon = 'Ash'
unc = 'bayes'


#####################################################################
##  biomass averaged by plot in each cell for all taxa combined   ###
#####################################################################
total_biomass_by_plot <- data_by_plot %>% group_by(plt_cn) %>%
  summarize(total_biomass = sum(biomass_total))%>%
  inner_join(grid, by = c('plt_cn' = 'CN'))

biomass_by_cell <- total_biomass_by_plot %>% group_by(cell) %>%
  summarize(avg_biomass_by_plot = mean(total_biomass), count = n())

avgbiomass_data_complete <- albers %>% left_join(biomass_by_cell, by = c('cell' = 'cell'))

avgbiomass_data_complete$count[is.na(avgbiomass_data_complete$count)] <- 0

write.csv(avgbiomass_data_complete, "./stats/output/total.observation_v0.2.csv", row.names = FALSE)


# code to run total biomass,
# run the next 4 lines, then run the code within the function in the fit_bam_totalbiomass.R file.
fulldata = avgbiomass_data_complete
grid = pred_grid
unc = 'bayes'

###########################################################################################
###  Code to Examine the 198 Lost/Missing Cells in Predictions compared to Observations ###
###########################################################################################
#after running the fit_bam.R code and getting the preds2 object, you can either run the code 
#below when looping through each taxa.  Or you can used the read back in the predictions by
#taxa using the following oak example code
oak = read.csv("./stats/output/Oak.prediction_v0.1.csv")

#plot the observations (albers) vs the predictions (points)
png(filename = paste("./stats/output/figures_v0.1/198_MissingCells.png", sep=''),  height = 768, width=1024)
plot(albers$x,albers$y,col='blue',pch=16)
points(oak$x,oak$y,col='red',pch=15)
dev.off()

#You can see the lost (blue) points by running this after the merge of 'albers' and 'preds':

#original code from Chris if you are running a taxa within the loop and get the preds2 output from line 96 of fit_bam.R
plot(albers$x,albers$y,col='blue',pch=16)
points(preds2$x,preds2$y,col='red',pch=16)


###########################################################
### Combine Total Biomass Observed, Predicted, and SD #####
###########################################################

observed = read.csv("./stats/output/total.observation_v0.2.csv", header = TRUE, stringsAsFactors = FALSE)
predicted = read.csv("./stats/output/total.prediction_v0.2.csv", header = TRUE, stringsAsFactors = FALSE)
uncertainty = read.csv("./stats/output/total.uncertainty_v0.2.csv", header = TRUE, stringsAsFactors = FALSE)

obs.pred = observed %>% left_join(predicted, by = c('x' = 'x', 'y' = 'y',
                                                       'cell' = 'cell'))
total.summary =obs.pred %>% left_join(uncertainty, by = c('x' = 'x', 'y' = 'y',
                                                        'cell' = 'cell'))
write.csv(total.summary,"./stats/output/total.summary_v0.2.csv", row.names = FALSE)

#########################################################################################################
###  Code to Examine the 198 Lost/Missing Cells in Total Biomass Predictions compared to Observations ###
#########################################################################################################
# this is the same code as above, but done for the total biomass per PalEON grid cell
png(filename = paste("./stats/output/198_MissingCells_v0.2.png", sep=''),  height = 768, width=1024)
plot(albers$x,albers$y,col='blue',pch=16)
points(predicted$x,predicted$y,col='red',pch=15)
dev.off()


#################################################################################################
## Code to switch out Total observations, predictions and uncertainty in v0.1 FIA_biomass csvs ##
#################################################################################################

#Switch out v0.1 Total biomass observations for v0.2 Total biomass observations
FIA.obs = read.csv("./stats/output/FIA_biomass_observations_v0.1.csv")
FIA.obs = FIA.obs[,-23]
head(FIA.obs)
head(observed)


colnames(observed) = c("x","y","cell","Total","count")

FIA.obs.total =FIA.obs %>% left_join(observed, by = c('x' = 'x', 'y' = 'y',
                                                      'cell' = 'cell'))

head(FIA.obs.total)
FIA.obs.total = FIA.obs.total[,-24]
write.csv(FIA.obs.total,"./stats/output/FIA_biomass_observations_v0.2.csv", row.names = FALSE)


#Switch out v0.1 Total biomass predictions for v0.2 Total biomass predictions
FIA.preds = read.csv("./stats/output/FIA_biomass_predictions_v0.1.csv")
FIA.preds = FIA.preds[,-23]
head(FIA.preds)
head(predicted)

colnames(predicted) = c("x","y","cell","Total")

FIA.preds.total =FIA.preds %>% left_join(predicted, by = c('x' = 'x', 'y' = 'y',
                                                           'cell' = 'cell'))

head(FIA.preds.total)
write.csv(FIA.preds.total,"./stats/output/FIA_biomass_predictions_v0.2.csv", row.names = FALSE)


#Switch out v0.1 Total biomass uncertainty for v0.2 Total biomass uncertainty
FIA.unc = read.csv("./stats/output/FIA_biomass_uncertainty_v0.1.csv")
FIA.unc = FIA.unc[,-23]
head(FIA.unc)
head(uncertainty)

colnames(uncertainty) = c("x","y","cell","Total")

FIA.unc.total =FIA.unc %>% left_join(uncertainty, by = c('x' = 'x', 'y' = 'y',
                                                         'cell' = 'cell'))

head(FIA.unc.total)
write.csv(FIA.unc.total,"./stats/output/FIA_biomass_uncertainty_v0.2.csv", row.names = FALSE)


