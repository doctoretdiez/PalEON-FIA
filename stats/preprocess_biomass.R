library(readr)
library(magrittr)
library(dplyr)

# download files from Wiki:
# biom_fia_pecan.zip (unzip this)
# fia_paleongrid_albers.csv
# fia_conversion_v02-sgd.csv

# decide if use Jenkins or Pecan allometry
# for now if use Pecan, just use point estimate, namely biomass_pecan_pred_mean
# CJP believes that 'pred' are the columns to use as these should incorporate tree-to-tree variability

dataDir <- '.'

dataFile <- 'biom_fia_pecan.csv'
gridFile <- 'fia_paleongrid_albers.csv'
taxaFile <- 'fia_conversion_v02-sgd.csv'

biomassCol <- 'biomass_pecan_pred_mean'

# using character string here for safety because FIA plot ids are near the maximum length of integers in 16-bit numeric representation

setwd(dataDir)

grid <- read_csv(gridFile, col_types = 'dcddd') %>%
    filter(STATECD %in% c(26, 27, 55))  # upper midwest
taxa <- read_csv(taxaFile)
data <- read_csv(dataFile,
                 col_types = 'cdcdddddcddddddddddcd')
# next lines are gymnastics so can treat the biomass column programmatically within dplyr syntax
nm <- names(data)
wh <- which(nm == biomassCol)
nm[wh] <- 'biomass_use'
names(data) <- nm
data <- data %>% select(plt_cn, PalEON, time, biomass_use)


albers <- grid %>% select(x,y,cell) %>% unique()

# total number of fia points per grid cell
total <- grid %>% group_by(cell) %>% summarize(total = n()) 

# note some of these taxa were not modeled in PLS analyses
all_taxa <- expand.grid(PalEON = sort(unique(data$PalEON)),
                        cell = sort(unique(grid$cell)),
                        stringsAsFactors = FALSE) %>%
    left_join(total, by = c('cell' = 'cell')) %>%
    inner_join(albers, by = c('cell' = 'cell'))

data_plot <- data  %>% group_by(plt_cn, PalEON) %>%
    summarize(biomass_total = sum(Jenkins_Biomass)) %>%
    inner_join(grid, by = c('plt_cn' = 'CN')) 

# average biomass (for occupied cells) and number of non-zero fia plots by cell
data_cell <- data_plot %>% group_by(cell, PalEON) %>%
    summarize(biomass_avg = mean(biomass_total), count = n())

data_complete <- all_taxa %>% left_join(data_cell, by = c('PalEON' = 'PalEON',
                                                           'cell' = 'cell'))
data_complete$count[is.na(data_complete$count)] <- 0




