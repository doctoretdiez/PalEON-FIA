library(readr)
library(magrittr)
library(dplyr)

# using character string here for safety because FIA plot ids are near the maximum length of integers in 16-bit numeric representation
setwd(file.path('..','Conversion_tables'))
grid <- read_csv('FIA_paleongrid_albers.csv', col_types = 'dcddd') %>%
    filter(STATECD %in% c(26, 27, 55))  # upper midwest
taxa <- read_csv('FIA_conversion-SGD_remove_dups.csv')
# cp ~/Downloads/tree_data5.csv ../data
data <- read_csv('~/Downloads/tree_data5.csv',
                 col_types = 'cdcdddddcddddddddddcd') %>%
    select(plt_cn, PalEON, time, Jenkins_Biomass)


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




