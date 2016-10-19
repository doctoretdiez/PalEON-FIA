# FIA_biom.R
# Adapted from FIA_biomass.R
# Created by Sean DuBois 6/3/16

fia_biom <- function(x, write_out = TRUE) {
  # Conversion factors
  ac2ha   <- 0.404686   #conversion from 1 acre to hectares
  
  # ---------------Calculate FIA biomass----------------------
  tree_data <- x
  tree_data$biomass_fia <- tree_data %>% 
    select(drybio_bole, drybio_top, drybio_stump) %>% 
    rowSums * 453.592*10 ^ (-6) * 6.018046 * (1/ac2ha)
  
  if (write_out == TRUE) write.csv(tree_data, 'data/output/biom_fia_fia.csv')
  
  return(tree_data)
}