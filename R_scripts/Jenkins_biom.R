# Jenkins_biom.R
# Adapted from FIA_biomass.R
# Created by Sean DuBois 6/4/16

# This has been modified by SJG to:
# 1. Use dplyr for speed & to clean up the variable namespace
# 2. Be wrapped in a function to reduce the memory overhead across all functions.

jenkins_biom <- function(x, write_out = TRUE) {

  #Import tree names and codes.
  spp.codes <- read.csv('Conversion_tables/FIA_conversion_v02-SGD.csv', 
                        header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::select(spcd, PalEON) %>% 
    dplyr::distinct(spcd, PalEON)

  tree.spp <- read.csv("Conversion_tables/plss.pft.conversion-SGD.csv", header = TRUE,
                       stringsAsFactors = FALSE)
  
  # Conversion factors
  ac2ha   <- 0.404686   #conversion from 1 acre to hectares
  
  # -----------------Calculate Jenkins biomass----------------
  tree_data <- x %>% 
    left_join(spp.codes, by = "spcd") %>% 
    left_join(tree.spp, by = c("PalEON" = "PLSS"))

  calcbiomass <- function(beta1, beta2, dia, tpa){
    bm <- (exp(beta1 + beta2 * log(dia)))*tpa
    return(bm)
  }
  
  tree_data$biomass_jenk <- calcbiomass(beta1 = tree_data$biomass_b1, 
                                        beta2 = tree_data$biomass_b2, 
                                        dia   = tree_data$dbh, 
                                        tpa   = tree_data$tpa_unadj) * (1/(ac2ha*1000))
  
  if (write_out == TRUE) write.csv(tree_data, 'data/output/biom_fia_jenk.csv')
  
  return(tree_data)
}