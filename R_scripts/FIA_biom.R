# FIA_biom.R
# Adapted from FIA_biomass.R
# Created by Sean DuBois 6/3/16

# Conversion factors
ac2ha   <- 0.404686   #conversion from 1 acre to hectares

# ---------------Calculate FIA biomass----------------------
tree_data3 <- surv.current
tree_data3$FIA_total_biomass <- rowSums(tree_data3[,c('drybio_bole', 'drybio_top', 'drybio_stump')]) * 453.592*10^-6 * 6.018046 * (1/ac2ha)
write.csv(tree_data3, 'data/output/biom_fia_fia.csv')