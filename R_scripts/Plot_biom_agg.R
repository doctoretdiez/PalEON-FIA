# Plot_biom_agg.R
# Adapted from FIA_biomass.R
# Created by SGD 06/07/16

# Modified by SJG - Converted to function, plus added dplyr & tidyr methods to speed up
# aggregation & spreading.

agg_to_plot <- function(tree_data, column, write_out = TRUE) {
  
  long_plot <- tree_data %>% 
    dplyr::group_by(plt_cn, spcd) %>% 
    dplyr::summarise(paste0("biomass = sum(",column,")"))
  
  colnames(long_plot)[3] <- "biomass"
  
  wide_plot <- long_plot %>% 
    tidyr::spread(spcd, biomass) %>% 
    as.data.frame
  
  if (write_out == TRUE) write.csv(wide_plot, paste0('data/output/',column, '.csv'))
  
  wide_plot
}