# PEcAn_biom.R
# Adapted from FIA_biomass.R
# Created by Sean DuBois 6/3/16

# Load packages

pecan_biom <- function(x, write_out = TRUE, plotting = FALSE) {
  if (!require(PEcAn.allometry)) {
    devtools::install_github("PecanProject/pecan", subdir = "modules/allometry")
    library(PEcAn.allometry)
  } else {
    library(PEcAn.allometry)
  }
  
  # Conversion factors
  ac2ha   <- 0.404686   #conversion from 1 acre to hectares
  
  # Import tree names and codes (this also happens in the `jenkins_biom` call,
  # so if those columns are already there, then ignore).
  # Note that `pecan` masks `select`, so I've made the call explicit.
  if (!all(c('PalEON', 'spcd', 'PFT') %in% colnames(x))) {
    spp.codes <- read.csv('Conversion_tables/FIA_conversion_v02-SGD.csv', 
                          header = TRUE, stringsAsFactors = FALSE) %>%
      dplyr::select(spcd, PalEON) %>% 
      dplyr::distinct(spcd, PalEON)
    
    tree.spp <- read.csv("Conversion_tables/plss.pft.conversion-SGD.csv", header = TRUE,
                         stringsAsFactors = FALSE)

    tree_data <- x %>% 
      left_join(spp.codes, by = "spcd") %>% 
      left_join(tree.spp, by = c("PalEON" = "PLSS"))
  
  } else {
    tree_data <- x
  }
  
  spp.codes <- read.csv('Conversion_tables/FIA_conversion_v02-SGD.csv', 
                        header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::select(spcd, pft, acronym) %>% 
    dplyr::distinct(spcd, pft, acronym)
  
  #  This adds the 'pft' column onto the data.frame
  tree_data <- tree_data %>% 
    left_join(spp.codes, by = 'spcd') %>% 
    dplyr::select(-acronym)
  
  # Clean the tree_data to remove trees without allometric models:
  
  tree_data <- tree_data %>% 
    filter(!pft %in% c('Hydric', 'UNK'))
  spp.codes <- spp.codes %>% 
    filter(!pft %in% c('Hydric', 'UNK') & spcd %in% tree_data$spcd)
  
  # Setup the Pecan run:
  # Create model using all species from FIA_conversion
  pfts <- list()
  for (i in 1:length(unique(spp.codes$pft))) {
    pfts[[i]] <- unique(na.omit(spp.codes[spp.codes$pft %in% unique(spp.codes$pft)[i], c('acronym', 'spcd')]))
    names(pfts)[i] <- unique(spp.codes$pft)[i]
  }
  
  outdir <- "data/output/PEcAn_allom/"
  
  maxdbh <- ceiling(max(tree_data$dbh))

  if (!length(list.files(outdir, pattern = "Rdata")) == 8) {
    # There are only eight written Rdata files since the Evergreen PFT fails to write.
    
    allom.stats <- AllomAve(pfts,
                            components = 2,
                            outdir     = outdir,
                            ngibbs     = 10000,
                            dmin       = 17,      # dmin > 17 causes error
                            dmax       = maxdbh)
  }
  
  allom.fit <- load.allom(outdir)

  # Run allom.predict by tree (may be too memory intensive to run every tree_data row at once)

  # Pre-build the table to save space in memory:
  pecan_vars <- data.frame(pecan_pred_mean = rep(NA, nrow(tree_data)),
                           pecan_pred_025  = NA,
                           pecan_pred_500  = NA,
                           pecan_pred_975  = NA,
                           pecan_conf_mean = NA,
                           pecan_conf_025  = NA,
                           pecan_conf_500  = NA,
                           pecan_conf_975  = NA)

#  @crollinson proposed a solution in https://github.com/PalEON-Project/PalEON-FIA/issues/5
#  I haven't implemented it, but I'll look into it.  This is the
#  abortive start.
#  
#  for (i in 1:length(unique(tree_data$pft))) {
#    start <- Sys.time()
#    
#  }
  
  mins <- rep(NA, 50)
  
  for (i in (i - 1):length(tree_data$tree_cn)) {

    start <- Sys.time()
    
    pred <- allom.predict(allom.fit, 
                          dbh = tree_data$dbh[i], 
                          pft = tree_data$pft[i])
    
    conf <- allom.predict(allom.fit, 
                          dbh = tree_data$dbh[i], 
                          pft = tree_data$pft[i], 
                          interval = "confidence")
    
    PI = quantile(pred, c(0.025, 0.5, 0.975), na.rm = TRUE)
    CI = quantile(conf, c(0.025, 0.5, 0.975), na.rm = TRUE)
    
    pecan_vars[i,] <- c(mean(pred), PI, mean(conf), CI)
    
    end <- Sys.time()
    
    mins[i %% 50] <- as.numeric((end - start) * (nrow(tree_data) - i), units = "mins")
    
    cat(paste0("\r", i, " of ", nrow(tree_data), ".  You have about ", 
               round(mean(mins, na.rm = TRUE), 0), " minutes left."))
    flush.console()
  }
  
  if (plotting == TRUE) {
    # plot mean and intervals by PFT
    mypfts <- as.character(unique(tree_data$pft))
    for (i in mypfts) {
      
      tree_data.tmp <- tree_data[tree_data$pft == i, ]
      
      tree_data.tmp <- tree_data.tmp[order(tree_data.tmp$dbh), ]
      
      png(paste("data/output/figures/Allom_estimates_", i, ".png",sep = ""),
          width = 5, height = 5, units = "in", res = 200)
      
      plot(tree_data.tmp$dbh, tree_data.tmp$conf_025,
           type = 'l', lty = 2, col = "blue",
           ylim = c(min(tree_data.tmp$pred_025), 
                    max(tree_data.tmp$pred_975)), 
           ylab = "Biomass (kg)", xlab = "DBH (cm)", main = i)
      
      lines(tree_data.tmp$dbh, tree_data.tmp$conf_500,  lty = 2, col = "blue")
      lines(tree_data.tmp$dbh, tree_data.tmp$conf_975,  lty = 2, col = "blue")
      lines(tree_data.tmp$dbh, tree_data.tmp$conf_mean, lty = 1, col = "blue", lwd = 2)
      
      lines(tree_data.tmp$dbh, tree_data.tmp$pred_025,  lty = 2, col = "red")
      lines(tree_data.tmp$dbh, tree_data.tmp$pred_500,  lty = 2, col = "red")
      lines(tree_data.tmp$dbh, tree_data.tmp$pred_975,  lty = 2, col = "red")
      lines(tree_data.tmp$dbh, tree_data.tmp$conf_mean, lty = 1, col = "red", lwd = 2)
      
      legend("topleft", 
             legend = c("Prediction", 
                        "Confidence", 
                        "Quantile (0.025, 0.5, 0.975)", 
                        "Mean"), 
             text.col = c("red", "blue", "black", "black"),
             lty = c(0, 0, 2, 1))
      dev.off()
    }
  }

  pecan_vars <- pecan_vars * 6.018046 * (1/(ac2ha*1000)) # units: Mg/ha

  colnames(pecan_vars) <- paste0("biomass_", colnames(pecan_vars))
  tree_data <- data.frame(tree_data, pecan_vars)
  
  if (write_out == TRUE) {
    write.csv(tree_data, 'data/output/biom_fia_pecan.csv', row.names = FALSE)
  }
  
  return(tree_data)
}