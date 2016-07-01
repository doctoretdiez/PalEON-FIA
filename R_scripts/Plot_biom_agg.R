# Plot_biom_agg.R
# Adapted from FIA_biomass.R
# Created by SGD 06/07/16

# Load biomass estimates
Jenkins_biom <- read.csv('data/biom_fia_jenk.csv')
FIA_biom <- read.csv('data/biom_fia_fia.csv')
PEcAn_biom <- read.csv('data/biom_fia_pecan.csv')

# --------------Calculate plot level biomass estimates--------
species_FIA_bio <- aggregate(FIA_total_biomass ~ spcd + plt_cn, FIA_biom, FUN=sum)
species_Jenkins_bio <- aggregate(Jenkins_Biomass ~ spcd + plt_cn, Jenkins_biom, FUN=sum)
species_PEcAn_bio <- aggregate(PEcAn_Biomass ~ spcd + plt_cn, PEcAn_biom, FUN=sum)

# Compare Jenkins and FIA plot level estimates
plot(species_Jenkins_bio$Jenkins_Biomass, species_FIA_bio$FIA_total_biomass)
abline(0,1,col="red")

# Combine all
species_plot_bio <- join_all(list(species_FIA_bio, species_Jenkins_bio, species_PEcAn_bio), type = "full")

setwd("C:/Users/sgdubois/Dropbox/FIA_work/CodeOutput/data/")
write.csv(species_plot_bio, 'species_plot_biom.csv', row.names=FALSE)
