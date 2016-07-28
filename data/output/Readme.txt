FILENAME: 
Readme for CSVs following the format biom_fia_*

HEADER:
tree_cn: The "cn" column in the FIA Tree table. According to the FIADB User Guide: "Sequence number. A unique sequence number used to identify a tree record."	

spcd: FIA species code. To see the list of species and codes go to Appendix F of teh FIADB User Guide 	

PalEON: "PalEON" column from FIA_conversion_v02-SGD.csv. PalEON Level 3 tree taxa	

time: "MEASYEAR" column from FIA Plot table. According to the FIADB User Guide: "Measurement year. The year in which the plot was completed. MEASYEAR may differ
from INVYR."	

plot: "Plot" column from FIA Plot table. According to the FIADB User Guide: "Phase 2 plot number. An identifier for a plot. Along with STATECD, INVYR, UNITCD, COUNTYCD and/or some other combinations of variables, PLOT may be used to uniquely identify a plot."

cycle: "Cycle" column from the FIA Plot table. FIA Inventory Cycle Number. According to the FIADB User Guide "A number assigned to a set of plots, measured over a particular period of time from which a State estimate using all possible plots is obtained. A cycle number >1 does not necessarily mean that information for previous cycles resides in the database. A cycle is relevant for periodic and annual inventories."

subcycle: "Subcycle" column from the FIA Plot table. FIA Inventory Subcycle Number. According to the FIADB User Guide "For an annual inventory that takes n years to measure all plots, subcycle shows in which of the n years of the cycle the data were measured. Subcycle is 0 for a periodic inventory. Subcycle 99 may be used for plots that are not included in the estimation process."

statecd: FIA state code. 9 = Connecticut, 10 = Delaware, 17 = Illinois, 18 = Indiana, 23 = Maine, 25 = Massachusetts, 26 = Michigan, 27 = Minnesota, 33 = New Hampshire, 34 = New Jersey, 36 = new York, 39 = Ohio, 42 = Pennsylvania, 44 = Rhode Island, 50 = Vermont, 55 = Wisconsin	

plt_cn: The "cn" column from the FIA Plot table. According to the FIADB User Guide: "Sequence number. A unique sequence number used to identify a plot record." Use this value to match the unfuzzed/unswapped coordinates to the plot

dbh: diameter at breast height	

statuscd: FIA status code. A code indicating whether the sample tree is live, cut, or dead at the time of measurement. Includes dead and cut trees, which are required to estimate aboveground biomass and net annual volume for growth, mortality, and removals. This code is not used when querying data for change estimates. Note: New and replacement plots use only codes 1 and 2. All trees in tree_data5.csv have status code of 1 (live tree). 0 = no status, 1 = live tree, 2 = dead tree, 3 = removed - cut and removed by direct human activity related to harvesting , silviculture or land clearing. This tree is assumed to be utilized.	

tpa_unadj: FIA trees per acre unadjusted. According to the FIADB User Guide "The number of seedlings per acre that the seedling count theoretically represents based on the sample design. For fixed-radius plots taken with the mapped plot design (PLOT.DESIGNCD =1), TPA_UNADJ equals 74.965282 times the number of seedlings counted. For plots taken with other sample designs, this attribute may be blank (null). Based on the procedures described in Bechtold and Patterson (2005), this attribute can be adjusted using factors stored on the POP_STRATUM table to derive population estimates. Examples of estimating population totals are shown in chapter 10."  All tpa_unadj values in tree_dat5.csv are 6.018046 tree per acre.

drybio_bole: FIA dry biomass in the merchantable bole. According to the FIADB User Guide "The oven-dry biomass (pounds) in the merchantable bole of timber species [trees where diameter is measured at breast height (DBH)] greater than or equal to 5 inches in diameter. This is the biomass of sound wood in live and dead trees, including bark, from a 1-foot stump to a minimum 4-inch top diameter of the central stem.  This is a per tree value and must be multiplied by TPA_UNADJ to obtain per acre information. This attribute is blank (null) for timber species with DIA <5.0 inches and for woodland species. See DRYBIO_WDLD_SPP for biomass of woodland species and  DRYBIO_SAPLING for biomass of timber species with DIA <5 inches. For dead or cut timber trees, this number represents the biomass at the time of death or last measurement. DRYBIO_BOLE is based on VOLCFSND and specific gravity information derived by the Forest Products Lab and others (values stored in the REF_SPECIES table).  If VOLCFSND is not available, then either VOLCFGRS * Percent Sound or VOLCFNET * (average ratio of cubic foot sound to cubic foot net volume, calculated as national averages by species group and diameter) is used. The source of specific gravity information for each species can be found by linking the REF_SPECIES table to the REF_CITATION table. Appendix M contains equations used to estimate biomass components in the FIADB."

drybio_top: FIA dry biomass in the top of the tree. According to the FIADB User Guide "The oven-dry biomass (pounds) in the top and branches (combined) of timber species [trees where diameter is measured at breast height (DBH)] greater than or equal to 5 inches in diameter. DRYBIO_TOP includes the tip, the portion of the stem above the merchantable bole (i.e., above the 4-inch top diameter), and all branches;  excludes foliage. Estimated for live and dead trees. This is a per tree value and must be multiplied by TPA_UNADJ to obtain per acre information. For dead or cut trees, this number represents the biomass at the time of death or last measurement. This attribute is blank (null) for timber species with DIA <5.0 inches and for woodland species. See DRYBIO_WDLD_SPP for biomass of woodland species, and DRYBIO_SAPLING for biomass of timber species with DIA <5.0 inches. Appendix M contains equations used to estimate biomass components in the FIADB."

drybio_stump: FIA dry biomass in the tree stump. According to the FIADB User Guide "The oven-dry biomass (pounds) in the stump of timber species [trees where diameter is measured at breast height (DBH)] greater than or equla to 5 inches in diameter.  The stump is that portion of the tree from the ground to the bottom of the merchantable bole (i.e., below 1 foot). This is a per tree value and must be multiplied by TPA_UNADJ to obtain per acre information. Estimated for live and dead trees. For dead or cut trees, this number represents the biomass at the time of death or last measurement. This attribute is blank (null) for timber species with DIA <5.0 inches and for woodland species. See DRYBIO_WDLD_SPP for biomass of woodland species, and DRYBIO_SAPLING for biomass of timber species with DIA <5.0 inches. Appendix M contains equations used to estimate biomass components in the FIADB."

biomass_b1: The "biomass_b1" from FIA_conversion _v02-SGD.csv.

biomass_b2: The "biomass_b2" from FIA_conversion _v02-SGD.csv.

Jenkins_Biomass: Biomass (Mg/ha) calculated using Jenkin's allometries

FIA_total_biomass: Biomass (Mg/ha) calculated using Jaclyn Hatal Matthes's code

pft: The "pft" column from FIA_conversion_v02-SGD.csv. 

PEcAn_Biomass: Biomass (Mg/ha) calculated using PEcAn allometries