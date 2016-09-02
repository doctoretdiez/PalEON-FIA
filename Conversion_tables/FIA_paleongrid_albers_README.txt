FILENAME: 
FIA_paleongrid_albers.csv

DESCRIPTION: 
Use this file to convert the plt_cn (FIA plot identifier) to the 8x8km Albers PalEON grid cells.


For each filename, a short description of what data it contains |Format of the file if not obvious from the file name | If the data set includes multiple files that relate to one another, the relationship between the files or a description of the file structure that holds them (possible terminology might include "dataset" or "study" or "data package")
********
HEADER:
statecd: FIA state code. 9 = Connecticut, 10 = Delaware, 17 = Illinois, 18 = Indiana, 23 = Maine, 25 = Massachusetts, 26 = Michigan, 27 = Minnesota, 33 = New Hampshire, 34 = New Jersey, 36 = new York, 39 = Ohio, 42 = Pennsylvania, 44 = Rhode Island, 50 = Vermont, 55 = Wisconsin	

CN: The plot record number. This number matches the "plt_cn" column in the FIA files. According to the FIADB User Guide: "Sequence number. A unique sequence number used to identify a plot record." Use this value to match the unfuzzed/unswapped coordinates to the plot. Note that this number is very long and is represented in R and excel in scientific notation. 

x: x coordinate for the center of PalEON grid cells. This is in the Great Lakes St Lawrence Albers projection (+init=epsg:3175)

y: y coordinate for the center of PalEON grid cells. This is in the Great Lakes St Lawrence Albers projection (+init=epsg:3175)

cell: PalEON grid cell number

********
Name/institution/address/email information for Principal investigator (or person responsible for collecting the data) | Associate or co-investigators | Contact person for questions
********
PI: Jason McLachlan
Creator: Jody Peters
Contact for data: Jody Peters
********

Information about geographic location of data collection
******
Full PalEON Domain from Minnesota to Maine


******
Date that the file was created
*September 2, 2016
Date(s) that the file(s) was updated and the nature of the update(s), if applicable
*

Keywords used to describe the data topic
********
FIA, Forest Inventory, Modern Trees, PalEON Great Lakes St Lawrence Albers Projection, 8x8 km Grid 


********
Language information
*English

Methodological information
Method description, links or references to publications or other documentation containing experimental design or protocols used in data collection | Any instrument-specific information needed to understand or interpret the data | Standards and calibration information, if appropriate | Describe any quality-assurance procedures performed on the data | Definitions of codes or symbols used to note or characterize low quality/questionable/outliers that people should be aware of | People involved with sample collection, processing, analysis and/or submission | 
*********
Jody's code for merging the plt_cn vlaues in the FIA files with the CN values from the FIA_paleongrid_albers.csv are on GitHub: https://github.com/PalEON-Project/PalEON-FIA



*********

