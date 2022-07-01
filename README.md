# Canada Arctic fluvial chemistry, constituent export, and ecosystem carbon balance
## Introduction
Source code for the manuscript: Permafrost Landscape History Shapes Fluvial Chemistry, Ecosystem Carbon Balance, and Potential Trajectories of Future Change. DOI:https://zenodo.org/badge/509124741

## Authors
- [Scott Zolkos](https://www.researchgate.net/profile/Scott-Zolkos)
- Suzanne E. Tank
- Steven V. Kokelj
- Robert G. Striegl
- Sarah Shakil
- Carolina Voigt
- Oliver Sonnentag
- William L. Quinton
- Edward A.G. Schuur
- Donatella Zona
- Peter M. Lafleur
- Ryan C. Sullivan
- Masahito Ueyama
- David Billesbach
- David Cook
- Elyn R. Humphreys
- Philip Marsh

## Data
### The following data associated with this manuscript can be downloaded from the [Arctic Data Center](https://arcticdata.io/)
 
## Scripts
### Data processing
#### *Note: These scripts process the data archived via the link above, which was compiled as detailed in the Methods of this manuscript. Users of these scripts will first need to update working directory pathways within scripts as needed.*  
*1_.R*: Functions used in some scripts below.  
*2_.R*: Read and compile raw data, to compile QA/QC'd data into spreadsheet that is archived via the link above and used in scripts summarized below.  
*3_.R*: Calculate mean ± standard error for all variables.  
*4_.R*: Statistics, as detailed in manuscript Methods.  
*5_.R*: Plot 13C-DIC vs. pH.  
*6_R.*: Plot carbon species fluxes as proportions of regional total.    
*7_.R*: Plot C yields vs. runoff by study region.  
*8_.R*: Perform RDA, plot results.  
*9_.R*: Plots of total fluvial C flux relative to NEE, by region.  
*10_.R*: Estimate mean and range of daily CO2 and CH4 efflux from fluvial networks.  
*11_.R*: Net Ecosystem Carbon Balance (NECB) calculations- mean for each site and location.  
*12_.R*: Derive mean air temperature and total rainfall over desired interval preceding sampling, graphics for Figures S1, S2.  
*13_.R*: Plot mean daily NEE estimated from SPL4CMDL vs. AmeriFlux stations.  
*14_.R*: Plot Figure S2: Absorbance (a254) from Horiba Aqualog vs. Genesys10.  

## Packages
### *For data processing, plotting, and analyses*
- Hmisc
- plotrix
- reshape2
- foreign
- ggplot2
- gdata
- vegan
- ecodist
- pvclust
- lubridate
- corrplot
- ggfortify
- RColorBrewer
- foreign
- purrr
- scales
- lmperm
- lme4
- car
- ggrepel
- vegan
- ggplot2
