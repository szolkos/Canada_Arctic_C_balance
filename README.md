# Canada Arctic fluvial chemistry, constituent export, and ecosystem carbon balance
## Introduction
Source code for the manuscript: Permafrost Landscape History Shapes Fluvial Chemistry, Ecosystem Carbon Balance, and Potential Trajectories of Future Change. DOI: https://zenodo.org/badge/509124741

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
*1_functions.R*: Functions used in some scripts below.  
*2_compile_data.R*: Read and compile raw data, to compile QA/QC'd data into spreadsheet that is archived via the link above and used in scripts summarized below.  
*3_mean_se.R*: Calculate mean ± standard error for all variables.  
*4_stats.R*: Statistics, as detailed in manuscript Methods.  
*5_fig3.R*: Plot 13C-DIC vs. pH.  
*6_fig4a.R.*: Plot carbon species fluxes as proportions of regional total.    
*7_fig4bcd.R*: Plot C yields vs. runoff by study region.  
*8_fig5.R*: Perform RDA, plot results.  
*9_fig6.R*: Plots of total fluvial C flux relative to NEE, by region.  
*10_C_gas_upscale.R*: Estimate mean and range of daily CO2 and CH4 efflux from fluvial networks.  
*11_necb.R*: Net Ecosystem Carbon Balance (NECB) calculations- mean for each site and location.  
*12_figS1_2.R*: Derive mean air temperature and total rainfall over desired interval preceding sampling, graphics for Figures S1, S2.  
*13_figS3.R*: Plot mean daily NEE estimated from SPL4CMDL vs. AmeriFlux stations.  
*14_figS4.R*: Plot Figure S2: Absorbance (a254) from Horiba Aqualog vs. Genesys10.  

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
