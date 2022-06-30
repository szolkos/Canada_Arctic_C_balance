#========================================================================================================#
# 2_compile_data.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-12
# Background: Read and compile data
#========================================================================================================#

  # Load packages ####
    library(Hmisc)
    library(plotrix)
    library(reshape2)
    library(foreign)
    library(ggplot2)
    library(gdata) # to bind columns with different number of rows
    library(vegan) # e.g. for rda function
    library(ecodist)
    library(pvclust)
    library(lubridate) # To convert from date to Julian day
    library(corrplot) # to graphically display correlation matrices
    library(ggfortify) # so ggplot can read PCA objects
    library(RColorBrewer) # for plotting with R color brewer
    library(foreign) # to read .dbf files
    require(purrr) # to read multiple .dbf files
    library(scales) # for creating pie charts
    library(lme4) # for lmer
    library(car) # for VIF
    library(lmPerm) # for permANOVA
    library(writexl) # export data in xlsx format
    library(ggrepel)
    library(vegan)
    library(ggplot2)
    #library(simmr) # For Bayesian mixing models of weathering sources
    #library(boot) # for bootstrap resampling
  
  # Units and conversions
    # Convert ions from mg/L to µM - conversion: [(mg/L x 0.001 g/mg) / MOLAR MASS g/mol] x 1,000,000 µmol/mol = µmol/L; see: https://ocean.ices.dk/Tools/UnitConversion.aspx

  
  # Set working directory ####
    dir <- "/Users/szolkos/Documents/Research/Projects/PhD/Analyses/Data/Data4R/"
    setwd(dir)
  

  # Read in data ####
      
    # Field and geochemistry data
      c5_sampling_periods <- read.csv(paste0(dir,"Ch5/","c5_sampling_periods.csv"), header=T)
      c5ysi <- read.csv(paste0(dir,"Ch5/","Ch5_YSI.csv"), header=T)
      c5ysi$SiteID <- NULL # Remove undesired columns
      c5strmord <- read.csv(paste0(dir,"Ch5/","Ch5_StrmOrd.csv"), header=T)  
      c5cations <- read.csv(paste0(dir,"Ch5/","Ch5_Cations.csv"), header=T)
      c5traceel <- read.csv(paste0(dir,"Ch5/","Ch5_TraceEl.csv"), header=T)
      c5anions <-  read.csv(paste0(dir,"Ch5/","Ch5_Anions.csv"), header=T)
      c5dic <- read.csv(paste0(dir,"Ch5/","Ch5_DIC.csv"), header=T) # QA/QC'd by SZ on Feb 13, 2016
      c5co2sys <- read.csv(paste0(dir,"Ch5/","CO2sys_2020_03_30.csv"), header=T)
      c513c <- read.csv(paste0(dir,"Ch5/","Ch5_13C.csv"), header=T) # 13C for first and last site visits, only for subset of sites
        c513c$DICppm <- NULL # Remove DIC conc. mmnts from U. Ottawa 13C-DIC data
        c513c$DOCppm <- NULL # Remove DOC conc. mmnts from U. Ottawa 13C-DOC data
      c5co2 <- read.csv(paste0(dir,"Ch5/","Ch5_CO2.csv"), header=T)
      c5ch4 <- read.csv(paste0(dir,"Ch5/","Ch5_CH4.csv"), header=T)
      c5cgasflux <- read.csv(paste0(dir,"Ch5/","Ch5_Cgasflux_2022_01_10_update.csv"), header=T) # Flux data from first and last site visits
        c5cgasflux$CO2water <- NULL; c5cgasflux$CO2sat <- NULL; c5cgasflux$CH4water <- NULL; c5cgasflux$CH4sat <- NULL
      c518O <- read.csv(paste0(dir,"Ch5/","Ch5_18O.csv"), header=T) # 18O only for August site visits (n=32)
      c5si <- read.csv(paste0(dir,"Ch5/","Ch5_Si.csv"), header=T) # Si only for August site visits (n=32)
      c5doc <- read.csv(paste0(dir,"Ch5/","Ch5_DOC.csv"), header=T)
      c5suva <- read.csv(paste0(dir,"Ch5/","Ch5_SUVA.csv"), header=T)
      c5sr <- read.csv(paste0(dir,"Ch5/","Ch5_SR.csv"), header=T)
      c5abs <- read.csv(paste0(dir,"Ch5/","Ch5_abs.csv"), header=T) # For comparing mmnts of crctd a254 between ARI machine + Aqualog, to test for potential effects of storage time
      c5N <- read.csv(paste0(dir,"Ch5/","Ch5_N.csv"), header=T)
      c5tsspoc <- read.csv(paste0(dir,"Ch5/","Ch5_TSS.csv"), header=T) # QA/QC'd by SZ on Feb 13, 2016; TSS for all visits, for subset of sites
      c5q <- read.csv(paste0(dir,"Ch5/","Ch5_Discharge.csv"), header=T)
      #c5elcatch <- read.csv(paste0(dir,"Ch5/","Ch5_SiteElevCatchArea.csv"), header=T); c5elcatch$SiteID <- NULL # Remove undesired columns
      c5_modis_gpp_amflux <- read.csv(paste0(dir,"Ch5/","MODIS_GPP_at_AmeriFlux_stations.csv"), header=T)
      
    # Physiographic variables
      # Hydrology, surficial geology, bedrock geology
        c5hydro_geol <- read.csv(paste0(dir,"Ch5/","Ch5_Lndscp_hydro_geol.csv"), header=T)
        c5_veg <- read.csv(paste0(dir,"Ch5/","Ch5_Veg.csv"), header=T) # GPP converted from kgC/m2/8d (MODIS GPP product in GEE, scaling factor of 0.0001) to gC/m2/d by: GEE_GPP*0.0001*1000/8
      # Mean slope
        c5meanslp <- droplevels(subset(read.dbf(paste0(dir,"Ch5/","sheds_slopedegrees.dbf")), select=c("NAME","MEAN")))
        c5meanslpap07 <- droplevels(subset(read.dbf(paste0(dir,"Ch5/","sheds_slopedegrees_ap07.dbf")), select=c("NAME","MEAN")))
        c5meanslpap08 <- droplevels(subset(read.dbf(paste0(dir,"Ch5/","sheds_slopedegrees_ap08.dbf")), select=c("NAME","MEAN")))
        c5meanslp <- rbind(c5meanslp,c5meanslpap07,c5meanslpap08)
        names(c5meanslp) <- c("SiteID","MeanSlope")
      # Compile
        c5hydro_geol$SiteID <- paste0(c5hydro_geol$Loc, "0", c5hydro_geol$Site)
        lndscp <- merge(x=c5hydro_geol, y=c5meanslp, by="SiteID", all.x=T, all.y=T, sort=T) 
        c5lndscp <- merge(x=lndscp, y=c5_veg, by=c("Loc","Site"), all.x=T, all.y=T, sort=T)
        c5lndscp <- subset(c5lndscp, select=c("Date","Loc","Site","SiteID","ShedAream2","MeanEl","MeanSlope",
                                              "MeanEVI","MeanGPP","SOCC30cm","SOCC100cm","LakesPondpcnt",
                                              "AlluvialPcnt","ColluvialPcnt","FluvialPcnt","MorainePcnt","OrganicPcnt",
                                              "CarbonatePcnt","SilicatePcnt"))
        
    # Climate data
      clim <- read.csv(paste0(dir,"Ch5/","Ch5_climate.csv"), header=T)
      clim$Date_clim <- format(as.POSIXct(clim$DateTime,format="%m/%d/%y %H:%M"),"%m/%d/%y")
      clim$Time_clim <- format(as.POSIXct(clim$DateTime,format="%m/%d/%y %H:%M"),"%H:%M")
      clim$DateTime <- paste0(clim$Date_clim," ",clim$Time_clim)
      clim <- subset(clim, select=c("DateTime","Date_clim","Time_clim","Loc","AirTavg","RainTot","recno"))
      
  # Create a dataframe of sampling (YSI) time rounded to the nearest hour (use to merge climate data)
    rndtime <- as.data.frame(matrix(ncol=1,nrow=nrow(c5ysi)))
    names(rndtime) <- "Time"
    j=1
    for(i in 1:nrow(c5ysi)){
      
      h <- as.numeric(substr(as.character(c5ysi[i,]$Time), 1, 2)) # Extract hour
      m <- as.numeric(substr(as.character(c5ysi[i,]$Time), 4, 5)) # Extract minute
      if(m >= 30){
        h2 <- h+1
        Times <- paste0(h2,":00") # If the time is at the half hour later, round up to next hour
      }
      else{
        Times <- paste0(h,":00") # If the time is before the half hour later, round down to start of hour
      }
      rndtime[j,1] <- Times
      j <- j+1
    }
    c5ysi$Time <- rndtime
 
  # Calculate pCO2, pCH4 ####
        
    # Merge YSI and C gas data, to calculate partial pressures
      # 'c5co2' to 'c5ysi', creating the df
        c5df <- merge(x=c5ysi, y=c5co2, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T) # 'NA' inserted where rows incomplete
      # 'c5ch4' to 'c5df'
        c5df <- merge(x=c5df, y=c5ch4, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
  
    # Bind into one df
      Ch5gasDIC <- as.data.frame(matrix(nrow=nrow(c5df), ncol=9))
      colnames(Ch5gasDIC) <- c("Date","Site","Loc","CO2H2OuM","CO2sat","pCO2","CH4H2OuM","CH4sat","pCH4")
      j=1
      for(i in 1:nrow(Ch5gasDIC)){
        Ch5gasDIC[j,1] <- pCO2(c5df,i)[1] # Date
        Ch5gasDIC[j,2] <- pCO2(c5df,i)[2] # Site
        Ch5gasDIC[j,3] <- pCO2(c5df,i)[3] # Loc
        Ch5gasDIC[j,4] <- round(as.numeric(pCO2(c5df,i)[4]),2) # CO2 (µM)
        Ch5gasDIC[j,5] <- round(as.numeric(pCO2(c5df,i)[5]),2) # CO2sat
        Ch5gasDIC[j,6] <- round(as.numeric(pCO2(c5df,i)[6]),2) # pCO2
        Ch5gasDIC[j,7] <- round(as.numeric(pCH4(c5df,i)[4]),3) # CH4 (µM)
        Ch5gasDIC[j,8] <- round(as.numeric(pCH4(c5df,i)[5]),3) # CH4sat
        Ch5gasDIC[j,9] <- round(as.numeric(pCH4(c5df,i)[6]),2) # pCH4
        j <- j+1
      }
      
    # Merge pCO2 and pCH4 ('c5CgasDIC') to 'c5df'
      c5df <- merge(x=c5df, y=Ch5gasDIC, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T) 
  
    # Exclude columns that were used to calculate pCO2, pCH4 and are now extraneous
      c5df <- c5df[,!names(c5df) %in% c("Tcollection","SyringeNoAtm","SyringeNoH2O","Tanalysis","AtmH2O","VolColln","VolHdspc","VolCollnCH4","VolHdspcCH4","TempAnCH4")] # Exclude desired columns; "Atmatm",

      
  # Merge remaining data ####
      
    # Where 'Date', 'Loc', and 'Site' are matching, bind from:
      # 'c5cations' to 'c5df', creating the df
        c5df <- merge(x=c5df, y=c5cations, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
        c5df$CauM <- ((c5df$Ca/1000)/40.078)*1000000 # Ca = 40.078 g/mol
        c5df$MguM <- ((c5df$Mg/1000)/24.305)*1000000 # Mg = 24.305 g/mol
        c5df$NauM <- ((c5df$Na/1000)/22.9898)*1000000
        c5df$KuM <- ((c5df$K/1000)/39.0983)*1000000
      # 'c5traceel' to 'c5df'
        c5df <- merge(x=c5df, y=c5traceel, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
        c5df$FeuM <- ((c5df$Fe/1000)/55.845)*1000000
        c5df$AluM <- ((c5df$Al/1000000)/26.981539)*1000000
      # 'c5anions' to 'c5df'
        c5df <- merge(x=c5df, y=c5anions, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
        c5df$SO4uM <- ((c5df$SO4/1000)/96.06)*1000000 # SO4 = 96.06 g/mol
        c5df$CluM <- ((c5df$Cl/1000)/35.453)*1000000
      # 'c5si' to 'c5df'
        c5df <- merge(x=c5df, y=c5si, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
        c5df$SiuM <- ((c5df$Si/1000)/28.0855)*1000000
      # 'c5dic' to 'c5df'
        c5df <- merge(x=c5df, y=c5dic, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
      # 'c5co2sys' to 'c5df'
        c5df <- merge(x=c5df, y=c5co2sys, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)      
      # 'c5doc' to 'c5df'
        c5df <- merge(x=c5df, y=c5doc, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)    
        c5df$DOCuM <- ((c5df$DOC/1000)/12.0107)*1000000 # C = 12.0107 g/mol
      # 'c5suva' to 'c5df'
        c5df <- merge(x=c5df, y=c5suva, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)    
      # 'c5sr' to 'c5df'
        c5df <- merge(x=c5df, y=c5sr, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)       
      # 'c5N' to 'c5df'
        c5df <- merge(x=c5df, y=c5N, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
        c5df$NH4uM <- ((c5df$NH4ugL/1000000)/14.0067)*1000000 # N = 14.0067 g/mol (BASL reports e.g. DIN as N ug/L)
        c5df$NO3uM <- ((c5df$NO3ugL/1000000)/14.0067)*1000000
        c5df$DINuM <- ((c5df$DINugL/1000000)/14.0067)*1000000
        c5df$DONuM <- ((c5df$DONugL/1000000)/14.0067)*1000000
        c5df$TDNuM <- ((c5df$TDNugL/1000000)/14.0067)*1000000
      # 'c5cgasflux' to 'c5df'
        c5df <- merge(x=c5df, y=c5cgasflux, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
      # 'c513c' to 'c5df'
        c5df <- merge(x=c5df, y=c513c, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
      # 'c518O' to 'c5df'
        c5df <- merge(x=c5df, y=c518O, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)   
      # 'c5q' to 'c5df'
        c5df <- merge(x=c5df, y=c5q, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)       
      # 'c5tsspoc' to 'c5df'
        c5df <- merge(x=c5df, y=c5tsspoc, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)   
        c5df$POCuM <- ((c5df$POCugL/1000000)/12.0107)*1000000
        c5df$PICuM <- c5df$POCuM*0.154
        c5df$PONuM <- ((c5df$PONugL/1000000)/14.0067)*1000000
        c5df$TCuM <- c5df$DICuM + c5df$PICuM + c5df$DOCuM + c5df$POCuM
      # 'c5lndscp' to 'c5df'
        c5df <- merge(x=c5df, y=c5lndscp, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
      # 'c5strmord' to 'c5df'
        c5df <- merge(x=c5df, y=c5strmord, by=c("Loc","Site"), all.x=T, all.y=T, sort=T)
      # 'c5elcatch' to 'c5df'
        #c5df <- merge(x=c5df, y=c5elcatch, by=c("Loc","Site"), all.x=T, all.y=T, sort=T) 
      # 'c5_sampling_periods' to 'c5df'
        c5df <- merge(x=c5df, y=c5_sampling_periods, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
      # 'clim' to 'c5df'
        c5df$DateTime <- paste0(c5df$Date," ",as.matrix(c5df$Time))
        #c5df <- merge(x=c5df, y=clim, by=c("Loc","DateTime"), all.x=T, sort=T)
      ## Merge derived historical climate data to climate dataframe (user must specify # prior hours in next code line)
        hxclimT <- merge(x=clim_mrg, y=hx_clim(HrsInPast=12), by=c("Date","Loc","Site"), all.x=T)
        hxclimT <- merge(x=hxclimT, y=hx_clim(HrsInPast=24), by=c("Date","Loc","Site"), all.x=T)
        hxclimT <- merge(x=hxclimT, y=hx_clim(HrsInPast=48), by=c("Date","Loc","Site"), all.x=T)
        hxclimT <- merge(x=hxclimT, y=hx_clim(HrsInPast=72), by=c("Date","Loc","Site"), all.x=T)
        hxclimT <- merge(x=hxclimT, y=hx_clim(HrsInPast=96), by=c("Date","Loc","Site"), all.x=T)
        hxclimT <- hxclimT[,!names(hxclimT) %in% c("Time")]
      # 'hxclimT' to 'c5df'
        c5df <- merge(x=c5df, y=hxclimT, by=c("Date","Loc","Site"), all.x=T, all.y=T, sort=T)
        
      # Add H+, SiteID, SampleID
        c5df$Hplus <- 10^-c5df$pH
        c5df$SiteID <- paste0(c5df$Loc,"-",c5df$Site)
        c5df$SampleID <- paste0(c5df$Loc,"-",c5df$Site,"-",c5df$Date)
        
      # Calculate dexcess, following Turner et al. (2014, PPP)
        c5df$dexcess <- c5df$d2H-8*c5df$d18O
      
      # Calculate fluxes (mmol/s) as: conc (µmol/L) * 1000 L/m3 * Q (m3/s) / 1000 µmol/mmol
        c5df$CO2flux <- c5df$CO2uM * c5df$Q * 1000 / 1000
        c5df$CH4flux <- c5df$CH4H2OuM * c5df$Q * 1000 / 1000
        c5df$HCO3flux <- c5df$HCO3uM * c5df$Q * 1000 / 1000
        c5df$DOCflux <- c5df$DOCuM * c5df$Q * 1000 / 1000
        c5df$DICflux <- c5df$DICuM * c5df$Q * 1000 / 1000
        c5df$POCflux <- c5df$POCuM * c5df$Q * 1000 / 1000
        c5df$PICflux <- c5df$PICuM * c5df$Q * 1000 / 1000
        c5df$TCflux <- c5df$TCuM * c5df$Q * 1000 / 1000
        
      # Calculate yields (µmol/m2/d) as: (conc ((µmol/L) * Q (m3/s) * 1000 L/m3) / area (m2)) * 86400 sec/day
        c5df$DICyld <- ((c5df$DICuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$CO2yld <- ((c5df$CO2uM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$CH4yld <- ((c5df$CH4H2OuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$HCO3yld <- ((c5df$HCO3uM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$PICyld <- ((c5df$PICuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$DOCyld <- ((c5df$DOCuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$POCyld <- ((c5df$POCuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$TCyld <- ((c5df$TCuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$Runoff <- (c5df$Q / c5df$ShedAream2) * 100 * 86400 *10 # Runoff in mm/d
        c5df$TDNyld <- ((c5df$TDNuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$DONyld <- ((c5df$DONuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$DINyld <- ((c5df$DINuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$Cayld <- ((c5df$CauM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$Mgyld <- ((c5df$MguM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$Nayld <- ((c5df$NauM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$Kyld <- ((c5df$KuM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$SO4yld <- ((c5df$SO4uM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        c5df$Clyld <- ((c5df$CluM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        #c5df$yld <- ((c5df$uM * c5df$Q * 1000) / c5df$ShedAream2) * 86400
        
      # Convert C gas fluxes from instantaneous to daily
        c5df$jCO2daily <- c5df$jCO2*86400
        c5df$jCH4daily <- c5df$jCH4*86400
      
      # Determine Julian Day
        JulianDay <- as.data.frame(as.Date(c5df$Date, "%m/%d/%y")); names(JulianDay) <- "JulianDay"; JulianDay <- (yday(JulianDay$JulianDay)) # Add column to ysi for Julian day
        c5df <- cbind(c5df, JulianDay) # Convert with yday into a new column "julian"
      
      # Calculate Cexcess
        c5df$CO2excess <- c5df$CO2H2OuM/c5df$CO2sat
        c5df$CH4excess <- c5df$CH4H2OuM/c5df$CH4sat
      
      # Omit pCH4 values < 0
        c5df$pCH4[c5df$pCH4<0] <- NA
        
      # Exclude columns that are now extraneous (e.g., used to calculate pCO2, pCH4)
        c5df <- c5df[,!names(c5df) %in% c("DateTime","Date_clim","Time_clim","DIC","CO2mgL","HCO3mgL","CO3mgL","Talk","DOC","a254","Na","K","Ca","Mg","SO4","Cl","Fe","Al","Si")] # Exclude desired columns
        
      # Subset data for Ch. 5
        c5df <- subset(c5df, select=c("SampleID","SiteID","JulianDay","Date","Time","SmplPer","Loc","Site","Strahler",
                                      "Atmatm","AirT","Temp","Cond","DOpcnt","DOmgL","pH","Hplus","CO2atm","pCO2","CO2H2OuM","CO2sat","CO2excess","CH4atm","pCH4","CH4H2OuM","CH4excess","CH4sat",
                                      "TCuM","DICuM","CO2uM","HCO3uM","CO3uM","POCuM","PICuM","TSS",
                                      "kCO2","jCO2","jCO2daily","kCH4","jCH4","jCH4daily",
                                      "CO2flux","CH4flux","HCO3flux","DOCflux","DICflux","POCflux","PICflux","TCflux",
                                      "DICyld","CH4yld","CO2yld","HCO3yld","PICyld","DOCyld","POCyld","TCyld","TDNyld","DINyld","DONyld","Cayld","Mgyld","Nayld","Kyld","SO4yld","Clyld",
                                      "CauM","MguM","NauM","KuM","SO4uM","CluM","FeuM","AluM","SiuM", # Ions
                                      "DOCuM","SUVA254","SR",
                                      "TDNuM","DONuM","DINuM","NH4uM","NO3uM","PONuM", # Nitrogen
                                      "DI13C","DO13C","d2H","d18O","dexcess", # Isotopes
                                      "W","Davg","Vavg","Q","Runoff", # Hydrology
                                      "ShedAream2","MeanEl","MeanSlope","MeanEVI","MeanGPP", # Topography/vegetation
                                      "LakesPondpcnt","SOCC30cm","SOCC100cm","AlluvialPcnt","ColluvialPcnt","FluvialPcnt","MorainePcnt","OrganicPcnt",
                                      "CarbonatePcnt","SilicatePcnt", # Geology
                                      "AirTavg","AirTavg12","AirTavg24","AirTavg48","AirTavg72","AirTavg96","RainTot","RainTot12","RainTot24","RainTot48","RainTot72","RainTot96")) # Climate
          
      # Set plotting order
        c5df$Loc <- factor(c5df$Loc, levels=c("BB","PP","PAP","AP"))

      # Set plotting colors
        regional_colors <- c("blue","purple","green3","orange")
        regional_colors_strmord <- c("blue","blue","blue","blue","purple","purple","purple","purple","green3","green3","green3","green3","orange","orange","orange","orange")
        
      # Optional: add molar ratios for Bayesian mixing analysis
        #c5df$HCO3Na <- c5df$HCO3uM/c5df$NauM
        #c5df$MgNa <- c5df$MguM/c5df$NauM
        #c5df$ClNa <- c5df$CluM/c5df$NauM
        #c5df$CaNa <- c5df$CauM/c5df$NauM
        #c5df$SiHCO3 <- c5df$SiuM/c5df$HCO3uM
        
        
  # Optional: Export data ####
        
    # Export c5df
      #write.csv(c5df, "/Users/szolkos/Desktop/c5df.csv", row.names=F)
        
    # Export data for CO2sys  
      #write.csv(subset(c5df, select=c("Date","Loc","Site","Temp","DICuM","pCO2","pH","Atmatm")), "/Users/szolkos/Desktop/forco2sys.csv", row.names=F)
      
    