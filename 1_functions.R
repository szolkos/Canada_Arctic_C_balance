#===========================================================================================================#
# 1_functions.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2018-03-17
# Background: Functions for Canada Arctic C balance manuscript
#===========================================================================================================#

##### Statistics
  
  # Function for calculating p-value for entire model (from: http://www.gettinggeneticsdone.com/2011/01/rstats-function-for-extracting-f-test-p.html)
    lmp <- function(modelobject){
    if(class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }

  # Function for calculating p-value for entire model and creating legend for plots, with adj R2 and p (Modified from: http://www.gettinggeneticsdone.com/2011/01/rstats-function-for-extracting-f-test-p.html)
    Lgnd <- function(mdl){
      if(class(mdl) != "lm") stop("Not an object of class 'lm' ")
      
    # Extract adjusted R2
      Rsq <- round(summary(mdl)$adj.r.squared,2)
      
    # Extract p-value for entire model
      f <- summary(mdl)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      if(p < 0.001){
        p <- 0.001
        lgnd <- c(bquote(italic(R)^2 == .(Rsq)), bquote(italic(p) < .(p)))
      }
      else if(0.001 < p & p < 0.01){
        p <- 0.01
        lgnd <- c(bquote(italic(R)^2 == .(Rsq)), bquote(italic(p) < .(p)))
      }
      else{
        p <- round(p,2)
        lgnd <- c(bquote(italic(R)^2 == .(Rsq)), bquote(italic(p) == .(p)))
      }
      return(lgnd)
    }
    
  # Function for calculating correlation significance for corrplot (see Ch. 5 code)
    cor.mtest <- function(mat, ...){
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat<- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      }
      colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
      p.mat
    }
    
##### Plotting
    
  # Function for numerical y-axis labels, no commas, no extraneous decimal places
  ## Source: https://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot
    NotFancy <- function(l) {
      l <- format(l, scientific = FALSE)
      parse(text=l)
    }
    
##### Geochemistry

  # Calculate CO2 in water
    # Inputs are from CO2 and YSI field data
    pCO2 <- function(df,obs){
    
    co2ysi <- df[obs,]
    
    # INPUTS: Extract data for input variables from master spreadsheet
    date <- as.character(co2ysi$Date) # as.character(co2ysi[obs,]$Date) 
    site <- as.character(co2ysi$Site)
    loc <- as.character(co2ysi$Loc)
    
    ## CO2, as measured on IRGA
    co2.h20.ppm <- co2ysi$CO2H2O
    co2.atm.ppm <- co2ysi$CO2atm
    
    ## Water temp (C): from YSI
    water.temp <- co2ysi$Temp
    ## Water temp, in Kelvins: convert from ˚C to K (i.e. convert to absolute temperature)
    water.temp.k <- water.temp+273.15
    
    ## Air temp (C): at time of sampling, from field thermometer
    air.temp <- co2ysi$AirT
    ## Air temp, in Kelvin
    air.temp.k <- air.temp+273.15
    
    ## Atmospheric pressure (atm)
    Atm <- co2ysi$Atm
    
    ## Pressure (kPa): convert atm to kPa (Atm*101.325 = kPa)
    pressure <- co2ysi$Atm*101.325
    
    ## Water vol: from sample syringe
    water.vol <- co2ysi$VolColln
    
    ## Headpsace vol: from sample syringe
    headspace.vol <- co2ysi$VolHdspc
    
    # CALCULATIONS- pCO2 calculations follow Tank et al. 2009 Ecosystems
    ## "Headspace pCO2 values were corrected to give lakewater pCO2 using Henry's constant corrected for 
    ## water temperature (Weiss 1974), and taking into account atmospheric CO2 introduced into the 
    ## sampling bottle." (Tank et al. 2009 p147)
  
    ## Henry's constant corrected for field water temperature. K0 From Weiss 1974, Eq.12 p207, Table 1 (Units of K0 in mol/L*atm). (mol/L*atm).
    ## In R, 'log' = ln
    KhCO2 <- exp(-58.0931+90.5069*(100/water.temp.k)+22.294*log(water.temp.k/100)) # Solubility depends on water temperature
    
    ## CO2 in water (umol)
    # (ppm/1,000,000)*(mol/L*atm)*(mL/1,000)*(1,000,000 µmol/mol) => (µmol/atm)*atm => µmol
    # *Weiss (1974) defines Bunsen coefficient as vol. of gas... where total pressure and fugactity are both 1 atm (p205)
    co2.water <- co2.h20.ppm*Atm*KhCO2*(water.vol/1000)

    ## CO2 in headspace (umol)
    co2.hdspc <- co2.h20.ppm*Atm*(headspace.vol/1000)/(0.082057*water.temp.k)
    
    ## CO2 (i.e. atmospheric) added for equilibration (umol)
    co2.added <- co2.atm.ppm*Atm*(headspace.vol/1000)/(0.082057*air.temp.k)
    
    ## CO2 water (µmol/L), corrected for atmospheric CO2
    co2.water <- (co2.water + co2.hdspc - co2.added)/(water.vol/1000)
    
    ## CO2 sat (conc water would have, if it were in equilibrium with the air (Cole and Caraco 2001, p105))
    co2.sat <- co2.atm.ppm*Atm*KhCO2
    
    ## pCO2 in stream ppm (uatm)
    pco2 <- co2.water/KhCO2
  
    return(c(date,site,loc,co2.water,co2.sat,pco2))
  }
  
  # Calculate CH4 in water
    pCH4 <- function(df,obs){
  
    ch4ysi <- df[obs,]
    
    # INPUTS: Extract data for input variables from master spreadsheet
    date <- as.character(ch4ysi$Date)
    site <- as.character(ch4ysi$Site)
    loc <- as.character(ch4ysi$Loc)
    
    # INPUTS: Extract data for input variables from master spreadsheet
    
    ## CH4 (ppm): as measured on GC
    ch4.h20.ppm <- ch4ysi$CH4H2O
    ch4.atm.ppm <- ch4ysi$CH4atm
    
    ## Water temp (C): from YSI
    water.temp <- ch4ysi$Temp
    ## Water temp, in Kelvins: convert from ˚C to K (i.e. convert to absolute temperature)
    water.temp.k <- water.temp+273.15
    
    ## Air temp (C): at time of sampling, from field thermometer
    air.temp <- ch4ysi$AirT
    ## Air temp, in Kelvin
    air.temp.k <- air.temp+273.15
    
    ## Atmospheric pressure (atm)
    Atm <- ch4ysi$Atm
    
    ## Pressure (kPa): convert atm to kPa (Atm*101.325 = kPa)
    pressure <- ch4ysi$Atm*101.325
    
    ## Water vol: from sample syringe
    water.vol <- ch4ysi$VolColln #VolCollnCH4
    
    ## Headpsace vol: from sample syringe
    headspace.vol <- ch4ysi$VolHdspc #VolHdspcCH4
    
    # CALCULATIONS- pCH4 calculations follow Weiss 1970 (DSR), Weiss et al. 1974 (MC), Wiesenburg and Guinasso 1979 (JCED)
    
    # Calculate Bunsen solubility coefficient (dimensionless) for methane
      # Weiss 1970 DSR report equation (Eq. 3 (p726))
      # Wiesenburg and Guinasso 1979 JCED derive constants for CH4 (Eq. 1 with constants from Table I), units are mol/L*atm
      # log(Bunsen) = ln(mol fxn gas) + A1 + A2*(100/T) + A3*log(T/100) *Freshwaters, so salinity terms assumed negligible
      # Bunsen = Bunsen solubility coefficient
      # T = absolute temperature (i.e. T in Kelvin)
      # S‰ = salinity (parts per thousand)
      # A1-A3, B1-B3 = constants: -68.8862, +101.4956, +28.7314, -0.076146, 0.043970, -0.0068672
      # log(Bunsen) = -68.8862+101.4956*(100/water.temp.k)+28.7314*log(water.temp.k/100)-62.0757*(water.temp.k/100) # Where salinity = 0
      Bunsen_uncorrected <- exp(-68.8862 + 101.4956*(100/water.temp.k) + 28.7314*log(water.temp.k/100))
    # Correct Bunsen to appropriate units (mol/L*atm) following Sander 2015 (Atm Chem Phys, Table 1)
      KhCH4 <- Bunsen_uncorrected*0.044615
    
    ## CH4 in water: µmol = (µmol/mol) * atm * (mol/L* 1/atm) * (mL/1 * 1L/1000mL)
    ch4.water <- ch4.h20.ppm*Atm*KhCH4*(water.vol/1000)
    
    ## CH4 in headspace (umol)
    ch4.hdspc <- ch4.h20.ppm*Atm*(headspace.vol/1000)/(0.082057*water.temp.k)
    
    ## CH4 (i.e. atmospheric) added for equilibration (umol)
    ch4.added <- ch4.atm.ppm*Atm*(headspace.vol/1000)/(0.082057*air.temp.k)
    
    ## CH4 water (µmol/L), corrected for atmospheric CH4
    ch4.water <- (ch4.water + ch4.hdspc - ch4.added)/(water.vol/1000)
    
    ## CH4 sat (conc water would have, if it were in equilibrium with the air (Cole and Caraco 2001, p105))
    ch4.sat <- ch4.atm.ppm*Atm*KhCH4
    
    ## pCH4 in stream ppm (uatm)
    pch4 <- ch4.water/KhCH4
    
    return(c(date,site,loc,ch4.water,ch4.sat,pch4))
    }
    