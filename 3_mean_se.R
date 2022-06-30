#========================================================================================================#
# 3_mean_se.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Calculate mean ± standard error for all variables
#========================================================================================================#

  # BACKGROUND ####
  
    # No units, functions, or other pertinent background information to summarize here
    

  # DATA PROCESSING AND ANALYSIS ####

    # Calculate for all parameters, store in dataframe
      #params <- c("Loc","Site","Temp","n","Tempse","Cond","n","Condse","pH","n","pHse","DICuM","n","DICuMse","CO2uM","n","CO2uMse","HCO3uM","n","HCO3uMse","CauM","n","CauMse","MguM","n","MguMse","NauM","n","NauMse","KuM","n","KuMse","SO4uM","n","SO4uMse","CluM","n","CluMse","FeuM","n","FeuMse","AluM","n","AluMse","SiuM","n","SiuMse","DOCuM","n","DOCuMse","SUVA254","n","SUVA254se","SR","n","SRse","DI13C","n","DI13Crange","DO13C","n","DO13Crange","d18O","n","d18Orange","d2H","n","d2Hrange","dexcess","n","dexcessse","POCuM","n","POCuMse","pCO2","n","pCO2se","pCH4","n","pCH4se","jCO2","n","jCO2se","kCO2","n","kCO2se","jCH4","n","jCH4se","kCH4","n","kCH4se","Q","n","Qse","CO2yld","n","CO2yldse","CH4yld","n","CH4yldse","HCO3yld","n","HCO3yldse","PICyld","n","PICyldse","DOCyld","n","DOCyldse","POCyld","n","POCyldse","DINuM","n","DINuMse","DONuM","n","DONuMse","TDNuM","n","TDNuMse","TSS","n","TSSse","Vavg","n","Vavgse","MeanEVI","n","MeanEVIse","MeanGPP","n","MeanGPPse","Runoff","n","Runoffse","DOmgL","n","DOmgLse","TCuM","n","TCuMse","TCyld","n","TCyldse","PICuM","n","PICuMse","DICyld","n","DICyldse","W","n","Wse","Runoff","n","Runoffse","TDNyld","n","TDNyldse","SOCC30cm","SOCC100cm","ColluvialPcnt","CarbonatePcnt","MeanEl","MeanSlope","LakesPondpcnt","ShedAream2","Strahler")
      #col_names <- c("Loc","Site","Temp","n","Tempse","Cond","n","Condse","pH","n","pHse","DICuM","n","DICuMse","CO2uM","n","CO2uMse","HCO3uM","n","HCO3uMse","CauM","n","CauMse","MguM","n","MguMse","NauM","n","NauMse","KuM","n","KuMse","SO4uM","n","SO4uMse","CluM","n","CluMse","FeuM","n","FeuMse","AluM","n","AluMse","SiuM","n","SiuMse","DOCuM","n","DOCuMse","SUVA254","n","SUVA254se","SR","n","SRse","DI13C","n","DI13Crange","DO13C","n","DO13Crange","d18O","n","d18Orange","d2H","n","d2Hrange","dexcess","n","dexcessse","POCuM","n","POCuMse","pCO2","n","pCO2se","pCH4","n","pCH4se","jCO2","n","jCO2se","kCO2","n","kCO2se","jCH4","n","jCH4se","kCH4","n","kCH4se","Q","n","Qse","CO2yld","n","CO2yldse","CH4yld","n","CH4yldse","HCO3yld","n","HCO3yldse","PICyld","n","PICyldse","DOCyld","n","DOCyldse","POCyld","n","POCyldse","DINuM","n","DINuMse","DONuM","n","DONuMse","TDNuM","n","TDNuMse","TSS","n","TSSse","Vavg","n","Vavgse","MeanEVI","n","MeanEVIse","MeanGPP","n","MeanGPPse","Runoff","n","Runoffse","DOmgL","n","DOmgLse","TCuM","n","TCuMse","TCyld","n","TCyldse","PICuM","n","PICuMse","DICyld","n","DICyldse","W","n","Wse","Runoff","n","Runoffse","TDNyld","n","TDNyldse","SOCC30cm","SOCC100cm","ColluvialPcnt","CarbonatePcnt","MeanEl","MeanSlope","LakesPondpcnt","ShedAream2","Strahler")
      params <- c("Loc","Site","Temp","n","Tempse","Cond","n","Condse","pH","n","pHse","DICuM","n","DICuMse","CO2uM","n","CO2uMse","HCO3uM","n","HCO3uMse",
                  "CauM","n","CauMse","MguM","n","MguMse","NauM","n","NauMse","KuM","n","KuMse","SO4uM","n","SO4uMse","CluM","n","CluMse","FeuM","n","FeuMse","AluM","n","AluMse","SiuM","n","SiuMse",
                  "DOCuM","n","DOCuMse","SUVA254","n","SUVA254se","SR","n","SRse","DI13C","n","DI13Crange","DO13C","n","DO13Crange","d18O","n","d18Orange","d2H","n","d2Hrange","dexcess","n","dexcessse","POCuM","n","POCuMse","pCO2","n","pCO2se","pCH4","n","pCH4se","jCO2","n","jCO2se","kCO2","n","kCO2se","jCH4","n","jCH4se","kCH4","n","kCH4se","Q","n","Qse",
                  "CO2flux","n","CO2fluxse","CH4flux","n","CH4fluxse","HCO3flux","n","HCO3fluxse","DOCflux","n","DOCfluxse","DICflux","n","DICfluxse","POCflux","n","POCfluxse","PICflux","n","PICfluxse","TCflux","n","TCfluxse",
                  "CO2yld","n","CO2yldse","CH4yld","n","CH4yldse","HCO3yld","n","HCO3yldse","PICyld","n","PICyldse","DOCyld","n","DOCyldse","POCyld","n","POCyldse","DINuM","n","DINuMse","DONuM","n","DONuMse","TDNuM","n","TDNuMse","TSS","n","TSSse","Vavg","n","Vavgse","MeanEVI","n","MeanEVIse","MeanGPP","n","MeanGPPse","Runoff","n","Runoffse","DOmgL","n","DOmgLse","TCuM","n","TCuMse","TCyld","n","TCyldse","PICuM","n","PICuMse","DICyld","n","DICyldse","W","n","Wse","Runoff","n","Runoffse","TDNyld","n","TDNyldse","DICyld","n","DICyldse",
                  "NH4uM","n","NH4uMse","NO3uM","n","NO3uMse","DOpcnt","n","DOpcntse",
                  "SOCC30cm","SOCC100cm","ColluvialPcnt","CarbonatePcnt","MeanEl","MeanSlope","LakesPondpcnt","ShedAream2","Strahler","FluvialPcnt","MorainePcnt","OrganicPcnt","SilicatePcnt")
      col_names <- c("Loc","Site","Temp","n","Tempse","Cond","n","Condse","pH","n","pHse","DICuM","n","DICuMse","CO2uM","n","CO2uMse","HCO3uM","n","HCO3uMse",
                     "CauM","n","CauMse","MguM","n","MguMse","NauM","n","NauMse","KuM","n","KuMse","SO4uM","n","SO4uMse","CluM","n","CluMse","FeuM","n","FeuMse","AluM","n","AluMse","SiuM","n","SiuMse",
                     "DOCuM","n","DOCuMse","SUVA254","n","SUVA254se","SR","n","SRse","DI13C","n","DI13Crange","DO13C","n","DO13Crange","d18O","n","d18Orange","d2H","n","d2Hrange","dexcess","n","dexcessse","POCuM","n","POCuMse","pCO2","n","pCO2se","pCH4","n","pCH4se","jCO2","n","jCO2se","kCO2","n","kCO2se","jCH4","n","jCH4se","kCH4","n","kCH4se","Q","n","Qse",
                     "CO2flux","n","CO2fluxse","CH4flux","n","CH4fluxse","HCO3flux","n","HCO3fluxse","DOCflux","n","DOCfluxse","DICflux","n","DICfluxse","POCflux","n","POCfluxse","PICflux","n","PICfluxse","TCflux","n","TCfluxse",
                     "CO2yld","n","CO2yldse","CH4yld","n","CH4yldse","HCO3yld","n","HCO3yldse","PICyld","n","PICyldse","DOCyld","n","DOCyldse","POCyld","n","POCyldse","DINuM","n","DINuMse","DONuM","n","DONuMse","TDNuM","n","TDNuMse","TSS","n","TSSse","Vavg","n","Vavgse","MeanEVI","n","MeanEVIse","MeanGPP","n","MeanGPPse","Runoff","n","Runoffse","DOmgL","n","DOmgLse","TCuM","n","TCuMse","TCyld","n","TCyldse","PICuM","n","PICuMse","DICyld","n","DICyldse","W","n","Wse","Runoff","n","Runoffse","TDNyld","n","TDNyldse","DICyld","n","DICyldse",
                     "NH4uM","n","NH4uMse","NO3uM","n","NO3uMse",
                     "SOCC30cm","SOCC100cm","ColluvialPcnt","CarbonatePcnt","MeanEl","MeanSlope","LakesPondpcnt","ShedAream2","Strahler","FluvialPcnt","MorainePcnt","OrganicPcnt","SilicatePcnt")
      c5means <- as.data.frame(matrix(nrow=length(unique(c5df$SiteID)), ncol=length(params)))
      names(c5means) <- params
      j=1
      for(i in unique(c5df$SiteID)){
        c5means[j,1] <- as.character(unique(c5df$Loc[c5df$SiteID==i]))
        c5means[j,2] <- unique(c5df$Site[c5df$SiteID==i])
        
        c5means[j,3] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[3], c5df$SiteID==i)))) # Temp
        c5means[j,4] <- p3n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[3], c5df$SiteID==i))))) # n, for calculating standard error (next line)
        c5means[j,5] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[3], c5df$SiteID==i)))))/sqrt(p3n)
        
        c5means[j,6] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[6], c5df$SiteID==i)))) # Cond
        c5means[j,7] <- p6n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[6], c5df$SiteID==i)))))
        c5means[j,8] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[6], c5df$SiteID==i)))))/sqrt(p6n)
        
        #c5means[j,9] <- pHmean <- -log10(as.numeric(colMeans(na.omit(subset(c5df, select=params[9], c5df$SiteID==i))))) # pH
        #c5means[j,10] <- p9n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[9], c5df$SiteID==i)))))
        #pHmax <- -log10(as.numeric(min(na.omit(subset(c5df, select=params[9], c5df$SiteID==i)))))
        #pHmin <- -log10(as.numeric(max(na.omit(subset(c5df, select=params[9], c5df$SiteID==i)))))
        #c5means[j,11] <- (pHmax-pHmin)/2 # pH range
        
        c5means[j,9] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[9], c5df$SiteID==i)))) # pH
        c5means[j,10] <- p9n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[9], c5df$SiteID==i)))))
        c5means[j,11] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[9], c5df$SiteID==i)))))/sqrt(p9n)
        
        c5means[j,12] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[12], c5df$SiteID==i)))) # DICuM
        c5means[j,13] <- p12n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[12], c5df$SiteID==i)))))
        c5means[j,14] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[12], c5df$SiteID==i)))))/sqrt(p12n)
        
        c5means[j,15] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[15], c5df$SiteID==i))))  # CO2uM
        c5means[j,16] <- p15n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[15], c5df$SiteID==i)))))
        c5means[j,17] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[15], c5df$SiteID==i)))))/sqrt(p15n)
        
        c5means[j,18] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[18], c5df$SiteID==i)))) # HCO3uM
        c5means[j,19] <- p18n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[18], c5df$SiteID==i)))))
        c5means[j,20] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[18], c5df$SiteID==i)))))/sqrt(p18n)
        
        c5means[j,21] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[21], c5df$SiteID==i)))) # CauM
        c5means[j,22] <- p21n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[21], c5df$SiteID==i)))))
        c5means[j,23] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[21], c5df$SiteID==i)))))/sqrt(p21n)
        
        c5means[j,24] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[24], c5df$SiteID==i)))) # MguM
        c5means[j,25] <- p24n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[24], c5df$SiteID==i)))))
        c5means[j,26] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[24], c5df$SiteID==i)))))/sqrt(p24n)
        
        c5means[j,27] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[27], c5df$SiteID==i)))) # NauM
        c5means[j,28] <- p27n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[27], c5df$SiteID==i)))))
        c5means[j,29] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[27], c5df$SiteID==i)))))/sqrt(p27n)
        
        c5means[j,30] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[30], c5df$SiteID==i)))) # KuM
        c5means[j,31] <- p30n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[30], c5df$SiteID==i)))))
        c5means[j,32] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[30], c5df$SiteID==i)))))/sqrt(p30n)
        
        c5means[j,33] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[33], c5df$SiteID==i)))) # SO4uM
        c5means[j,34] <- p33n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[33], c5df$SiteID==i)))))
        c5means[j,35] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[33], c5df$SiteID==i)))))/sqrt(p33n)
        
        c5means[j,36] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[36], c5df$SiteID==i)))) # CluM
        c5means[j,37] <- p36n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[36], c5df$SiteID==i)))))
        c5means[j,38] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[36], c5df$SiteID==i)))))/sqrt(p36n)
        
        c5means[j,39] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[39], c5df$SiteID==i)))) # FeuM
        c5means[j,40] <- p39n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[39], c5df$SiteID==i)))))
        c5means[j,41] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[39], c5df$SiteID==i)))))/sqrt(p39n)
        
        c5means[j,42] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[42], c5df$SiteID==i)))) # AluM
        c5means[j,43] <- p42n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[42], c5df$SiteID==i)))))
        c5means[j,44] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[42], c5df$SiteID==i)))))/sqrt(p42n)
        
        c5means[j,45] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[45], c5df$SiteID==i)))) # SiuM
        c5means[j,46] <- p45n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[45], c5df$SiteID==i)))))
        c5means[j,47] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[45], c5df$SiteID==i)))))/sqrt(p45n)
        
        c5means[j,48] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[48], c5df$SiteID==i)))) # DOCuM
        c5means[j,49] <- p48n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[48], c5df$SiteID==i)))))
        c5means[j,50] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[48], c5df$SiteID==i)))))/sqrt(p48n)
        
        c5means[j,51] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[51], c5df$SiteID==i)))) # SUVA254
        c5means[j,52] <- p51n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[51], c5df$SiteID==i)))))
        c5means[j,53] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[51], c5df$SiteID==i)))))/sqrt(p51n)
        
        c5means[j,54] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[54], c5df$SiteID==i)))) # SR
        c5means[j,55] <- p54n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[54], c5df$SiteID==i)))))
        c5means[j,56] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[54], c5df$SiteID==i)))))/sqrt(p54n)
        
        c5means[j,57] <- mean(as.numeric(as.matrix(subset(c5df, select=params[57], c5df$SiteID==i))), na.rm=T) # DI13C;  where n = 0, will yield "NaN"
        c5means[j,58] <- p57n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[57], c5df$SiteID==i)))))
        DI13Cmin <- min(as.numeric(as.matrix(subset(c5df, select=params[57], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        DI13Cmax <- max(as.numeric(as.matrix(subset(c5df, select=params[57], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        c5means[j,59] <- (abs(DI13Cmin)-abs(DI13Cmax))/2 # DI13C range; where n = 0, will yield "NaN"
        
        c5means[j,60] <- mean(as.numeric(as.matrix(subset(c5df, select=params[60], c5df$SiteID==i))), na.rm=T) # DO13C;  where n = 0, will yield "NaN"
        c5means[j,61] <- p60n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[60], c5df$SiteID==i)))))
        DO13Cmin <- min(as.numeric(as.matrix(subset(c5df, select=params[60], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        DO13Cmax <- max(as.numeric(as.matrix(subset(c5df, select=params[60], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        c5means[j,62] <- (abs(DO13Cmin)-abs(DO13Cmax))/2 # DO13C range; where n = 0, will yield "NaN"
        
        c5means[j,63] <- mean(as.numeric(as.matrix(subset(c5df, select=params[63], c5df$SiteID==i))), na.rm=T) # d18O;  where n = 0, will yield "NaN"
        c5means[j,64] <- p63n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[63], c5df$SiteID==i)))))
        d18Omin <- min(as.numeric(as.matrix(subset(c5df, select=params[63], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        d18Omax <- max(as.numeric(as.matrix(subset(c5df, select=params[63], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        c5means[j,65] <- (abs(d18Omin)-abs(d18Omax))/2 # d18O range; where n = 0, will yield "NaN"
        
        c5means[j,66] <- mean(as.numeric(as.matrix(subset(c5df, select=params[66], c5df$SiteID==i))), na.rm=T) # d2H;  where n = 0, will yield "NaN"
        c5means[j,67] <- p66n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[66], c5df$SiteID==i)))))
        d2Hmin <- min(as.numeric(as.matrix(subset(c5df, select=params[66], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        d2Hmax <- max(as.numeric(as.matrix(subset(c5df, select=params[66], c5df$SiteID==i))), na.rm=T) # Where n = 0, will yield Warning re:"Inf"
        c5means[j,68] <- (abs(d2Hmin)-abs(d2Hmax))/2 # d2H range; where n = 0, will yield "NaN"
        
        c5means[j,69] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[69], c5df$SiteID==i)))) # dexcess
        c5means[j,70] <- p69n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[69], c5df$SiteID==i)))))
        c5means[j,71] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[69], c5df$SiteID==i)))))/sqrt(p69n)
        
        c5means[j,72] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[72], c5df$SiteID==i)))) # POCuM
        c5means[j,73] <- p72n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[72], c5df$SiteID==i)))))
        c5means[j,74] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[72], c5df$SiteID==i)))))/sqrt(p72n)
        
        c5means[j,75] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[75], c5df$SiteID==i)))) # pCO2
        c5means[j,76] <- p75n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[75], c5df$SiteID==i)))))
        c5means[j,77] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[75], c5df$SiteID==i)))))/sqrt(p75n)
        
        c5means[j,78] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[78], c5df$SiteID==i)))) # pCH4
        c5means[j,79] <- p78n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[78], c5df$SiteID==i)))))
        c5means[j,80] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[78], c5df$SiteID==i)))))/sqrt(p78n)
        
        c5means[j,81] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[81], c5df$SiteID==i)))) # jCO2
        c5means[j,82] <- p81n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[81], c5df$SiteID==i)))))
        c5means[j,83] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[81], c5df$SiteID==i)))))/sqrt(p81n)
        
        c5means[j,84] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[84], c5df$SiteID==i)))) # kCO2
        c5means[j,85] <- p84n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[84], c5df$SiteID==i)))))
        c5means[j,86] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[84], c5df$SiteID==i)))))/sqrt(p84n)
        
        c5means[j,87] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[87], c5df$SiteID==i)))) # jCH4
        c5means[j,88] <- p87n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[87], c5df$SiteID==i)))))
        c5means[j,89] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[87], c5df$SiteID==i)))))/sqrt(p87n)
        
        c5means[j,90] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[90], c5df$SiteID==i)))) # kCH4
        c5means[j,91] <- p90n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[90], c5df$SiteID==i)))))
        c5means[j,92] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[90], c5df$SiteID==i)))))/sqrt(p90n)
        
        c5means[j,93] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[93], c5df$SiteID==i)))) # Q
        c5means[j,94] <- p93n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[93], c5df$SiteID==i)))))
        c5means[j,95] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[93], c5df$SiteID==i)))))/sqrt(p93n)
        
        c5means[j,96] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[96], c5df$SiteID==i)))) # CO2flux
        c5means[j,97] <- p96n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[96], c5df$SiteID==i))))) 
        c5means[j,98] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[96], c5df$SiteID==i)))))/sqrt(p96n) # std error
        
        c5means[j,99] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[99], c5df$SiteID==i)))) # CH4flux
        c5means[j,100] <- p99n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[99], c5df$SiteID==i))))) 
        c5means[j,101] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[99], c5df$SiteID==i)))))/sqrt(p99n) # std error
        
        c5means[j,102] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[102], c5df$SiteID==i)))) # HCO3flux
        c5means[j,103] <- p102n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[102], c5df$SiteID==i))))) 
        c5means[j,104] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[102], c5df$SiteID==i)))))/sqrt(p102n) # std error
        
        c5means[j,105] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[105], c5df$SiteID==i)))) # DOCflux
        c5means[j,106] <- p105n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[105], c5df$SiteID==i))))) 
        c5means[j,107] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[105], c5df$SiteID==i)))))/sqrt(p105n) # std error
        
        c5means[j,108] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[108], c5df$SiteID==i)))) # DICflux
        c5means[j,109] <- p108n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[108], c5df$SiteID==i))))) 
        c5means[j,110] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[108], c5df$SiteID==i)))))/sqrt(p108n) # std error
        
        c5means[j,111] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[111], c5df$SiteID==i)))) # POCflux
        c5means[j,112] <- p111n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[111], c5df$SiteID==i))))) 
        c5means[j,113] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[111], c5df$SiteID==i)))))/sqrt(p111n) # std error
        
        c5means[j,114] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[114], c5df$SiteID==i)))) # PICflux
        c5means[j,115] <- p114n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[114], c5df$SiteID==i))))) 
        c5means[j,116] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[114], c5df$SiteID==i)))))/sqrt(p114n) # std error
        
        c5means[j,117] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[117], c5df$SiteID==i)))) # TCflux
        c5means[j,118] <- p117n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[117], c5df$SiteID==i))))) 
        c5means[j,119] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[117], c5df$SiteID==i)))))/sqrt(p117n) # std error
        
        c5means[j,120] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[120], c5df$SiteID==i)))) # CO2yld
        c5means[j,121] <- p120n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[120], c5df$SiteID==i)))))
        c5means[j,122] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[120], c5df$SiteID==i)))))/sqrt(p120n)
        
        c5means[j,123] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[123], c5df$SiteID==i)))) # CH4yld
        c5means[j,124] <- p123n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[123], c5df$SiteID==i)))))
        c5means[j,125] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[123], c5df$SiteID==i)))))/sqrt(p123n)
        
        c5means[j,126] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[126], c5df$SiteID==i)))) # HCO3yld
        c5means[j,127] <- p126n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[126], c5df$SiteID==i)))))
        c5means[j,128] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[126], c5df$SiteID==i)))))/sqrt(p126n)
        
        c5means[j,129] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[129], c5df$SiteID==i)))) # PICyld
        c5means[j,130] <- p129n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[129], c5df$SiteID==i)))))
        c5means[j,131] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[129], c5df$SiteID==i)))))/sqrt(p129n)
        
        c5means[j,132] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[132], c5df$SiteID==i)))) # DOCyld
        c5means[j,133] <- p132n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[132], c5df$SiteID==i)))))
        c5means[j,134] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[132], c5df$SiteID==i)))))/sqrt(p132n)
        
        c5means[j,135] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[135], c5df$SiteID==i)))) # POCyld
        c5means[j,136] <- p135n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[135], c5df$SiteID==i)))))
        c5means[j,137] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[135], c5df$SiteID==i)))))/sqrt(p135n)
        
        c5means[j,138] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[138], c5df$SiteID==i)))) # DINuM
        c5means[j,139] <- p138n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[138], c5df$SiteID==i)))))
        c5means[j,140] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[138], c5df$SiteID==i)))))/sqrt(p138n)
        
        c5means[j,141] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[141], c5df$SiteID==i)))) # DONuM
        c5means[j,142] <- p141n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[141], c5df$SiteID==i)))))
        c5means[j,143] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[141], c5df$SiteID==i)))))/sqrt(p141n)
        
        c5means[j,144] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[144], c5df$SiteID==i)))) # TDNuM
        c5means[j,145] <- p144n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[144], c5df$SiteID==i)))))
        c5means[j,146] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[144], c5df$SiteID==i)))))/sqrt(p144n)
        
        c5means[j,147] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[147], c5df$SiteID==i)))) # TSS
        c5means[j,148] <- p147n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[147], c5df$SiteID==i)))))
        c5means[j,149] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[147], c5df$SiteID==i)))))/sqrt(p147n)
        
        c5means[j,150] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[150], c5df$SiteID==i)))) # V
        c5means[j,151] <- p150n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[150], c5df$SiteID==i)))))
        c5means[j,152] <- sd(as.numeric(as.matrix(na.omit(subset(c5df, select=params[150], c5df$SiteID==i)))))/sqrt(p150n)
        
        c5means[j,153] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[153], c5df$SiteID==i)))) # MeanEVI
        c5means[j,154] <- p153n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[153], c5df$SiteID==i)))))
        c5means[j,155] <- sd(as.numeric(as.matrix(subset(c5df, select=params[153], c5df$SiteID==i), na.rm=T)))/sqrt(p153n)
        
        c5means[j,156] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[156], c5df$SiteID==i)))) # MeanGPP
        c5means[j,157] <- p156n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[156], c5df$SiteID==i)))))
        c5means[j,158] <- sd(as.numeric(as.matrix(subset(c5df, select=params[156], c5df$SiteID==i), na.rm=T)))/sqrt(p156n)
        
        c5means[j,159] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[159], c5df$SiteID==i)))) # Runoff
        c5means[j,160] <- p159n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[159], c5df$SiteID==i)))))
        c5means[j,161] <- sd(as.numeric(as.matrix(subset(c5df, select=params[159], c5df$SiteID==i), na.rm=T)))/sqrt(p159n)
        
        c5means[j,162] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[162], c5df$SiteID==i)))) # DOmgL
        c5means[j,163] <- p162n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[162], c5df$SiteID==i)))))
        c5means[j,164] <- sd(as.numeric(as.matrix(subset(c5df, select=params[162], c5df$SiteID==i), na.rm=T)))/sqrt(p162n)
        
        c5means[j,165] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[165], c5df$SiteID==i)))) # TCuM
        c5means[j,166] <- p165n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[165], c5df$SiteID==i)))))
        c5means[j,167] <- sd(as.numeric(as.matrix(subset(c5df, select=params[165], c5df$SiteID==i), na.rm=T)))/sqrt(p165n)
        
        c5means[j,168] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[168], c5df$SiteID==i)))) # TCyld
        c5means[j,169] <- p168n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[168], c5df$SiteID==i)))))
        c5means[j,170] <- sd(as.numeric(as.matrix(subset(c5df, select=params[168], c5df$SiteID==i), na.rm=T)))/sqrt(p168n)
        
        c5means[j,171] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[171], c5df$SiteID==i)))) # PICuM
        c5means[j,172] <- p171n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[171], c5df$SiteID==i)))))
        c5means[j,173] <- sd(as.numeric(as.matrix(subset(c5df, select=params[171], c5df$SiteID==i), na.rm=T)))/sqrt(p171n)
        
        c5means[j,174] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[174], c5df$SiteID==i)))) # DICyld
        c5means[j,175] <- p174n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[174], c5df$SiteID==i)))))
        c5means[j,176] <- sd(as.numeric(as.matrix(subset(c5df, select=params[174], c5df$SiteID==i), na.rm=T)))/sqrt(p174n)
        
        c5means[j,177] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[177], c5df$SiteID==i)))) # W
        c5means[j,178] <- p177n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[177], c5df$SiteID==i)))))
        c5means[j,179] <- sd(as.numeric(as.matrix(subset(c5df, select=params[177], c5df$SiteID==i), na.rm=T)))/sqrt(p177n)
        
        c5means[j,180] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[180], c5df$SiteID==i)))) # Runoff
        c5means[j,181] <- p180n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[180], c5df$SiteID==i)))))
        c5means[j,182] <- sd(as.numeric(as.matrix(subset(c5df, select=params[180], c5df$SiteID==i), na.rm=T)))/sqrt(p180n)
        
        c5means[j,183] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[183], c5df$SiteID==i)))) # TDNyld
        c5means[j,184] <- p183n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[183], c5df$SiteID==i)))))
        c5means[j,185] <- sd(as.numeric(as.matrix(subset(c5df, select=params[183], c5df$SiteID==i), na.rm=T)))/sqrt(p183n)
        
        c5means[j,186] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[186], c5df$SiteID==i)))) # DICyld
        c5means[j,187] <- p186n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[186], c5df$SiteID==i)))))
        c5means[j,188] <- sd(as.numeric(as.matrix(subset(c5df, select=params[186], c5df$SiteID==i), na.rm=T)))/sqrt(p186n)
        
        c5means[j,189] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[189], c5df$SiteID==i)))) # NH4uM
        c5means[j,190] <- p189n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[189], c5df$SiteID==i)))))
        c5means[j,191] <- sd(as.numeric(as.matrix(subset(c5df, select=params[189], c5df$SiteID==i), na.rm=T)))/sqrt(p189n)
        
        c5means[j,192] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[192], c5df$SiteID==i)))) # NO3uM
        c5means[j,193] <- p192n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[192], c5df$SiteID==i)))))
        c5means[j,194] <- sd(as.numeric(as.matrix(subset(c5df, select=params[192], c5df$SiteID==i), na.rm=T)))/sqrt(p192n)
        
        c5means[j,195] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[195], c5df$SiteID==i)))) # DOpcnt
        c5means[j,196] <- p195n <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[195], c5df$SiteID==i)))))
        c5means[j,197] <- sd(as.numeric(as.matrix(subset(c5df, select=params[195], c5df$SiteID==i), na.rm=T)))/sqrt(p195n)
        
        c5means[j,198] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[198], c5df$SiteID==i)))) # SOCC30cm
        c5means[j,199] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[199], c5df$SiteID==i)))) # SOCC100cm
        c5means[j,200] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[200], c5df$SiteID==i)))) # ColluvialPcnt
        c5means[j,201] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[201], c5df$SiteID==i)))) # CarbonatePcnt
        c5means[j,202] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[202], c5df$SiteID==i)))) # MeanEl
        c5means[j,203] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[203], c5df$SiteID==i)))) # MeanSlope
        c5means[j,204] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[204], c5df$SiteID==i)))) # LakesPondpcnt
        c5means[j,205] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[205], c5df$SiteID==i)))) # ShedAream2
        c5means[j,206] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[206], c5df$SiteID==i)))) # Strahler
        c5means[j,207] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[207], c5df$SiteID==i)))) # FluvialPcnt
        c5means[j,208] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[208], c5df$SiteID==i)))) # MorainePcnt
        c5means[j,209] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[209], c5df$SiteID==i)))) # OrganicPcnt
        c5means[j,210] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[210], c5df$SiteID==i)))) # SilicatePcnt
        #c5means[j,158] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[158], c5df$SiteID==i)))) # JulianDay
        
        # Template:
        #c5means[j,] <- as.numeric(colMeans(na.omit(subset(c5df, select=params[XX], c5df$SiteID==i)))) # PARAMETER
        #c5means[j,] <- pXXn <- length(as.numeric(as.matrix(na.omit(subset(c5df, select=params[XX], c5df$SiteID==i)))))
        #c5means[j,] <- sd(as.numeric(as.matrix(subset(c5df, select=params[XX], c5df$SiteID==i), na.rm=T)))/sqrt(pXXn)
        
        j <- j+1
      }
      c5means$Loc <- factor(c5means$Loc, levels=c("BB","PP","PAP","AP"))
      
    # Optional: Export
      #write.csv(c5means, "/Users/szolkos/Desktop/c5means.csv", row.names=F)
  