#========================================================================================================#
# 11_necb.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2018-03-17
# Background: Net Ecosystem Carbon Balance (NECB) calculations- mean for each site and location
#========================================================================================================#

  # Build NECB dataframe; Units: jCO2 (µmol/m2/s), jCH4 (nmol/m2/s), DIC-CO2-HCO3-PIC-DOC-POC yld (µmol/m2/d), NEEmean (gC/m2/d)
    c5means$SiteID <- paste0(c5means$Loc,"0",c5means$Site)
    c5means_sub <- drop.levels(subset(c5means, select=c("SiteID","Loc","Site","Q","ShedAream2","jCO2","jCH4","DICyld","CO2yld","HCO3yld","PICyld","DOCyld","POCyld","TCyld")))
    c5nee_df_sub <- drop.levels(subset(c5nee_df, select=c("SiteID","NEEmean","NEEsd")))
    
  # Merge NEE data with 'c5means'
    necb_df <- merge(x=c5means_sub, y=c5nee_df_sub, by="SiteID", all.x=T, all.y=T, sort=T) 
    necb_df$Loc <- factor(necb_df$Loc, levels=c("BB","PP","PAP","AP"))
    necb_df$SiteID <- factor(necb_df$SiteID)
    necb_df$PICyld[necb_df$PICyld=="NaN"] <- NA
    necb_df$POCyld[necb_df$POCyld=="NaN"] <- NA
    
  # Merge
    j_df2 <- j_df[,!names(j_df) %in% c("SSAtot_mean","SSAtot_min","SSAtot_max")]
    aquatic_nee <- merge(x=j_df2, y=lc_df, by="LocSite")
    #aquatic_nee <- drop.levels(subset(lc_df, select=c("SiteID","LakesPondpcnt","SSAtot","propTerrArea","propAquaticArea","propStreamsArea","jCO2tot_mean","jCO2tot_min","jCO2tot_max","jCH4tot_mean","jCH4tot_min","jCH4tot_max")))
    aquatic_nee <- drop.levels(subset(aquatic_nee, select=c("SiteID","jCO2tot_mean","jCO2tot_min","jCO2tot_max","jCH4tot_mean","jCH4tot_min","jCH4tot_max","propTerrArea_mean","propAquaticArea_mean","propStreamsArea_mean","propTerrArea_min","propAquaticArea_min","propStreamsArea_min","propTerrArea_max","propAquaticArea_max","propStreamsArea_max")))
    necb_df <- merge(x=necb_df, y=aquatic_nee, by="SiteID", all.x=T, all.y=T, sort=T)
    
  # Lateral C export: convert all from µmolC/m2/d to gC/m2/d
    necb_df$DICgCm2d <- (necb_df$DICyld/1000000)*12.0107
    necb_df$CO2gCm2d <- (necb_df$CO2yld/1000000)*12.0107
    necb_df$HCO3gCm2d <- (necb_df$HCO3yld/1000000)*12.0107
    necb_df$PICgCm2d <- (necb_df$PICyld/1000000)*12.0107
    necb_df$DOCgCm2d <- (necb_df$DOCyld/1000000)*12.0107
    necb_df$POCgCm2d <- (necb_df$POCyld/1000000)*12.0107
    necb_df$TCgCm2d <- (necb_df$TCyld/1000000)*12.0107
  
  # Daily C flux (kgC/d)
    # Calculate
      necb_df$jCO2kgCd_mean <- necb_df$jCO2tot_mean # Previously calculated as kgC/d; just store as new variable
      necb_df$jCO2kgCd_min <- necb_df$jCO2tot_min
      necb_df$jCO2kgCd_max <- necb_df$jCO2tot_max
      necb_df$jCH4kgCd_mean <- necb_df$jCH4tot_mean # Previously calculated kgC/d; just store as new variable'
      necb_df$jCH4kgCd_min <- necb_df$jCH4tot_min
      necb_df$jCH4kgCd_max <- necb_df$jCH4tot_max
      necb_df$DICkgCd <- (necb_df$DICgCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
      necb_df$CO2kgCd <- (necb_df$CO2gCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
      necb_df$HCO3kgCd <- (necb_df$HCO3gCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
      necb_df$PICkgCd <- (necb_df$PICgCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
      necb_df$DOCkgCd <- (necb_df$DOCgCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
      necb_df$POCkgCd <- (necb_df$POCgCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
      necb_df$TCkgCd <- (necb_df$TCgCm2d*necb_df$ShedAream2)/1000 # * tot. watershed area / 1000 to get kgC/d
    #  Calculate mean, min, and max NEE, given terrestrial surface area changes wrt mean, min, and max stream surface area
      necb_df$NEEmeankgCd_mean <- (necb_df$NEEmean*necb_df$ShedAream2*necb_df$propTerrArea_mean)/1000 # * terrestrial area (i.e. tot. watershed area - aquatic surface area) / 1000 to get kgC/d
      necb_df$NEEmeankgCd_min <- (necb_df$NEEmean*necb_df$ShedAream2*necb_df$propTerrArea_min)/1000
      necb_df$NEEmeankgCd_max <- (necb_df$NEEmean*necb_df$ShedAream2*necb_df$propTerrArea_max)/1000
    #  Calculate mean, min, and max of the Aquatic flux component, given variability wrt mean, min, and max jCO2 and jCH4
      necb_df$AQkgCd_mean <- necb_df$jCO2kgCd_mean + necb_df$jCH4kgCd_mean + necb_df$TCkgCd
      necb_df$AQkgCd_min <- necb_df$jCO2kgCd_min + necb_df$jCH4kgCd_min + necb_df$TCkgCd
      necb_df$AQkgCd_max <- necb_df$jCO2kgCd_max + necb_df$jCH4kgCd_max + necb_df$TCkgCd
    #  Calculate mean, min, and max of the Aquatic flux component, accounting for variability in terrestrial surface area and also variability in jCO2 and jCH4
      necb_df$PropAQkgCd_mean <- necb_df$AQkgCd_mean/(necb_df$NEEmeankgCd_mean*-1)
      necb_df$PropAQkgCd_min <- necb_df$AQkgCd_min/(necb_df$NEEmeankgCd_min*-1)
      necb_df$PropAQkgCd_max <- necb_df$AQkgCd_max/(necb_df$NEEmeankgCd_max*-1)
    # Change NaN to NA
      necb_df[necb_df=="NaN"] <- NA
    # Export data
      #write.csv(necb_df, "/Users/szolkos/Desktop/necb_df.csv", row.names=F)
    
  # Plot
    ggplot(necb_df, aes(x=necb_df$Loc, y=PropAQkgCd_mean, fill=Loc)) + 
      geom_boxplot() +
      # Set axis limits
        scale_fill_manual(name="Source", values=regional_colors) +
        scale_y_log10(limits=c(0.01,1), breaks=c(0.01,0.02,0.05,0.1,0.2,0.5,1)) +
        scale_x_discrete(labels=levels(necb_df$Loc)) +
      # Set panel theme
        theme_bw() +
        theme(plot.margin=unit(c(0.1,0.1,0,0.2), "in"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              plot.background=element_rect(colour="white", size=1),
              panel.border=element_rect(colour="black", fill=NA, size=1),
              text=element_text(size=13)) +
        theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
        theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
        theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
        theme(legend.position="none") +
        theme(plot.background=element_rect(fill='white')) +
        theme(panel.background=element_rect(fill='white')) +
      # Add data points
        geom_point(data=necb_df, aes(x=necb_df$Loc, y=PropAQkgCd_mean), shape=21, size=5, stroke=1) +
      # Labels
        labs(y=expression("C balance (Fluvial:Terrestrial C flux)"), x="")
        #labs(tag="a", text=element_text(size=20))
    
    
  # OTHER ####
    
    # EACH LOC/SITE SAMPLE
      necb_df1 <- drop.levels(subset(c5df, select=c("Date","Loc","Site","SiteID","Q","ShedAream2","CO2uM","CH4H2OuM","DICuM","HCO3uM","PICuM","DOCuM","POCuM")))
      necb_df1$CO2flux <- (necb_df1$CO2uM/1000000)*12.0107/1000*86400
      necb_df1$CH4flux <- (necb_df1$CH4H2OuM/1000000)*12.0107/1000*86400
      necb_df1$DICflux <- (necb_df1$DICuM/1000000)*12.0107/1000*86400
      necb_df1$HCO3flux <- (necb_df1$HCO3uM/1000000)*12.0107/1000*86400
      necb_df1$PICflux <- (necb_df1$PICuM/1000000)*12.0107/1000*86400
      necb_df1$DOCflux <- (necb_df1$DOCuM/1000000)*12.0107/1000*86400
      necb_df1$POCflux <- (necb_df1$POCuM/1000000)*12.0107/1000*86400
        
    # For every sampling day: Total lateral fluvial C transport (DIC, DOC, PIC, POC) & NEE (mean, min, max)
  
      # Calculate daily ecosystem C balance for each watershed
        c5df_ecb_pre <- drop.levels(subset(c5df, select=c("Date","Loc","Site","jCO2","jCH4","DICuM","PICuM","DOCuM","POCuM","Q"))) # "CO2uM","HCO3uM","CO3uM",
        
      # Calculate lateral fluxes in kgCd- convert µmolC/L to kgC/L, multiply by Q (L/s)
        #c5df_ecb_pre$CO2kgCd <- (((c5df_ecb_pre$CO2uM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        #c5df_ecb_pre$HCO3kgCd <- (((c5df_ecb_pre$HCO3uM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        #c5df_ecb_pre$CO3kgCd <- (((c5df_ecb_pre$CO3uM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        c5df_ecb_pre$DICkgCd <- (((c5df_ecb_pre$DICuM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        c5df_ecb_pre$DOCkgCd <- (((c5df_ecb_pre$DOCuM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        c5df_ecb_pre$PICkgCd <- (((c5df_ecb_pre$PICuM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        c5df_ecb_pre$POCkgCd <- (((c5df_ecb_pre$POCuM/1000000)*12.0107)/1000)*(c5df_ecb_pre$Q*1000)*86400
        c5df_ecb_pre <- drop.levels(subset(c5df_ecb_pre, select=c("Date","Loc","Site","DICkgCd","DOCkgCd","PICkgCd","POCkgCd"))) # "CO2kgCd","HCO3kgCd","CO3kgCd",
        c5df_ecb_pre$LocSite <- paste0(c5df_ecb_pre$Loc,"0",c5df_ecb_pre$Site)
        c5df_ecb_pre$LocSite <- factor(c5df_ecb_pre$LocSite)
      
      # Merge lateral fluxes with vertical fluxes (df 'j_df', created above)
        #j_df_sub <- drop.levels(subset(j_df, select=c("LocSite","jCO2tot_mean","jCH4tot_mean","jCO2tot_min","jCH4tot_min","jCO2tot_max","jCH4tot_max")))
        #c5df_ecb_pre <- merge(x=c5df_ecb_pre, y=j_df_sub, by=c("Date","Loc","Site"), all.x=T, sort=T)
        #c5df_ecb_pre <- merge(x=c5df_ecb_pre, y=j_df_sub, by=c("LocSite"), all.x=T, sort=T)
        
      # Subset daily SPL4CMDL NEE estimates
        smap_sub <- drop.levels(subset(smap, select=c("Date","Loc","Site","NEEmean")))
        smap_sub$Loc <- factor(smap_sub$Loc); smap_sub$Site <- factor(smap_sub$Site)
        
      # Calculate daily NEE (kgC/d)
        lc_df_sub <- droplevels(subset(lc_df, select=c("Loc","Site","ShedAream2","propTerrArea_mean","propTerrArea_min","propTerrArea_max")))
        NEE_ecb <- merge(x=lc_df_sub, y=smap_sub, by=c("Loc","Site"), all.x=T, sort=T) 
        NEE_ecb$NEEkgCdmean <- ((NEE_ecb$NEEmean/1000)*(NEE_ecb$ShedAream2*NEE_ecb$propTerrArea_mean)) # Mean NEE kgCd
        NEE_ecb$NEEkgCdmin <- ((NEE_ecb$NEEmean/1000)*(NEE_ecb$ShedAream2*NEE_ecb$propTerrArea_min)) # Min NEE kgCd
        NEE_ecb$NEEkgCdmax <- ((NEE_ecb$NEEmean/1000)*(NEE_ecb$ShedAream2*NEE_ecb$propTerrArea_max)) # Max NEE kgCd
        NEE_ecb <- drop.levels(subset(NEE_ecb, select=c("Date","Loc","Site","NEEkgCdmean","NEEkgCdmin","NEEkgCdmax")))
        
      # Merge daily smap NEE estimates with daily fluxes
        c5df_ecb <- merge(x=c5df_ecb_pre, y=NEE_ecb, by=c("Date","Loc","Site"), all.x=T, sort=T)
        c5df_ecb$LocSite <- NULL
      # Optional: Export data
        #write.csv(c5df_ecb, "/Users/szolkos/Desktop/c5df_ecb.csv", row.names=F)
    