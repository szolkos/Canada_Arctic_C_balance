#========================================================================================================#
# 10_C_gas_upscale.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Estimate mean and range of daily CO2 and CH4 efflux from fluvial networks
#========================================================================================================#

  # BACKGROUND ####

    # Parameters and units ####
      # Total CO2 efflux: kgC/d
      # Total CH4 efflux: kgC/d
    
  # DATA PROCESSING AND ANALYSIS ####

    # Upscaling workflow:
      # 1) Calculate Total Stream Length (TSL) by region (Loc), watershed (Site), and stream order (Strahler)
        ## Because there are TSL estimates (but not efflux mmnts) for each Loc, Site, and Strahler, you will use efflux
        ## measurements from other sites of the same stream order in the same study region to estimate efflux 
        ## for each Loc and Site (for comparison with NEE, to make  NECB estimates for each watershed)
      # 2) Calculate mean, min, and max stream width (msw) for each Strahler order in each region ('Loc')
      # 3) Calculate mean, min, and max jCO2 and jCH4 for each Strahler order in each region ('Loc')
      # 4) Calculate total CO2 and CH4 efflux for each watershed (kgC/d)
        
      # 1) Total Stream Length (TSL) by region (Loc), watershed (Site), and stream order (Strahler)
          strms <- read.csv(paste0(dir,"Ch5/","Ch5_StrmGeom.csv"), header=T) # 'strms' contains the TSL for each stream order (multiple reaches) in each watershed ('WtrshdID')
        # For each watershed, calculate TSL by Loc, Site, Strahler
          strms$LocStrahl <- paste0(strms$Loc,"-",strms$Strahler)
          strms$LocSiteStrahl <- paste0(strms$Loc,"-",strms$Site,"-",strms$Strahler) # Add unique ID for 'apply' function
          strms_l <- round(as.data.frame(tapply(strms$StrmLength, strms$LocSiteStrahl, FUN=sum)),5)
        # Create dataframe of TSL by Loc, Site, and Strahler
          tsl_df <- as.data.frame(matrix(nrow=nrow(strms_l), ncol=5)); names(tsl_df) <- c("Loc","Site","Strahler","LocStrahl","TSL")
          tsl_df[,1] <- substr(row.names(strms_l), 1, 2)
          tsl_df[,2] <- substr(row.names(strms_l), 4, 4)
          tsl_df[,3] <- substr(row.names(strms_l), 6, 6)
          tsl_df[,4] <- paste0(substr(row.names(strms_l), 1, 2),"-",substr(row.names(strms_l), 6, 6))
          tsl_df[,5] <- strms_l
        # Summarize dataframe
          summary(tsl_df); dim(tsl_df)
          tsl_df$Loc <- factor(tsl_df$Loc, levels=c("BB","PP","PA","AP"))
          tsl_df$Site <- as.numeric(tsl_df$Site)
          tsl_df$Strahler <- factor(tsl_df$Strahler)
          tsl_df$LocStrahl <- factor(tsl_df$LocStrahl)
        
      # 2) Mean Stream Width (MSW) by Loc (i.e. watershed) and Strahler
          strms_w <- drop.levels(subset(c5df, select=c("Date","Loc","Site","Strahler","W")))
        # Convert "PAP" to "PA" by converting from factor to character, replacing, then converting back to factor
          strms_w$Loc <- as.character(strms_w$Loc); strms_w[strms_w=="PAP"] <- "PA"; strms_w$Loc <- factor(strms_w$Loc)
          strms_w$LocStrahl <- paste0(strms_w$Loc,"-",strms_w$Strahler)
        # Calculate MSW for each Strahler order for each Loc, use to create 'msw_df' next
          meanW <- round(as.data.frame(tapply(strms_w$W, strms_w$LocStrahl, FUN=mean)),4)
          minW <- round(as.data.frame(tapply(strms_w$W, strms_w$LocStrahl, FUN=min)),4)
          maxW <- round(as.data.frame(tapply(strms_w$W, strms_w$LocStrahl, FUN=max)),4)
          MSW <- cbind(meanW,minW,maxW)
          names(MSW) <- c("meanW","minW","maxW")
        # Create dataframe of mean/min/max stream width ('msw') by Loc (i.e. watershed) and Strahler
          msw_df <- as.data.frame(matrix(nrow=nrow(MSW), ncol=6))
          names(msw_df) <- c("Loc","Strahler","LocStrahl","meanW","minW","maxW")
          msw_df[,1] <- substr(row.names(MSW), 1, 2)
          msw_df[,2] <- substr(row.names(MSW), 4, 4)
          msw_df[,3] <- paste0(substr(row.names(MSW), 1, 2),"-",substr(row.names(MSW), 4, 4))
          msw_df[,4] <- MSW$meanW
          msw_df[,5] <- MSW$minW
          msw_df[,6] <- MSW$maxW
        # Summarize dataframe
          summary(msw_df); dim(msw_df)
          msw_df$Loc <- factor(msw_df$Loc, levels=c("BB","PP","PA","AP"))
          msw_df$Strahler <- factor(msw_df$Strahler)
          msw_df$LocStrahl <- factor(msw_df$LocStrahl)
        
      # 3) Calculate mean, min, and max jCO2 and jCH4 for each Loc and Site, merge  with 'ssa_df'
        # Create dataframe with mean efflux for each site
          streams_j <- drop.levels(na.omit(subset(c5df, select=c("Loc","Site","ShedAream2","Strahler","W","jCO2","jCH4"))))
          streams_j1 <- drop.levels(na.omit(subset(c5df, select=c("Loc","Site","ShedAream2","Strahler","W","jCO2","jCH4"), c5df$JulianDay<198))) # 1st sampling campaign
          streams_j2 <- drop.levels(na.omit(subset(c5df, select=c("Loc","Site","ShedAream2","Strahler","W","jCO2","jCH4"), c5df$JulianDay>214))) # 2nd sampling campaign
        ## 'streams_j' to be used for mean of early and late season NECB
        ## If desired to analyze NECB for early or late season, set 'stream_j' in next line as 'streams_j1' or 'streams_j2', respectively
          streams_j <- streams_j # 'streams_j1' = first synoptic campaign, 'streams_j2' = second
          streams_j$Loc <- as.character(streams_j$Loc); streams_j[streams_j=="PAP"] <- "PA"; streams_j$Loc <- factor(streams_j$Loc)
          streams_j$LocStrahl <- paste0(streams_j$Loc,"-",streams_j$Strahler)
        # Calculate mean jCO2 and and jCH4 for each stream order, in each region
          jCO2mean <- round(as.data.frame(tapply(streams_j$jCO2, streams_j$LocStrahl, FUN=mean)),4)
          jCO2min <- round(as.data.frame(tapply(streams_j$jCO2, streams_j$LocStrahl, FUN=min)),4)
          jCO2max <- round(as.data.frame(tapply(streams_j$jCO2, streams_j$LocStrahl, FUN=max)),4)
          jCH4mean <- round(as.data.frame(tapply(streams_j$jCH4, streams_j$LocStrahl, FUN=mean)),4)
          jCH4min <- round(as.data.frame(tapply(streams_j$jCH4, streams_j$LocStrahl, FUN=min)),4)
          jCH4max <- round(as.data.frame(tapply(streams_j$jCH4, streams_j$LocStrahl, FUN=max)),4)
          streams_j_vals <- cbind(jCO2mean,jCO2min,jCO2max,jCH4mean,jCH4min,jCH4max) # Bind into one df
          names(streams_j_vals) <- c("jCO2mean","jCO2min","jCO2max","jCH4mean","jCH4min","jCH4max")
        # Add site, etc. info to df
          streams_j_df <- as.data.frame(matrix(nrow=nrow(streams_j_vals), ncol=9))
          names(streams_j_df) <- c("Loc","Strahler","LocStrahl","jCO2mean","jCO2min","jCO2max","jCH4mean","jCH4min","jCH4max")
          streams_j_df[,1] <- substr(row.names(streams_j_vals), 1, 2)
          streams_j_df[,2] <- substr(row.names(streams_j_vals), 4, 4)
          streams_j_df[,3] <- paste0(substr(row.names(MSW), 1, 2),"-",substr(row.names(MSW), 4, 4))
          streams_j_df[,4] <- streams_j_vals$jCO2mean
          streams_j_df[,5] <- streams_j_vals$jCO2min
          streams_j_df[,6] <- streams_j_vals$jCO2max
          streams_j_df[,7] <- streams_j_vals$jCH4mean
          streams_j_df[,8] <- streams_j_vals$jCH4min
          streams_j_df[,9] <- streams_j_vals$jCH4max
        
      # 4) Calculate total CO2 and CH4 efflux for each watershed (kgC/d)
        ## Merge MSW ('msw_df') to 'strms'; 
        ## merged df will contain data for each stream reach from ArcGIS;
        ## using this, calculate SSA for each reach
          preflux_df <- merge(x=strms, y=subset(msw_df, select=c("LocStrahl","meanW","minW","maxW")), by="LocStrahl", all.x=T, all.y=T, sort=T)
          preflux_df$meanSSA <- preflux_df$StrmLength*preflux_df$meanW
          preflux_df$minSSA <- preflux_df$StrmLength*preflux_df$minW
          preflux_df$maxSSA <- preflux_df$StrmLength*preflux_df$maxW
        # Merge streams_j_df (i.e. mean efflux rates for each stream order in each watershed)
          flux_df <- merge(x=preflux_df, y=subset(streams_j_df, select=c("LocStrahl","jCO2mean","jCO2min","jCO2max","jCH4mean","jCH4min","jCH4max")), by="LocStrahl", all.x=T, all.y=T, sort=T)
          flux_df <- drop.levels(subset(flux_df, select=c("LocStrahl","WtrshdID","Loc","Site","Strahler","LocSiteStrahl","StrmLength","meanW","minW","maxW","meanSSA","minSSA","maxSSA","jCO2mean","jCO2min","jCO2max","jCH4mean","jCH4min","jCH4max")))
        # Calculate daily CO2 and CH4 efflux (kgC/d) for each stream reach
          ## MEAN
            flux_df$jCO2kgCdlyMean <- ((flux_df$meanSSA*flux_df$jCO2mean)/1000000)*12.0107*86400/1000
            flux_df$jCH4kgCdlyMean <- ((flux_df$meanSSA*flux_df$jCH4mean)/100000000)*12.0107*86400/1000
          ## MINIMUM
            flux_df$jCO2kgCdlyMin <- ((flux_df$minSSA*flux_df$jCO2min)/1000000)*12.0107*86400/1000
            flux_df$jCH4kgCdlyMin <- ((flux_df$minSSA*flux_df$jCH4min)/100000000)*12.0107*86400/1000
          ## MAX
            flux_df$jCO2kgCdlyMax <- ((flux_df$maxSSA*flux_df$jCO2max)/1000000)*12.0107*86400/1000
            flux_df$jCH4kgCdlyMax <- ((flux_df$maxSSA*flux_df$jCH4max)/100000000)*12.0107*86400/1000
        # Calculate daily total efflux (kgC/d) for each watershed (by WtrshdID) as sum of MEAN, sum of MIN, and sum of MAX
          ## MEAN
            tot_ssa_mean <- (round(as.data.frame(tapply(flux_df$meanSSA, flux_df$WtrshdID, FUN=sum)),4))
            tot_jco2_mean <- (round(as.data.frame(tapply(flux_df$jCO2kgCdlyMean, flux_df$WtrshdID, FUN=sum)),4))
            tot_jch4_mean <- (round(as.data.frame(tapply(flux_df$jCH4kgCdlyMean, flux_df$WtrshdID, FUN=sum)),4))
            j_df_mean <- as.data.frame(matrix(nrow=nrow(tot_jco2_mean), ncol=4))
            names(j_df_mean) <- c("LocSite","SSAtot_mean","jCO2tot_mean","jCH4tot_mean")
            j_df_mean[,1] <- row.names(tot_jco2_mean)
            j_df_mean[,2] <- tot_ssa_mean
            j_df_mean[,3] <- tot_jco2_mean
            j_df_mean[,4] <- tot_jch4_mean
            j_df_mean$LocSite <- factor(j_df_mean$LocSite)
          ## MINIMUM
            tot_ssa_min <- (round(as.data.frame(tapply(flux_df$minSSA, flux_df$WtrshdID, FUN=sum)),4))
            tot_jco2_min <- (round(as.data.frame(tapply(flux_df$jCO2kgCdlyMin, flux_df$WtrshdID, FUN=sum)),4))
            tot_jch4_min <- (round(as.data.frame(tapply(flux_df$jCH4kgCdlyMin, flux_df$WtrshdID, FUN=sum)),4))
            j_df_min <- as.data.frame(matrix(nrow=nrow(tot_jco2_min), ncol=4))
            names(j_df_min) <- c("LocSite","SSAtot_min","jCO2tot_min","jCH4tot_min")
            j_df_min[,1] <- row.names(tot_jco2_min)
            j_df_min[,2] <- tot_ssa_min
            j_df_min[,3] <- tot_jco2_min
            j_df_min[,4] <- tot_jch4_min
            j_df_min$LocSite <- factor(j_df_min$LocSite)
          ## MAX
            tot_ssa_max <- (round(as.data.frame(tapply(flux_df$maxSSA, flux_df$WtrshdID, FUN=sum)),4))
            tot_jco2_max <- (round(as.data.frame(tapply(flux_df$jCO2kgCdlyMax, flux_df$WtrshdID, FUN=sum)),4))
            tot_jch4_max <- (round(as.data.frame(tapply(flux_df$jCH4kgCdlyMax, flux_df$WtrshdID, FUN=sum)),4))
            j_df_max <- as.data.frame(matrix(nrow=nrow(tot_jco2_max), ncol=4))
            names(j_df_max) <- c("LocSite","SSAtot_max","jCO2tot_max","jCH4tot_max")
            j_df_max[,1] <- row.names(tot_jco2_max)
            j_df_max[,2] <- tot_ssa_max # Store stream surface area in each watershed for correcting terrestrial NEE estimates
            j_df_max[,3] <- tot_jco2_max
            j_df_max[,4] <- tot_jch4_max
            j_df_max$LocSite <- factor(j_df_max$LocSite)
          ## Store in new df: daily total efflux (kgC/d) for each watershed (by WtrshdID)
            j_df <- merge(x=j_df_mean, y=j_df_min, by="LocSite")
            j_df <- merge(x=j_df, y=j_df_max, by="LocSite")
        
            
    # Determine terrestrial vs aquatic land cover in watersheds ####
            
      # Merge to dataframe 'c5means'
        c5means$LocSite <- paste0(c5means$Loc,"0",c5means$Site)
        c5means$LocSite <- factor(c5means$LocSite)
        lc_df <- merge(x=c5means, y=j_df, by="LocSite", all.x=T, all.y=T, sort=T)
        lc_df <- subset(lc_df, select=c("LocSite","Loc","Site","ShedAream2","LakesPondpcnt","SSAtot_mean","SSAtot_min","SSAtot_max"))
      # Calculate terrestrial, aquatic, and streams landcover proportions in each watershed, for NEE calcs later
        ## MEAN
          lc_df$propTerrArea_mean <- (lc_df$ShedAream2-((lc_df$ShedAream2*(lc_df$LakesPondpcnt)/100)+lc_df$SSAtot_mean))/lc_df$ShedAream2
          lc_df$propAquaticArea_mean <- (lc_df$ShedAream2-(lc_df$propTerrArea_mean*lc_df$ShedAream2))/lc_df$ShedAream2
          lc_df$propStreamsArea_mean <- lc_df$SSAtot_mean/lc_df$ShedAream2
          #subset(lc_df, select=c("Loc","Site","propTerrArea","propAquaticArea","propStreamsArea"))
        ## MIN
          lc_df$propTerrArea_min <- (lc_df$ShedAream2-((lc_df$ShedAream2*(lc_df$LakesPondpcnt)/100)+lc_df$SSAtot_min))/lc_df$ShedAream2
          lc_df$propAquaticArea_min <- (lc_df$ShedAream2-(lc_df$propTerrArea_min*lc_df$ShedAream2))/lc_df$ShedAream2
          lc_df$propStreamsArea_min <- lc_df$SSAtot_min/lc_df$ShedAream2
        ## MAX
          lc_df$propTerrArea_max <- (lc_df$ShedAream2-((lc_df$ShedAream2*(lc_df$LakesPondpcnt)/100)+lc_df$SSAtot_max))/lc_df$ShedAream2
          lc_df$propAquaticArea_max <- (lc_df$ShedAream2-(lc_df$propTerrArea_max*lc_df$ShedAream2))/lc_df$ShedAream2
          lc_df$propStreamsArea_max <- lc_df$SSAtot_max/lc_df$ShedAream2
      # Add 'LocSite' to lc_df df, for later merging with NEE data (~L1200)
        lc_df$SiteID <- lc_df$LocSite
        
      # Proportion of watershed surface area that is terrestrial, aquatic, and streams only
        par(mar=c(4.5,4.5,1,1), mfrow=c(3,1))
        boxplot(lc_df$propTerrArea_mean~lc_df$Loc, ylab="Prop. of region comprised of terrestrial surfaces", lwd=1.5, col="darkgray")
        boxplot(lc_df$propAquaticArea_mean~lc_df$Loc, ylab="Prop. of region comprised of aquatic surfaces", lwd=1.5, col="darkgray")
        boxplot(lc_df$propStreamsArea_mean~lc_df$Loc, ylab="Prop. of region comprised of stream surfaces", lwd=1.5, col="darkgray")
          