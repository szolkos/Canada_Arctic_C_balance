#========================================================================================================#
# 13_figS3 ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Plot mean daily NEE estimated from SPL4CMDL vs. AmeriFlux stations
#========================================================================================================#

  # BACKGROUND ####

    # Parameters and units ####
      # Detailed below    

    # Functions ####

      # Daily means from ameriflux stations
        amflux_daily_means <- function(station){
          stn <- station
          dates <- as.character(unique(amflux_sub$Date[amflux_sub$Station==stn]))
          af_means <- as.data.frame(matrix(nrow=length(dates), ncol=ncol(amflux_sub)))
          j=1
          for(i in dates){
            af_means[j,1] <- stn
            af_means[j,2] <- i
            af_means[j,3] <- colMeans((na.omit(subset(amflux_sub, select="NEE", amflux_sub$Station==stn & Date==i))))
            af_means[j,4] <- colMeans((na.omit(subset(amflux_sub, select="FC", amflux_sub$Station==stn & Date==i))))
            af_means[j,5] <- colMeans((na.omit(subset(amflux_sub, select="GPP", amflux_sub$Station==stn & Date==i))))
            af_means[j,6] <- colMeans((na.omit(subset(amflux_sub, select="Reco", amflux_sub$Station==stn & Date==i))))
            j <- j+1
          }
          names(af_means) <- c("Station","Date","NEE","FC","GPP","Reco")
          return(af_means)
      }
    
  # DATA PROCESSING AND ANALYSIS ####

    # Compare AmeriFlux and SMAP (SPL4CMDL) NEE data ####
        
      # Read in data; Note: AmeriFlux flux units (e.g. NEE, FC) are µmol/m2/s
        amflux <- read.csv(paste0(dir,"Ch5/","AmeriFlux_sz092919.csv"), header=T)
        
      # Prepare data
        amflux_sub <- drop.levels(subset(amflux, select=c("Station","Date","NEE","FC","GPP","Reco")))
        amflux_sub$Station <- factor(amflux_sub$Station, levels=unique(amflux_sub$Station))
        amflux_sub <- drop.levels(subset(amflux_sub, amflux_sub$Station != "CA_SCB" & amflux_sub$Station != "US_NGB"))
        unique(amflux_sub$Station)
        
      # Calculate daily means
        # Calculate (CA_TVC & CA_DL1 are processed below)
          af_means_US_A10 <- amflux_daily_means("US_A10") # No GPP or Reco
          af_means_US_A03 <- amflux_daily_means("US_A03") # No GPP or Reco
          af_means_US_IVO <- amflux_daily_means("US_IVO") # No GPP or Reco
          af_means_US_EML <- amflux_daily_means("US_EML")
          af_means_US_PRR <- amflux_daily_means("US_PRR")
          af_means_US_UAF <- amflux_daily_means("US_UAF")
          af_means_CA_SCC <- amflux_daily_means("CA_SCC")
        # Where needed, assign FC to NEE
          af_means_US_IVO$NEE <- af_means_US_IVO$FC
          af_means_US_PRR$NEE <- af_means_US_PRR$FC
        
      # Bind means into one dataframe, replace NaN with NA
        af_means_all <- rbind(af_means_US_A10, af_means_US_A03, af_means_US_EML, af_means_US_IVO, af_means_US_PRR, af_means_US_UAF, af_means_CA_SCC)
        af_means_all$NEE[af_means_all$NEE=="NaN"] <- NA
        af_means_all$FC[af_means_all$FC=="NaN"] <- NA
        af_means_all$GPP[af_means_all$GPP=="NaN"] <- NA
        af_means_all$Reco[af_means_all$Reco=="NaN"] <- NA
        #af_means_all[8] <- NULL
        
      # Read in measurements from Daring Lake and Trail Valley Creek (mean daily, June 01-Aug 31, 2016)
        ca_dl <- read.csv(paste0(dir,"Ch5/","AmeriFlux_CA_DL_1.csv"), header=T)
        ca_tvc <- read.csv(paste0(dir,"Ch5/","AmeriFlux_CA_TVC.csv"), header=T)
        
      # Add CA_DL1 and CA_TVC to dataframe
        af_means_all <- rbind(af_means_all,ca_dl)
        af_means_all <- rbind(af_means_all,ca_tvc)
        
      # Convert NEE from µmol/m2/s to gC/m2/d
        af_means_all$NEE <- ((af_means_all$NEE/1000000)*12.0107)*86400
        af_means_all$FC <- ((af_means_all$FC/1000000)*12.0107)*86400
        af_means_all$GPP <- ((af_means_all$GPP/1000000)*12.0107)*86400
        af_means_all$Reco <- ((af_means_all$Reco/1000000)*12.0107)*86400
        
      # Store Station ID as factor
        af_means_all$Station <- factor(af_means_all$Station, levels=unique(af_means_all$Station))
        
      # Read in SPL4CMDL data
        smap_amfluxstn_dailymeans <- read.csv(paste0(dir,"Ch5/","SMAP_AmFluxStn_dailymeans.csv"), header=T)
        smap_amfluxstn_dailymeans <- drop.levels(subset(smap_amfluxstn_dailymeans, select=c("Station","Date","SMAP_NEEmean","SMAP_NEEsd","SMAP_GPPmean","SMAP_GPPsd"), smap_amfluxstn_dailymeans$Station != "US_NGB"))
        as.character(unique(smap_amfluxstn_dailymeans$Station))
        
      # Merge SPL4CMDL to AmeriFlux data
        af_smap <- merge(x=af_means_all[1:6], y=smap_amfluxstn_dailymeans, by=c("Station","Date"), all.x=T, all.y=T, sort=T) 
        af_smap$Station <- factor(af_smap$Station, levels=c("US_A10","US_A03","US_IVO","CA_DL1","US_UAF","US_PRR","CA_SCC","US_EML","CA_TVC"))
          af_smap_sub <- droplevels(subset(af_smap, af_smap$Station!="US_A10" & af_smap$Station!="US_A03"))
        
          # Plot values
            par(mar=c(4.5,4.5,1,1))
            plot(af_smap$SMAP_NEEmean~af_smap$NEE, type="n", ylab="NEE (SMAP)", xlab="NEE (AmeriFlux)", ylim=c(-4,2), xlim=c(-4,2))
            abline(a=0, b=1, lwd=1.2, lty=1, col="blue")
            points(af_smap$SMAP_NEEmean ~ af_smap$NEE, pch=21, bg=cols[af_smap$Station], col="black", lwd=1.2, cex=1.2)
            #text(c5_smap$SMAP_NEEmean~c5_smap$NEE, labels=c5_smap$Station,cex=0.4, pos=3)
            r2co2 <- round(summary(lm(af_smap$SMAP_NEEmean ~ af_smap$NEE))$adj.r.sq, 2)
            lgndco2 <- as.character(as.expression(bquote(Adjusted~italic(R)^2 == .(r2co2))))
        
      # Merge SPL4CMDL and AmeriFlux daily mean NEE values to sampling days
        c5_smap_Date <- as.data.frame(unique(c5df$Date))
        names(c5_smap_Date) <- "Date"
        c5_smap <- merge(x=c5_smap_Date, y=af_smap, by="Date", all.x=T, all.y=T, sort=T) 
        c5_smap <- drop.levels(na.omit(subset(c5_smap, select=c("Date","Station","NEE","SMAP_NEEmean"))))
        c5_smap$Station <- factor(c5_smap$Station, levels=c("US_A10","US_A03","US_IVO","CA_DL1","US_UAF","US_PRR","CA_SCC","US_EML","CA_TVC"))
        cols <- c("US_A10"="black", "US_A03"="darkgray", "US_IVO"="red", "CA_DL1"="orange", "US_UAF"="yellow", "US_PRR"="green", "CA_SCC"="lightblue", "US_EML"="blue", "CA_TVC"="purple")
        
      # Use linear model to compare NEE from SPL4CMDL versus AmeriFlux
        c5_smap_lm <- lm(c5_smap$SMAP_NEEmean~c5_smap$NEE)
        par(mar=c(4.5,4.5,1,1), mfrow=c(2,2))
        plot(c5_smap_lm)
        summary(c5_smap_lm)
        par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
        
      # Plot values
        par(mar=c(4.5,4.5,1,1))
        plot(c5_smap$SMAP_NEEmean~c5_smap$NEE, type="n", ylab="NEE (SMAP)", xlab="NEE (AmeriFlux)") # ylim=c(-1.3,-0.3), xlim=c(-1.3,-0.4),
        #abline(a=-0.4588, b=0.2732, lwd=1.2, lty=2, col="darkgray")
        abline(a=0, b=1, lwd=1.2, lty=1, col="blue")
        points(c5_smap$SMAP_NEEmean ~ c5_smap$NEE, pch=21, bg=cols[c5_smap$Station], col="black", lwd=1.2, cex=1.2)
        #text(c5_smap$SMAP_NEEmean~c5_smap$NEE, labels=c5_smap$Station,cex=0.4, pos=3)
        r2co2 <- round(summary(lm(c5_smap$SMAP_NEEmean ~ c5_smap$NEE))$adj.r.sq, 2)
        lgndco2 <- as.character(as.expression(bquote(Adjusted~italic(R)^2 == .(r2co2))))
        
      # Subset- omit US_A10, US_A03
        c5_smap_sub <- droplevels(subset(c5_smap, c5_smap$Station!="US_A10" & c5_smap$Station!="US_A03"))
        c5_smap_USA10_USA03 <- droplevels(subset(c5_smap, c5_smap$Station=="US_A10" | c5_smap$Station=="US_A03"))
        par(mar=c(4.5,4.5,1,1))
        plot(c5_smap_sub$SMAP_NEEmean~c5_smap_sub$NEE, type="n", ylab="NEE (SMAP)", xlab="NEE (AmeriFlux)") # ylim=c(-1.3,-0.3), xlim=c(-1.3,-0.4),
        abline(a=0, b=1, lwd=1.2, lty=1, col="black")
        cols_sub <- c("US_IVO"="red", "CA_DL1"="orange", "US_UAF"="yellow", "US_PRR"="green", "CA_SCC"="lightblue", "US_EML"="blue", "CA_TVC"="purple")
        points(c5_smap_sub$SMAP_NEEmean ~ c5_smap_sub$NEE, pch=21, bg=cols_sub[c5_smap_sub$Station], col="black", lwd=1.2, cex=1.2)
      # Linear model
        c5_smap_sub_lm <- lm(c5_smap_sub$SMAP_NEEmean~c5_smap_sub$NEE)
        par(mar=c(4.5,4.5,1,1), mfrow=c(2,2))
        plot(c5_smap_sub_lm)
        summary(c5_smap_sub_lm)
        par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
        
        tapply(c5_smap_sub)
        
      # Compile AmeriFlux and SPL4CMDL NEE means and sd, for comparison
          unique(c5_smap$Date)
        ## AmeriFlux
          nee_amflux_mean <- as.data.frame(round(tapply(c5_smap$NEE, c5_smap$Station, FUN=mean, na.rm=T),4))
          nee_amflux_sd <- as.data.frame(round(tapply(c5_smap$NEE, c5_smap$Station, FUN=sd, na.rm=T),4))
          nee_amflux <- cbind(row.names(nee_amflux_mean), nee_amflux_mean, nee_amflux_sd)
          names(nee_amflux) <- c("Station","nee_amflux_mean","nee_amflux_sd")
        ## SPL4CMDL
          nee_smap_mean <- as.data.frame(round(tapply(c5_smap$SMAP_NEEmean, c5_smap$Station, FUN=mean, na.rm=T),4))
          nee_smap_sd <- as.data.frame(round(tapply(c5_smap$SMAP_NEEmean, c5_smap$Station, FUN=sd, na.rm=T),4))
          nee_smap <- cbind(row.names(nee_smap_mean), nee_smap_mean, nee_smap_sd)
          names(nee_smap) <- c("Station","nee_smap_mean","nee_smap_sd")
        ## Combine dataframes
          nee_df <- merge(x=nee_amflux, y=nee_smap, by="Station")
          nee_df$Station <- factor(nee_df$Station, levels=unique(nee_df$Station))
          
          
      # Calculate summertime mean NEE from SMAP data for each Ch5 site
        ## NEE units are in gC/m2/d- see https://nsidc.org/data/SPL4CMDL/versions/4
          # values in 'smap' are daily means covering 06/01/2016 to 08/31/2016, inclusive
            smap <- read.csv(paste0(dir,"Ch5/","SMAP_summer2016_ch5sites.csv"), header=T)
        
        # Determine Julian Day for SMAP NEE measurements
          JulianDay_smap <- as.data.frame(as.Date(smap$Date, "%m/%d/%y")); names(JulianDay_smap) <- "JulianDay"; JulianDay_smap <- (yday(JulianDay_smap$JulianDay)) # Add column to ysi for Julian day
          smap <- cbind(JulianDay_smap, smap) # Convert with yday into a new column "julian"
        # Add 'ID' variable to  'c5df', for matching JulianDay date range of field samples with SMAP NEE data
          c5df$ID <- paste0(c5df$Loc,"0",c5df$Site)
          ## Test:
          range(subset(c5df, select="JulianDay", c5df$ID=="AP01"))
        # Create dataframe
          ids <- as.character(unique(smap$ID))
          c5nee_df <- as.data.frame(matrix(nrow=length(unique(smap$ID)), ncol=6))
          names(c5nee_df) <- c("SiteID","Loc","Site","NEEmean","NEEsd","NEEmean_rmse")
          j=1
          for(i in ids){
            c5nee_df[j,1] <- i
            c5nee_df[j,2] <- substr(i,1,2)
            c5nee_df[j,3] <- as.numeric(regmatches(i, gregexpr("[[:digit:]]+", i))) # Exract Site #
            min_date <- min(range(subset(c5df, select="JulianDay", c5df$ID==i)))
            max_date <- max(range(subset(c5df, select="JulianDay", c5df$ID==i)))
            c5nee_df[j,4] <- colMeans((na.omit(subset(smap, select="NEEmean", smap$ID==i))))
            # Calculate mean NEE for time range conciding with sampling period of Loc/Site (i.e. ID, e.g. 'AP01')
            #c5nee_df[j,4] <- colMeans(na.omit(subset(smap, select="NEEmean", smap$ID==i & smap$JulianDay_smap >= min_date & smap$JulianDay_smap <= max_date)))
            c5nee_df[j,5] <- colMeans((na.omit(subset(smap, select="NEEsd", smap$ID==i))))
            c5nee_df[j,6] <- colMeans((na.omit(subset(smap, select="NEEmean_rmse", smap$ID==i))))
            j <- j+1
          }
        # **Omit AP-08 b/c it has 1 sample, can't calculate reliable NEE
          #c5nee_df <- drop.levels(subset(c5nee_df, c5nee_df$SiteID!="AP08"))
          
          c5nee_df$Loc <- factor(c5nee_df$Loc, levels=c("BB","PP","PA","AP"))
          
          par(mar=c(4.5,4.5,1,1))
          boxplot(c5nee_df$NEEmean ~ c5nee_df$Loc, lwd=2, col=regional_colors, ylab=expression(NEE~(gC~m^-2~d^-1)))
    
          
    # Graphics
          
      # Plot SMAP NEE vs. AmeriFlux NEE for days on which I sampled
        ggplot(c5_smap, aes(x=NEE, y=SMAP_NEEmean)) + #fill=factor(Station)
          geom_abline(intercept=0, slope=1, color="black", linetype="solid", size=1) +
          geom_smooth(method="lm", se=TRUE) +
          geom_point(shape=21, fill="black", color="white", size=2) +
          #geom_point(shape=21, color="black", size=2) +
          scale_shape_manual(values=21) +
          #scale_fill_manual(values=cols) +
          geom_point(data=c5_smap, aes(x=NEE, y=SMAP_NEEmean), col="black", shape=21, size=2, fill=cols[c5_smap$Station]) +
          #geom_point(data=c5_smap_sub, aes(x=NEE, y=SMAP_NEEmean), col="black", shape=21, size=2, fill=cols_sub[c5_smap_sub$Station]) + # for no US_A10 or US_A03
          theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                plot.background=element_rect(colour="white", size=1),
                panel.border=element_rect(colour="black", fill=NA, size=1),
                text=element_text(size=14)) +
          theme(axis.text.x=element_text(angle=0, hjust=0.5, colour="black")) +
          theme(axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0))) +
          theme(axis.text.y=element_text(angle=0, hjust=0.5, colour="black")) +
          theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
          #theme(legend.position="none") +
          theme(plot.background=element_rect(fill='white')) +
          theme(panel.background=element_rect(fill='white')) +
          labs(y=expression(NEE~(SPL4CMDL*","~gC~m^-2~d^-1)), x=expression(NEE~(AmeriFlux*","~gC~m^-2~d^-1)))
        
      # Plot mean SMAP NEE vs. AmeriFlux NEE, w/ error bars (std dev), export as 4" x 5" landscape PDF
        ggplot(data=nee_df, aes(x=nee_amflux_mean, y=nee_smap_mean, colour="black")) +
          # Set axis limits
          #scale_y_continuous(limits=c(-2.1,0.4), breaks=seq(-2,0.5,1)) +
          #scale_x_continuous(limits=c(-3.4,2.3), breaks=seq(-3,2,1)) +
          scale_y_continuous(limits=c(-3.5,2.5), breaks=seq(-3,3,1)) +
          scale_x_continuous(limits=c(-3.5,2.5), breaks=seq(-3,3,1)) +
          # Set panel theme
          theme_bw() +
          theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                text=element_text(size=14)) +
          theme(axis.text.x=element_text(angle=0, hjust=0.5, colour="black")) +
          theme(axis.text.y=element_text(angle=0, hjust=0.5, colour="black")) +
          theme(plot.background=element_rect(fill='white')) +
          theme(panel.background=element_rect(fill='white')) +
          theme(legend.position="none") +
          theme(axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0))) +
          theme(axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0))) +
          # Add horizontal and vertical reference lines
          #geom_abline(intercept=0, slope=1, color="blue", linetype="solid", size=0.4) +
          geom_segment(x=-3, xend=2, y=-3, yend=2, color="blue", linetype="solid", size=0.4) +
          #geom_abline(intercept=-0.1541, slope=0.6852, color="black", linetype="solid", size=0.5) +
          #geom_abline(intercept=-0.4588, slope=0.2732, color="darkgray", linetype="dashed", size=0.5) +
          # Add data points
          geom_errorbar(aes(ymin=nee_df$nee_smap_mean-nee_df$nee_smap_sd, ymax=nee_df$nee_smap_mean+nee_df$nee_smap_sd), colour="black") +
          geom_errorbarh(aes(xmin=nee_df$nee_amflux_mean-nee_df$nee_amflux_sd, xmax=nee_df$nee_amflux_mean+nee_df$nee_amflux_sd), colour="black") +
          geom_point(aes(fill=nee_df$Station, shape=nee_df$Station), size=5, color="black", stroke=0.6) +
          scale_fill_manual(nee_df$Station, values=rep("darkgray",9)) +
          scale_shape_manual(nee_df$Station, values=rep(21,9)) +
          # Add axis labels
          labs(y=expression(NEE~(SPL4CMDL*","~gC~m^-2~d^-1)), x=expression(NEE~(AmeriFlux*","~gC~m^-2~d^-1)))
  
    # Plot mean SMAP NEE vs. AmeriFlux NEE, w/ error bars (std dev), export as 4" x 5" landscape PDF
      ggplot(data=af_smap_sub, aes(x=NEE, y=SMAP_NEEmean)) +
        # Set axis limits
          scale_y_continuous(limits=c(-4,3), breaks=seq(-4,2,2)) +
          scale_x_continuous(limits=c(-4,3), breaks=seq(-4,2,2)) +
        # Set panel theme
          theme_bw() +
          theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                text=element_text(size=14)) +
          theme(axis.text.x=element_text(angle=0, hjust=0.5, colour="black")) +
          theme(axis.text.y=element_text(angle=0, hjust=0.5, colour="black")) +
          theme(plot.background=element_rect(fill='white')) +
          theme(panel.background=element_rect(fill='white')) +
          #theme(legend.position="none") +
          theme(axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0))) +
          theme(axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0))) +
        # Add regression line
          #geom_smooth(method="glm", se=TRUE) +
        # Add horizontal and vertical reference lines
          geom_abline(intercept=0, slope=1, color="black", linetype="solid", size=1.2) +
          #geom_segment(x=-3, xend=2, y=-3, yend=2, color="blue", linetype="solid", size=0.4) +
          #geom_abline(intercept=-0.1541, slope=0.6852, color="black", linetype="solid", size=0.5) +
          #geom_abline(intercept=-0.4588, slope=0.2732, color="darkgray", linetype="dashed", size=0.5) +
        # Add data points
          #geom_errorbar(aes(ymin=nee_df$nee_smap_mean-nee_df$nee_smap_sd, ymax=nee_df$nee_smap_mean+nee_df$nee_smap_sd), colour="black") +
          #geom_errorbarh(aes(xmin=nee_df$nee_amflux_mean-nee_df$nee_amflux_sd, xmax=nee_df$nee_amflux_mean+nee_df$nee_amflux_sd), colour="black") +
          ##scale_fill_manual(nee_df$Station, values=rep("darkgray",9)) +
          #scale_shape_manual(nee_df$Station, values=rep(21,9)) +
        # Geometry
          #geom_hex(bins=25) +
          #geom_point(data=af_smap_sub, aes(x=NEE, y=SMAP_NEEmean), col="black", shape=21, size=2, fill=cols_sub[af_smap_sub$Station]) +
          #scale_fill_continuous(type = "viridis") +
          geom_density_2d(aes(fill=stat(level)), geom="raster", contour=F)+
          #scale_fill_distiller(palette=4, direction=-1) +
          #scale_x_continuous(expand=c(0,0)) +
          #scale_y_continuous(expand=c(0,0)) +
        # Add axis labels
          labs(y=expression(NEE~(SPL4CMDL*","~gC~m^-2~d^-1)), x=expression(NEE~(AmeriFlux*","~gC~m^-2~d^-1)))
      