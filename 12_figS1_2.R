#========================================================================================================#
# 12_figS1.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-06-17
# Background: Derive mean air temperature and total rainfall over desired interval preceding sampling,
#             graphics for Figures S1, S2
#========================================================================================================#

  # BACKGROUND ####
    
    # Parameters and units ####
      # AirTavg: mean hourly air temperature, ˚C
      # RainTot: total hourly rainfall, mm
    
    # Functions ####
      # Derive historical meteorological conditions during user-specified hours prior to sampling event
      ## Note: Calculates mean for all variables except Rainfall (total)
        hx_clim <- function(HrsInPast){
          hxclim <- as.data.frame(matrix(nrow=nrow(clim_mrg), ncol=5))
          names(hxclim) <- c("Date","Loc","Site",paste0("AirTavg",HrsInPast),paste0("RainTot",HrsInPast))
          j=1
          for(i in 1:nrow(clim_mrg)){
            recnos <- seq(from=clim_mrg[i,7]-HrsInPast, to=clim_mrg[i,7]-1, by=1) # Store record met stn record #s for past x hours (i.e. HrsInPast), excluding sampling hour
            clim_sub_df <- subset(clim, clim$recno %in% recnos) # Extract met data for previously stored record #s
            hxclim[j,1] <- as.character(clim_mrg[i,1]) # Date
            hxclim[j,2] <- as.character(clim_mrg[i,3]) # Loc (i.e., region)
            hxclim[j,3] <- as.character(clim_mrg[i,4]) # Site
            hxclim[j,4] <- colMeans(subset(clim_sub_df, select=c("AirTavg"))) # Mean air temp over previous 24h
            hxclim[j,5] <- round(sum(subset(clim_sub_df, select=c("RainTot"))),3) # Cumulative precipitation over previous 24h
            j <- j+1
          }
          return(hxclim)
        }
       
      # Derive total daily precipitation by study region
        reg_precip <- function(region){
          
          # Subset data
            clim_sub <- droplevels(subset(clim, clim$Loc==region))
          
          # Create dataframe
            names <- c("date","rain_tot","rain_tot_sd")
            df <- as.data.frame(matrix(ncol=length(names), nrow=length(unique(clim_sub$Date_clim))))
            names(df) <- names
            j=1
            for(i in unique(clim_sub$Date_clim)){
              df_sub <- droplevels(subset(clim_sub, clim_sub$Date_clim==i, select="RainTot"))
              df[j,1] <- i
              df[j,2] <- sum(df_sub$RainTot)
              df[j,3] <- sd(df_sub$RainTot)
              j <- j+1
            }
            df$date <- as.factor(df$date)
            clim$Date_clim <- format(as.POSIXct(clim$DateTime,format="%m/%d/%y %H:%M"),"%m/%d/%y")
            df$jday <- format(as.POSIXlt(df$date,format="%m/%d/%y"),"%j"); df$jday <- as.numeric(df$jday)
          
          # Merge sampling day w/ climate data
            smpl_day <- as.data.frame(unique(droplevels(subset(c5df, select="Date", c5df$Loc==region))$Date))
            names(smpl_day) <- "date"
            smpl_day$val <- 1
            df <- merge(x=df, y=smpl_day, by="date", all.x=T)
            df$rain_smpl <- df$rain_tot[df$val>0]

          # Plot
            # Conditionals
              if(region=="BB"){col <- "blue"; title <- "Richardson Mountains"}
              if(region=="PP"){col <- "purple"; title <- "Peel Plateau"}
              if(region=="AP"){col <- "orange"; title <- "Travaillant Uplands"}
            
            plot <- ggplot(data=df, aes(y=rain_tot, x=jday)) +
              # Scales
                scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
              # Themes
                theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                      panel.grid.minor=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.background=element_rect(fill="white"),
                      panel.border=element_rect(colour="black", fill=NA, size=1),
                      plot.background=element_rect(colour="white", size=1),
                      axis.text.y=element_text(angle=0, hjust=0.5, colour="black"),
                      axis.title.y.left=element_text(margin=margin(t=0, r=10, b=0, l=0)),
                      #axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=10)),
                      axis.text.x=element_text(angle=0, hjust=0.5, colour="black"),
                      axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0)),#element_blank()
                      #axis.title.x=element_text(margin=margin(t=-10, r=0, b=0, l=0)),
                      text=element_text(size=16),
                      plot.title=element_text(size=18),
                      legend.position="none") +
              # Data
                stat_summary(data=df, aes(ymin=rain_tot-rain_tot_sd, ymax=rain_tot+rain_tot_sd), fill=col, fun.data=mean, geom="ribbon", alpha=0.5) +
                geom_path(color=col, size=0.5) +
                geom_point(data=df, aes(y=rain_smpl, x=jday), pch=21, fill=col, col="black", size=2.5, stroke=1, alpha=1) +
              # Axis labels
                labs(title=title, y="Daily precipitation (mm)", x="Julian Day")
            
            # Return data
              #return(df)
              return(plot)
            
        }
    
  # DATA PROCESSING AND ANALYSES ####
    
    # Prepare data ####
      c5_sub <- droplevels(subset(c5df, select=c("Date","Time","DateTime","Loc","Site")))
      clim_sub <- droplevels(subset(clim, select=c("Date_clim","Time_clim","DateTime","Loc","AirTavg","RainTot","recno")))
      clim_mrg <- merge(x=c5_sub, y=clim_sub, by=c("Loc","DateTime"), all.x=T, sort=T)
      clim_mrg <- clim_mrg[,!names(clim_mrg) %in% c("DateTime","Date_clim","Time_clim")]
      clim_mrg <- subset(clim_mrg, select=c("Date","Time","Loc","Site","AirTavg","RainTot","recno"))
      
    # Test
      hx_clim(HrsInPast=12)
      
    # Analyses ####
      # Test for effects of preceding rainfall on water quality (Cond), carbon conc (TCuM), and fluxes (TC)
        par(mar=c(4.5,4.5,1,1))
        plot(c5df$TCuM~c5df$RainTot96, pch=21, cex=1.4, col="black", bg=regional_colors[c5df$Loc])
        plot(c5df$TCyld~c5df$RainTot96, pch=21, cex=1.4, col="black", bg=regional_colors[c5df$Loc])
     
      # Corr plot
        cor_df1 <- droplevels(subset(c5df, c5df$Loc=="PP", select=c("Date","Loc","Site","Cond","TCflux","RainTot","RainTot12","RainTot24","RainTot48","RainTot72","RainTot96"))) #"TCyld",
        cor_df2 <- cor_df1
        cor_df2$Cond_ln <- log(cor_df2$Cond); cor_df2$TCflux_ln <- log(cor_df2$TCflux); cor_df2$RainTot_ln <- log(cor_df2$RainTot+0.0001); cor_df2$RainTot12_ln <- log(cor_df2$RainTot12+0.0001); cor_df2$RainTot24_ln <- log(cor_df2$RainTot24+0.0001); cor_df2$RainTot48_ln <- log(cor_df2$RainTot48+0.0001); cor_df2$RainTot72_ln <- log(cor_df2$RainTot72+0.0001); cor_df2$RainTot96_ln <- log(cor_df2$RainTot96+0.0001)
        cor_df2 <- droplevels(subset(cor_df2, select=c("Date","Loc","Site","Cond_ln","TCflux_ln","RainTot_ln","RainTot12_ln","RainTot24_ln","RainTot48_ln","RainTot72_ln","RainTot96_ln"))) #"TCyld",
        cor_df <- cor_df2
        cordf <- round(cor(cor_df[4:ncol(cor_df)], use="pairwise.complete.obs"), 4)
        p.mat <- cor.mtest(cor_df[4:ncol(cor_df)])
        corrplot(cordf, type="lower", order="original", diag=F, number.cex=0.6, tl.col="black", tl.cex=0.75, cl.cex=0.8, p.mat=p.mat, sig.level=0.05, col=brewer.pal(n=10, name="RdYlBu")) # insig="p-value", insig="label_sig", 
        
      # Graphs
        par(mar=c(4.5,4.5,1,1))
        plot( log(c5df$Cond[c5df$Loc=="BB"]) ~ log(c5df$RainTot12[c5df$Loc=="BB"]), pch=21, cex=1.4, col="black", bg=regional_colors[1])
        plot( log(c5df$Cond[c5df$Loc=="BB"]) ~ log(c5df$RainTot24[c5df$Loc=="BB"]), pch=21, cex=1.4, col="black", bg=regional_colors[1])
        plot( log(c5df$Cond[c5df$Loc=="BB"]) ~ log(c5df$RainTot96[c5df$Loc=="BB"]), pch=21, cex=1.4, col="black", bg=regional_colors[1])
        plot( log(c5df$Cond[c5df$Loc=="AP"]) ~ log(c5df$RainTot96[c5df$Loc=="AP"]), pch=21, cex=1.4, col="black", bg=regional_colors[4])
        
       
      # Daily precip by study region
        
        reg_precip("PP")

        