#========================================================================================================#
# 6_fig4a.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Plot carbon species fluxes as proportions of regional total
#========================================================================================================#

  # BACKGROUND ####
  
    # Parameters and units ####
      # Fluxes: kgC/d    

    # Functions ####
      # Proportion of total fluvial C fluxes by site & region
        prop_fluxes <- function(loc, efflux_min_mean_max, print_means_yes_no){
          # Subset data
            df_sub <- droplevels(subset(necb_df, necb_df$Loc==loc, select=c("Loc","Site","TCkgCd","DICkgCd","PICkgCd","DOCkgCd","POCkgCd","jCO2kgCd_mean","jCO2kgCd_min","jCO2kgCd_max","jCH4kgCd_mean","jCH4kgCd_min","jCH4kgCd_max","NEEmeankgCd_mean","NEEmeankgCd_min","NEEmeankgCd_max")))
          # Prepare dataframe
            sites <- unique(df_sub$Site)
            df <- as.data.frame(matrix(nrow=length(sites), ncol=13))
            names(df) <- c("Loc","Site","JCO2p","JCH4p","DICp","PICp","DOCp","POCp","TotalfluvialC","NEE","Lateral_NEE","Efflux_NEE","Totalfluvial_NEE")
            j=1
          # Store data
            for(i in sites){
              # Subset data
                df_loc <- droplevels(subset(df_sub, df_sub$Site==i))
              # Store variables
                if(efflux_min_mean_max=="mean"){
                  jco2 <- df_loc$jCO2kgCd_mean
                  jch4 <- df_loc$jCH4kgCd_mean
                  nee <- df_loc$NEEmeankgCd_mean
                }
                if(efflux_min_mean_max=="min"){
                  jco2 <- df_loc$jCO2kgCd_min
                  jch4 <- df_loc$jCH4kgCd_min
                  nee <- df_loc$NEEmeankgCd_max
                }
                if(efflux_min_mean_max=="max"){
                  jco2 <- df_loc$jCO2kgCd_max
                  jch4 <- df_loc$jCH4kgCd_max
                  nee <- df_loc$NEEmeankgCd_min
                }
                dic <- df_loc$DICkgCd
                pic <- df_loc$PICkgCd
                doc <- df_loc$DOCkgCd
                poc <- df_loc$POCkgCd
                tc <- df_loc$TCkgCd # total lateral carbon flux
                lateral <- sum(dic,pic,doc,poc)
                efflux <- sum(jco2, jch4)
                total <- sum(lateral,efflux)
              # Store data
                df[j,1] <- loc
                df[j,2] <- i
                df[j,3] <- round(jco2/total,4)
                df[j,4] <- round(jch4/total,4)
                df[j,5] <- round(dic/total,4)
                df[j,6] <- round(pic/total,4)
                df[j,7] <- round(doc/total,4)
                df[j,8] <- round(poc/total,4)
                df[j,9] <- round(total,4) # total fluvial c
                df[j,10] <- round(nee*-1,4)
                df[j,11] <- round(lateral/nee*-1,4)
                df[j,12] <- round(efflux/nee*-1,4)
                df[j,13] <- round(total/nee*-1,4)
              # Recursive
                j <- j+1
            }
          # Optional: include column Mean + SD
            if(print_means_yes_no=="yes"){
              df[length(sites)+1,1:2] <- c("Mean","-")
              MEAN <- format(colMeans(na.omit(df[3:13])), scientific=F)
              SD <- round(apply(na.omit(df[,3:13]), 2, sd),4)
              df[length(sites)+1,3:13] <- paste0(MEAN," (",SD,")")
              
            }
          # Rewrite region codes
            if(loc=="AP"){df$Loc <- "TU"}
            if(loc=="PAP"){df$Loc <- "ML"}
            if(loc=="PP"){df$Loc <- "PP"}
            if(loc=="BB"){df$Loc <- "RM"}
          # Return data
            return(df)
        }

      # Proportion of total fluvial C fluxes by region 
        prop_fluxes_by_region <- function(loc){
           # Subset data
            df_sub <- droplevels(subset(necb_df, necb_df$Loc==loc))
          # Prepare dataframe
            df <- as.data.frame(matrix(nrow=6, ncol=5))
            names(df) <- c("Loc","Cspec","Mean","Min","Max")
            j=1
          # Store data
            df[j:6,1] <- loc
            df[j:6,2] <- c("JCO2","JCH4","DIC","PIC","DOC","POC")
            df[j,3] <- mean(na.omit(df_sub$jCO2kgCd_mean/df_sub$AQkgCd_mean)) # JCO2 (mean)
            df[j,4] <- mean(na.omit(df_sub$jCO2kgCd_min/df_sub$AQkgCd_min)) # JCO2 (min)
            df[j,5] <- mean(na.omit(df_sub$jCO2kgCd_max/df_sub$AQkgCd_max)) # JCO2 (max)
            df[j+1,3] <- mean(na.omit(df_sub$jCH4kgCd_mean/df_sub$AQkgCd_mean))
            df[j+1,4] <- mean(na.omit(df_sub$jCH4kgCd_min/df_sub$AQkgCd_min))
            df[j+1,5] <- mean(na.omit(df_sub$jCH4kgCd_max/df_sub$AQkgCd_max))
            df[j+2,3] <- mean(na.omit(df_sub$DICkgCd/df_sub$AQkgCd_mean))
            df[j+2,4] <- mean(na.omit(df_sub$DICkgCd/df_sub$AQkgCd_min))
            df[j+2,5] <- mean(na.omit(df_sub$DICkgCd/df_sub$AQkgCd_max))
            df[j+3,3] <- mean(na.omit(df_sub$PICkgCd/df_sub$AQkgCd_mean))
            df[j+3,4] <- mean(na.omit(df_sub$PICkgCd/df_sub$AQkgCd_min))
            df[j+3,5] <- mean(na.omit(df_sub$PICkgCd/df_sub$AQkgCd_max))
            df[j+4,3] <- mean(na.omit(df_sub$DOCkgCd/df_sub$AQkgCd_mean))
            df[j+4,4] <- mean(na.omit(df_sub$DOCkgCd/df_sub$AQkgCd_min))
            df[j+4,5] <- mean(na.omit(df_sub$DOCkgCd/df_sub$AQkgCd_max))
            df[j+5,3] <- mean(na.omit(df_sub$POCkgCd/df_sub$AQkgCd_mean))
            df[j+5,4] <- mean(na.omit(df_sub$POCkgCd/df_sub$AQkgCd_min))
            df[j+5,5] <- mean(na.omit(df_sub$POCkgCd/df_sub$AQkgCd_max))
          # Rewrite region codes
            if(loc=="AP"){df$Loc <- "TU"}
            if(loc=="PAP"){df$Loc <- "ML"}
            if(loc=="PP"){df$Loc <- "PP"}
            if(loc=="BB"){df$Loc <- "RM"}
          # Return data
            return(df)
        }
  
  
  # DATA PROCESSING AND ANALYSES ####
    
    # Proportion of total fluvial C fluxes by site & region
      prop_fluxes_mins <- rbind(prop_fluxes("AP","min","yes"), prop_fluxes("PAP","min","yes"), prop_fluxes("PP","min","yes"), prop_fluxes("BB","min","yes"))
      prop_fluxes_means <- rbind(prop_fluxes("AP","mean","yes"), prop_fluxes("PAP","mean","yes"), prop_fluxes("PP","mean","yes"), prop_fluxes("BB","mean","yes"))
      prop_fluxes_maxs <- rbind(prop_fluxes("AP","max","yes"), prop_fluxes("PAP","max","yes"), prop_fluxes("PP","max","yes"), prop_fluxes("BB","max","yes"))
      #write.csv(prop_fluxes_means, "/Users/szolkos/Desktop/prop_fluxes_means.csv", row.names=F)
      #write.csv(prop_fluxes_mins, "/Users/szolkos/Desktop/prop_fluxes_mins.csv", row.names=F)
      #write.csv(prop_fluxes_maxs, "/Users/szolkos/Desktop/prop_fluxes_maxs.csv", row.names=F)
        
    # Proportion of total fluvial C fluxes by region
      # Explore for one region
        prop_fluxes_by_region("BB")
      # Combine data for all sites
        prop_flux_df <- rbind(prop_fluxes_by_region("BB"),prop_fluxes_by_region("PP"),prop_fluxes_by_region("PAP"),prop_fluxes_by_region("AP"))
      # Regions and C species as factors
        prop_flux_df$Loc <- factor(prop_flux_df$Loc, levels=c("RM","PP","ML","TU"))
        prop_flux_df$Cspec <- factor(prop_flux_df$Cspec, levels=c("JCO2","JCH4","DIC","PIC","DOC","POC"))
  
    # Set plotting parameters
      x_labels <- paste0(c("Richardson","Peel","Mackenzie","Travaillant"),"\n",c("Mountains","Plateau","Lowlands","Uplands"))
      
    # Fluxes as proportion of regional total- export as 4"x5" landscape PDF
      ggplot(prop_flux_df, aes(x=Loc, y=Mean, ymin=Min, ymax=Max)) +
        # Set axis limits
          scale_y_continuous(limits=c(0,0.8), breaks=seq(0,0.8,0.2)) +
        # Add reference lines
          geom_hline(yintercept=0, lty=1, lwd=0.4, col="black") +
        # Add point ranges
          geom_pointrange(aes(fill=Cspec, shape=Cspec), size=0.9, stroke=0.8, position=position_dodge(width=0.6), color="black") +
        # Set panel theme
          theme_bw() +
            theme(plot.margin=unit(c(0.1,0.1,0,0.2), "in"),
                  panel.grid.minor=element_blank(),
                  panel.grid.major=element_blank(),
                  plot.background=element_rect(colour="white", size=1),
                  panel.border=element_rect(colour="black", fill=NA, size=0.8),
                  text=element_text(size=13)) +
            theme(axis.title.x=element_blank()) +
            theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
            theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
            theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
            theme(legend.position="none") +
            theme(plot.background=element_rect(fill='white')) +
            theme(panel.background=element_rect(fill='white')) +
          # Specify colors and shapes of data points
            scale_fill_manual(prop_flux_df$Cspec, values=c("white","grey80","grey60","grey40","grey20","black")) +
            scale_shape_manual(prop_flux_df$Loc, values=rep(c(21,22,23,24,25,19),4)) +
          # Add axis labels
            labs(y="Flux (proportion of regional total)") +
            scale_x_discrete(labels=x_labels)

    
  # OTHER ####
    
    # Proportions of total fluvial C flux by C species
      #x_labels <- c(expression(J[CO2]),expression(J[CH4]),expression(CO[2]),expression(HCO[3]^"-"),"PIC","DOC","POC")
      x_labels <- c(expression(J[CO2]),expression(J[CH4]),"DIC","PIC","DOC","POC")
    
      ggplot(c5flxerr, aes(x=Cspec, y=Mean, ymin=Min, ymax=Max)) +
        # Set axis limits
          scale_y_continuous(limits=c(0,0.8), breaks=seq(0,0.8,0.2)) +
        # Add reference lines
          geom_hline(yintercept=0, lty=1, lwd=0.4, col="black") +
        # Add point ranges
          geom_pointrange(aes(fill=Loc, shape=Loc), size=0.9, stroke=0.8, position=position_dodge(width=0.6), color="black") +
        # Set panel theme
          theme_bw() +
          theme(plot.margin=unit(c(0.1,0.1,0,0.2), "in"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                plot.background=element_rect(colour="white", size=1),
                panel.border=element_rect(colour="black", fill=NA, size=0.8),
                text=element_text(size=13)) +
          theme(axis.title.x=element_blank()) +
          theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
          theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
          theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
          theme(legend.position="none") +
          theme(plot.background=element_rect(fill='white')) +
          theme(panel.background=element_rect(fill='white')) +
        # Specify colors and shapes of data points
          scale_fill_manual(c5flxerr$Cspec, values=rep(regional_colors,4)) +
          scale_shape_manual(c5flxerr$Loc, values=c(22,23,25,21)) +
        # Add axis labels
          labs(y="Proportion of total fluvial C flux") +
          scale_x_discrete(labels=x_labels)
    