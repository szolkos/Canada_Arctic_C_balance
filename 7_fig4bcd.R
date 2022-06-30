#========================================================================================================#
# 7_fig4bcd.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Plot C yields vs. runoff by study region
#========================================================================================================#

  # BACKGROUND ####
  
    # Parameters and units ####
      # C yields: µmol/m2/d
      # Runoff: mm/d
    
      
  # DATA PROCESSING AND ANALYSES ####
      
    # Yields vs. Runoff for DIC, DOC, POC (scatterplots)

        yld_sp <- droplevels(subset(c5means, select=c("Loc","Site","Runoff","Runoffse","POCyld","POCyldse")))
        names(yld_sp) <- c("Loc","Site","Runoff","Runoffse","yld","yldse")
        yld_sp$yld[yld_sp$yld=="NaN"] <- "NA"; yld_sp$yld <- as.numeric(yld_sp$yld)
        yld_sp$Loc <- factor(yld_sp$Loc, levels=c("BB","PP","PAP","AP"))
        
      # Export as 4x5" landscape PDF
        ggplot(data=yld_sp, aes(y=yld, x=Runoff, fill=Loc, shape=Loc)) +
          # Scales
            #scale_y_log10(limits=c(40,25000), breaks=c(10,100,1000,10000), labels=NotFancy) + # for DIC, DOC
            scale_y_log10(limits=c(1,25000), breaks=c(1,10,100,1000,10000), labels=NotFancy) + # for POC
            scale_x_log10(limits=c(0.05,10), breaks=c(0.1,0.1,1,10,100), labels=NotFancy) + # for DIC, DOC, & POC
          # Themes
            theme_bw() +
            theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                  panel.grid.minor=element_blank(),
                  panel.grid.major=element_blank(),
                  plot.background=element_rect(colour="white", size=1),
                  panel.border=element_rect(colour="black", fill=NA, size=1),
                  text=element_text(size=13)) +
            theme(axis.text.y=element_text(size=14, angle=0, hjust=1, colour="black")) +
            theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
            theme(axis.text.x=element_text(size=14, angle=0, hjust=0.5, colour="black")) +
            theme(axis.title.x=element_text(margin=margin(t=0, r=0, b=-5, l=0))) +
            theme(legend.position="none") +
            theme(plot.background=element_rect(fill='white')) +
            theme(panel.background=element_rect(fill='white')) +
            annotation_logticks() + # annotation_logticks(outside=T) + coord_cartesian(clip = "off")
          # Points
            geom_errorbarh(data=yld_sp, aes(x=Runoff, y=yld, xmin=Runoff-Runoffse, xmax=Runoff+Runoffse), color=regional_colors[yld_sp$Loc], height=0, position=position_dodge()) +
            geom_errorbar(data=yld_sp, aes(x=Runoff, y=yld, ymin=yld-yldse, ymax=yld+yldse), color=regional_colors[yld_sp$Loc], width=0, position=position_dodge()) +
            geom_point(data=yld_sp, aes(y=yld, x=Runoff), pch=c(22,23,25,21)[yld_sp$Loc], fill=regional_colors[yld_sp$Loc], col="black", size=4, stroke=1, alpha=1) +
          # Labels
            labs(y=expression(POC~yield~(µmol~m^-2~d^-1)), x=expression(Runoff~(mm~d^-1)))
        
        
  # Statistics ####
        
      # Set plotting parameters
        par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
        
      # CO2yld
        plot((c5means$CO2yld)~(c5means$Runoff), ylim=c(10,15000), xlim=c(0.005,0.80), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        text(y=c5means$CO2yld, x=c5means$Runoff, labels=c5means$Site, col="white")
      # Linear model
        par(mar=c(4.5,4.5,1,1), mfrow=c(2,2))
        plot(lm(log(c5means$CO2yld)~log(c5means$Runoff))); summary(lm(log(c5means$CO2yld)~log(c5means$Runoff)))
        
      # HCO3yld
        plot((c5means$HCO3yld)~(c5means$Runoff), ylim=c(10,15000), xlim=c(0.005,0.80), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        text(y=c5means$HCO3yld, x=c5means$Runoff, labels=c5means$Site, col="white")
      # Linear models
      # All sites
        plot(lm(log(c5means$HCO3yld)~log(c5means$Runoff))); summary(lm(log(c5means$HCO3yld)~log(c5means$Runoff)))
        summary(lmer(log(HCO3yld)~log(Runoff) + (1|Loc), data=c5means))
      # HCO3yld - AP (all sites)
        plot(lm(log(c5means$HCO3yld[c5means$Loc=="AP"])~log(c5means$Runoff[c5means$Loc=="AP"]))); summary(lm(log(c5means$HCO3yld[c5means$Loc=="AP"])~log(c5means$Runoff[c5means$Loc=="AP"])))
      # HCO3yld - PAP (all sites)
        plot(lm(log(c5means$HCO3yld[c5means$Loc=="PAP"])~log(c5means$Runoff[c5means$Loc=="PAP"]))); summary(lm(log(c5means$HCO3yld[c5means$Loc=="PAP"])~log(c5means$Runoff[c5means$Loc=="PAP"])))
      # HCO3yld - PP (all sites)
        #plot(lm(log(c5means$HCO3yld[c5means$Loc=="PP"])~log(c5means$Runoff[c5means$Loc=="PP"]))); summary(lm(log(c5means$HCO3yld[c5means$Loc=="PP"])~log(c5means$Runoff[c5means$Loc=="PP"])))
        ## HCO3yld - PP without sites 1 or 8
        lm(subset(c5means, select=c("Loc","Site","HCO3yld"), c5means$Loc=="PP" & c5means$Site!="1" & c5means$Site!="8")$HCO3yld~subset(c5means, select=c("Loc","Site","Runoff"), c5means$Loc=="PP" & c5means$Site!="1" & c5means$Site!="8")$Runoff)
        PPylds_lm_no6or8 <- lm(subset(c5means, select=c("Loc","Site","HCO3yld"), c5means$Loc=="PP" & c5means$Site!="1" & c5means$Site!="8")$HCO3yld~subset(c5means, select=c("Loc","Site","Runoff"), c5means$Loc=="PP" & c5means$Site!="1" & c5means$Site!="8")$Runoff)
        plot(PPylds_lm_no6or8)
        summary(PPylds_lm_no6or8)
      # HCO3yld - BB (all sites)
        #plot(lm(log(c5means$HCO3yld[c5means$Loc=="BB"])~log(c5means$Runoff[c5means$Loc=="BB"]))); summary(lm(log(c5means$HCO3yld[c5means$Loc=="BB"])~log(c5means$Runoff[c5means$Loc=="BB"])))
      # HCO3yld - BB without sites 6, 7, or 8
        BBylds_lm_no678 <- lm(subset(c5means, select=c("Loc","Site","HCO3yld"), c5means$Loc=="BB" & c5means$Site!="6" & c5means$Site!="7" & c5means$Site!="8")$HCO3yld~subset(c5means, select=c("Loc","Site","Runoff"), c5means$Loc=="BB" & c5means$Site!="6" & c5means$Site!="7" & c5means$Site!="8")$Runoff)
        plot(BBylds_lm_no678)
        summary(BBylds_lm_no678)
        
      # DOCyld
        plot((c5means$DOCyld)~(c5means$Runoff), ylim=c(10,15000), xlim=c(0.005,0.80), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        text(y=c5means$DOCyld, x=c5means$Runoff, labels=c5means$Site, col="white")
      # Linear models
      # All sites
        plot(lm(log(c5means$DOCyld)~log(c5means$Runoff))); summary(lm(log(c5means$DOCyld)~log(c5means$Runoff)))
      # DOCyld - AP (all sites)
        plot(lm(log(c5means$DOCyld[c5means$Loc=="AP"])~log(c5means$Runoff[c5means$Loc=="AP"]))); summary(lm(log(c5means$DOCyld[c5means$Loc=="AP"])~log(c5means$Runoff[c5means$Loc=="AP"])))
      # DOCyld - PAP (all sites)
        plot(lm(log(c5means$DOCyld[c5means$Loc=="PAP"])~log(c5means$Runoff[c5means$Loc=="PAP"]))); summary(lm(log(c5means$DOCyld[c5means$Loc=="PAP"])~log(c5means$Runoff[c5means$Loc=="PAP"])))
      # DOCyld - PP (all sites)
        #plot(lm(log(c5means$DOCyld[c5means$Loc=="PP"])~log(c5means$Runoff[c5means$Loc=="PP"]))); summary(lm(log(c5means$DOCyld[c5means$Loc=="PP"])~log(c5means$Runoff[c5means$Loc=="PP"])))
      # DOCyld - PP without site 1
        PPylds_lm_no1 <- lm(subset(c5means, select=c("Loc","Site","DOCyld"), c5means$Loc=="PP" & c5means$Site!="1")$DOCyld~subset(c5means, select=c("Loc","Site","Runoff"), c5means$Loc=="PP" & c5means$Site!="1")$Runoff)
        plot(PPylds_lm_no1)
        summary(PPylds_lm_no1)
      # DOCyld - BB (all sites)
        plot(lm(log(c5means$DOCyld[c5means$Loc=="BB"])~log(c5means$Runoff[c5means$Loc=="BB"]))); summary(lm(log(c5means$DOCyld[c5means$Loc=="BB"])~log(c5means$Runoff[c5means$Loc=="BB"])))
        
      # POCyld
        plot((c5means$POCyld)~(c5means$Runoff), ylim=c(10,15000), xlim=c(0.005,0.80), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        text(y=c5means$POCyld, x=c5means$Runoff, labels=c5means$Site, col="white")
        # Linear models
        plot(lm(log(c5means$POCyld)~log(c5means$Runoff))); summary(lm(log(c5means$POCyld)~log(c5means$Runoff)))
      ## Other:
        #plot((c5means$DICyld)~(c5means$Runoff), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        #plot((c5means$PICyld)~(c5means$Runoff), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        
      # 
        plot((c5means$POCyld)~(c5means$Runoff), ylim=c(10,15000), xlim=c(0.005,0.80), log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
        
        plot((c5df$Cayld)~(c5df$Runoff), log="xy", pch=c(22,23,25,21)[c5df$Loc], col="black", bg=regional_colors[c5df$Loc], lwd=2, cex=1.6)
        
        
      # Linear models of C yields vs runoff
        CO2_lm_ap <- drop.levels(subset(c5means, select=c("Loc","Site","CO2yld","Runoff"), c5means=="AP"))
        CO2_lm_pap <- drop.levels(subset(c5means, select=c("Loc","Site","CO2yld","Runoff"), c5means=="PAP"))
        CO2_lm_pp <- drop.levels(subset(c5means, select=c("Loc","Site","CO2yld","Runoff"), c5means=="PP"))
        CO2_lm_bb <- drop.levels(subset(c5means, select=c("Loc","Site","CO2yld","Runoff"), c5means=="BB"))
        par(mfrow=c(2,2))
        plot((CO2_lm_ap$CO2yld) ~ (CO2_lm_ap$Runoff), log="y"); plot((CO2_lm_pap$CO2yld) ~ (CO2_lm_pap$Runoff), log="y")
        plot((CO2_lm_pp$CO2yld) ~ (CO2_lm_pp$Runoff), log="y"); plot((CO2_lm_bb$CO2yld) ~ (CO2_lm_bb$Runoff), log="y")
        par(mfrow=c(2,2))
        plot(lm(log(CO2_lm_ap$CO2yld) ~ (CO2_lm_ap$Runoff))); summary(lm(log(CO2_lm_ap$CO2yld) ~ (CO2_lm_ap$Runoff)))
        plot(lm(log(CO2_lm_pap$CO2yld) ~ (CO2_lm_pap$Runoff))); summary(lm(log(CO2_lm_pap$CO2yld) ~ (CO2_lm_pap$Runoff)))
        plot(lm(log(CO2_lm_pp$CO2yld) ~ (CO2_lm_pp$Runoff))); summary(lm(log(CO2_lm_pp$CO2yld) ~ (CO2_lm_pp$Runoff)))
        plot(lm(log(CO2_lm_bb$CO2yld) ~ (CO2_lm_bb$Runoff))); summary(lm(log(CO2_lm_bb$CO2yld) ~ (CO2_lm_bb$Runoff)))
        
        
  # OTHER ####

      yld_plot <- function(loc){
        # Subset data
          df_sub <- droplevels(subset(c5means, c5means$Loc==loc))
        # Prepare dataframe
          names <- c("Loc","Cspec","Mean","SE")
          cspec <- c("DIC","PIC","DOC","POC")
          df <- as.data.frame(matrix(nrow=length(cspec), ncol=length(names)))
          names(df) <- names
          j=1
        # Compile data
          df[j:4,1] <- loc
          df[j:4,2] <- cspec
          df[j,3] <- mean(na.omit(df_sub$DICyld)) # DIC mean
          df[j,4] <- mean(na.omit(df_sub$DICyldse)) # DIC SE
          df[j+1,3] <- mean(na.omit(df_sub$PICyld)) # PIC mean
          df[j+1,4] <- mean(na.omit(df_sub$PICyldse)) # PIC SE
          df[j+2,3] <- mean(na.omit(df_sub$DOCyld)) # DOC mean
          df[j+2,4] <- mean(na.omit(df_sub$DOCyldse)) # DOC SE
          df[j+3,3] <- mean(na.omit(df_sub$POCyld)) # POC mean
          df[j+3,4] <- mean(na.omit(df_sub$POCyldse)) # POC SE
        # Rewrite region codes
          if(loc=="AP"){df$Loc <- "TU"}
          if(loc=="PAP"){df$Loc <- "ML"}
          if(loc=="PP"){df$Loc <- "PP"}
          if(loc=="BB"){df$Loc <- "RM"}
        # Return data
          return(df)
      }
        
    # Proportion of total fluvial C fluxes
      # Explore for one region
        yld_plot("BB")
      # Combine data for all sites
        yld_plot_df <- rbind(yld_plot("BB"),yld_plot("PP"),yld_plot("PAP"),yld_plot("AP"))
      # Regions and C species as factors
        yld_plot_df$Loc <- factor(yld_plot_df$Loc, levels=c("RM","PP","ML","TU"))
        yld_plot_df$Cspec <- factor(yld_plot_df$Cspec, levels=c("DIC","PIC","DOC","POC"))
      

  # Compile C species concentrations, yields, and standard error for each, for C species concentration and yield geom_pointrange plots
    conc_plt <- droplevels(subset(c5means, select=c("Loc","Site","CO2uM","HCO3uM","DICuM","PICuM","DOCuM","POCuM")))
    conc_plt[conc_plt=="NaN"] <- NA
    conc_plt$Loc <- factor(conc_plt$Loc, levels=c("AP","BB","PAP","PP"))
    yld_plt <- drop.levels(subset(c5means, select=c("Loc","Site","CO2yld","HCO3yld","DICyld","PICyld","DOCyld","POCyld")))
    yld_plt[yld_plt=="NaN"] <- NA
    yld_plt$Loc <- factor(yld_plt$Loc, levels=c("AP","BB","PAP","PP"))
    
  # Concentrations
    {
      conc_plt_df <- as.data.frame(matrix(nrow=10,ncol=5))
      names(conc_plt_df) <- c("Cspec","AP","BB","PAP","PP")
      conc_plt_df[1,1] <- "CO2uM"
      conc_plt_df[1,2:5] <- round(tapply(conc_plt$CO2uM, conc_plt$Loc, FUN=mean, na.rm=T),4)
      conc_plt_df[2,1] <- "HCO3uM"
      conc_plt_df[2,2:5] <- round(tapply(conc_plt$HCO3uM, conc_plt$Loc, FUN=mean, na.rm=T),4)
      conc_plt_df[3,1] <- "PICuM"
      conc_plt_df[3,2:5] <- round(tapply(conc_plt$PICuM, conc_plt$Loc, FUN=mean, na.rm=T),4)
      conc_plt_df[4,1] <- "DOCuM"
      conc_plt_df[4,2:5] <- round(tapply(conc_plt$DOCuM, conc_plt$Loc, FUN=mean, na.rm=T),4)
      conc_plt_df[5,1] <- "POCuM"
      conc_plt_df[5,2:5] <- round(tapply(conc_plt$POCuM, conc_plt$Loc, FUN=mean, na.rm=T),4)
      conc_plt_df[6,1] <- "CO2uMse"
      conc_plt_df[6,2:5] <- round(tapply(conc_plt$CO2uM, conc_plt$Loc, FUN=std.error, na.rm=T),4)
      conc_plt_df[7,1] <- "HCO3uMse"
      conc_plt_df[7,2:5] <- round(tapply(conc_plt$HCO3uM, conc_plt$Loc, FUN=std.error, na.rm=T),4)
      conc_plt_df[8,1] <- "PICuMse"
      conc_plt_df[8,2:5] <- round(tapply(conc_plt$PICuM, conc_plt$Loc, FUN=std.error, na.rm=T),4)
      conc_plt_df[9,1] <- "DOCuMse"
      conc_plt_df[9,2:5] <- round(tapply(conc_plt$DOCuM, conc_plt$Loc, FUN=std.error, na.rm=T),4)
      conc_plt_df[10,1] <- "POCuMse"
      conc_plt_df[10,2:5] <- round(tapply(conc_plt$POCuM, conc_plt$Loc, FUN=std.error, na.rm=T),4)
      #write.csv(conc_plt_df, "/Users/szolkos/Desktop/conc_plt_df.csv", row.names=F)
      }
  # Yield
    {
      yld_plt_df <- as.data.frame(matrix(nrow=10,ncol=5))
      names(yld_plt_df) <- c("Cspec","AP","BB","PAP","PP")
      yld_plt_df[1,1] <- "CO2yld"
      yld_plt_df[1,2:5] <- round(tapply(yld_plt$CO2yld, yld_plt$Loc, FUN=mean, na.rm=T),4)
      yld_plt_df[2,1] <- "HCO3yld"
      yld_plt_df[2,2:5] <- round(tapply(yld_plt$HCO3yld, yld_plt$Loc, FUN=mean, na.rm=T),4)
      yld_plt_df[3,1] <- "PICyld"
      yld_plt_df[3,2:5] <- round(tapply(yld_plt$PICyld, yld_plt$Loc, FUN=mean, na.rm=T),4)
      yld_plt_df[4,1] <- "DOCyld"
      yld_plt_df[4,2:5] <- round(tapply(yld_plt$DOCyld, yld_plt$Loc, FUN=mean, na.rm=T),4)
      yld_plt_df[5,1] <- "POCyld"
      yld_plt_df[5,2:5] <- round(tapply(yld_plt$POCyld, yld_plt$Loc, FUN=mean, na.rm=T),4)
      yld_plt_df[6,1] <- "CO2yldse"
      yld_plt_df[6,2:5] <- round(tapply(yld_plt$CO2yld, yld_plt$Loc, FUN=std.error, na.rm=T),4)
      yld_plt_df[7,1] <- "HCO3yldse"
      yld_plt_df[7,2:5] <- round(tapply(yld_plt$HCO3yld, yld_plt$Loc, FUN=std.error, na.rm=T),4)
      yld_plt_df[8,1] <- "PICyldse"
      yld_plt_df[8,2:5] <- round(tapply(yld_plt$PICyld, yld_plt$Loc, FUN=std.error, na.rm=T),4)
      yld_plt_df[9,1] <- "DOCyldse"
      yld_plt_df[9,2:5] <- round(tapply(yld_plt$DOCyld, yld_plt$Loc, FUN=std.error, na.rm=T),4)
      yld_plt_df[10,1] <- "POCyldse"
      yld_plt_df[10,2:5] <- round(tapply(yld_plt$POCyld, yld_plt$Loc, FUN=std.error, na.rm=T),4)
      #write.csv(yld_plt_df, "/Users/szolkos/Desktop/yld_plt_df.csv", row.names=F)
    }
  
  # Read in data
    # Concentrations and yields
      concs_ylds_errs <- read.csv(paste0(dir,"Ch5/","Ch5_C_concs_ylds_errs_2020_03_30.csv"), header=T)
      concs_ylds_errs$Loc <- factor(concs_ylds_errs$Loc, levels=c("BB","PP","PAP","AP"))
      concs_ylds_errs$Cspec <- factor(concs_ylds_errs$Cspec, levels=c("CO2","HCO3","PIC","DOC","POC"))
      concs_ylds_errs$Type <- factor(concs_ylds_errs$Type, levels=c("conc","yield"))

  # Set plotting parameters
    constituent_colors_2 <- c("yellow","green3","red","purple","orange","darkred","darkgreen")
    x_labels <- c(expression(J[CO2]),expression(J[CH4]),expression(CO[2]),expression(HCO[3]^"-"),"PIC","DOC","POC")
    
  # Create plot for ABSOLUTE (i.e. not as proportions of total) fluxes and errors
    cy_plot <- drop.levels(subset(concs_ylds_errs, concs_ylds_errs$Type=="yield"))
    #cy_plot2 <- cy_plot; cy_plot2$Mean <- (cy_plot2$Mean/86400)*(10^4); cy_plot2$se <- (cy_plot2$se/86400)*(10^4); cy_plot <- cy_plot2 # for Yields
    cy_plot$Loc <- factor(cy_plot$Loc, levels=c("BB","PP","PAP","AP"))
    cy_plot$Cspec <- factor(cy_plot$Cspec, levels=c("CO2","HCO3","PIC","DOC","POC"))
    
    x_labels <- c(expression(CO[2]),expression(HCO[3]^"-"),"PIC","DOC","POC")
    
  # Plot, export as 4x5" landscape PDF
    ggplot(yld_plot_df, aes(x=Cspec, y=Mean, ymin=Mean-SE, ymax=Mean+SE)) +
      # Set axis limits
        #scale_y_continuous(limits=c(0,0.8), breaks=seq(0,0.8,0.2)) +
        #scale_y_log10(limits=c(2,5000), breaks=c(10,100,1000,5000)) + # for Concentrations
        #scale_y_log10(limits=c(0.3,1300), breaks=c(1,10,100,1000)) + # for instantaneous Yields
        scale_y_log10(limits=c(1,10000), breaks=c(1,10,100,1000,10000)) + # for daily Yields
        #scale_y_continuous() +
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
        theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) + # for Concs
        #theme(axis.title.y=element_text(margin=margin(t=0, r=1, b=0, l=-5))) + # for Yields
        theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
        theme(legend.position="none") +
        theme(plot.background=element_rect(fill='white')) +
        theme(panel.background=element_rect(fill='white')) +
      # Specify colors and shapes of data points
        scale_fill_manual(yld_plot_df$Cspec, values=rep(regional_colors,4)) +
        scale_shape_manual(yld_plot_df$Loc, values=c(22,23,25,21)) +
      # Add axis labels
        #labs(y="Concentration (µM)") +
        #labs(y=expression(Yield~(µmol~m^-2~s^-1~"x"~10^4))) +
        labs(y=expression(Yield~(µmol~m^-2~d^-1))) +
        scale_x_discrete(labels=x_labels)
    
    
  # Prepare data for C species concentrations and yields as PROPORTIONS
    
    # Concentrations-DO NOT PLOT; ONLY PLOT YIELDS
      # Compile data
        conc_df <- drop.levels(subset(c5means, select=c("Loc","Site","CO2uM","HCO3uM","DICuM","PICuM","DOCuM","POCuM","TCuM")))
        conc_df[conc_df=="NaN"] <- NA
        conc_df$CO2concp <- conc_df$CO2uM/conc_df$TCuM
        conc_df$HCO3concp <- conc_df$HCO3uM/conc_df$TCuM
        conc_df$PICconcp <- conc_df$PICuM/conc_df$TCuM
        conc_df$DOCconcp <- conc_df$DOCuM/conc_df$TCuM
        conc_df$POCconcp <- conc_df$POCuM/conc_df$TCuM
        concp_df <- drop.levels(subset(conc_df, select=c("Loc","Site","CO2concp","HCO3concp","PICconcp","DOCconcp","POCconcp")))
        concp_df$Loc <- factor(concp_df$Loc, levels=c("AP","BB","PAP","PP"))
      # Calculate regional means and  standard error
        {
          conc_prop_df <- as.data.frame(matrix(nrow=10,ncol=5))
          names(conc_prop_df) <- c("Cspec","AP","BB","PAP","PP")
          conc_prop_df[1,1] <- "CO2concp"
          conc_prop_df[1,2:5] <- round(tapply(concp_df$CO2concp, concp_df$Loc, FUN=mean, na.rm=T),4)
          conc_prop_df[2,1] <- "HCO3concp"
          conc_prop_df[2,2:5] <- round(tapply(concp_df$HCO3concp, concp_df$Loc, FUN=mean, na.rm=T),4)
          conc_prop_df[3,1] <- "PICconcp"
          conc_prop_df[3,2:5] <- round(tapply(concp_df$PICconcp, concp_df$Loc, FUN=mean, na.rm=T),4)
          conc_prop_df[4,1] <- "DOCconcp"
          conc_prop_df[4,2:5] <- round(tapply(concp_df$DOCconcp, concp_df$Loc, FUN=mean, na.rm=T),4)
          conc_prop_df[5,1] <- "POCconcp"
          conc_prop_df[5,2:5] <- round(tapply(concp_df$POCconcp, concp_df$Loc, FUN=mean, na.rm=T),4)
          conc_prop_df[6,1] <- "CO2concpse"
          conc_prop_df[6,2:5] <- round(tapply(concp_df$CO2concp, concp_df$Loc, FUN=std.error, na.rm=T),4)
          conc_prop_df[7,1] <- "HCO3concpse"
          conc_prop_df[7,2:5] <- round(tapply(concp_df$HCO3concp, concp_df$Loc, FUN=std.error, na.rm=T),4)
          conc_prop_df[8,1] <- "PICconcpse"
          conc_prop_df[8,2:5] <- round(tapply(concp_df$PICconcp, concp_df$Loc, FUN=std.error, na.rm=T),4)
          conc_prop_df[9,1] <- "DOCconcpse"
          conc_prop_df[9,2:5] <- round(tapply(concp_df$DOCconcp, concp_df$Loc, FUN=std.error, na.rm=T),4)
          conc_prop_df[10,1] <- "POCconcpse"
          conc_prop_df[10,2:5] <- round(tapply(concp_df$POCconcp, concp_df$Loc, FUN=std.error, na.rm=T),4)
          #write.csv(conc_prop_df, "/Users/szolkos/Desktop/conc_prop_df.csv", row.names=F)
        }
      # Read in data
        concs_prop <- read.csv(paste0(dir,"Ch5/","Ch5_conc_prop_df.csv"), header=T)
        concs_prop$Loc <- factor(concs_prop$Loc, levels=c("BB","PP","PAP","AP"))
        concs_prop$Cspec <- factor(concs_prop$Cspec, levels=c("CO2","HCO3","PIC","DOC","POC"))
    
    # Yields
      # Compile data
        yld_df <- drop.levels(subset(c5means, select=c("Loc","Site","CO2yld","CH4yld","HCO3yld","DICyld","PICyld","DOCyld","POCyld","TCyld")))
        yld_df[yld_df=="NaN"] <- NA
        yld_df$CO2yldp <- yld_df$CO2yld/yld_df$TCyld
        yld_df$CH4yldp <- yld_df$CH4yld/yld_df$TCyld
        yld_df$HCO3yldp <- yld_df$HCO3yld/yld_df$TCyld
        yld_df$DICyldp <- yld_df$DICyld/yld_df$TCyld
        yld_df$PICyldp <- yld_df$PICyld/yld_df$TCyld
        yld_df$DOCyldp <- yld_df$DOCyld/yld_df$TCyld
        yld_df$POCyldp <- yld_df$POCyld/yld_df$TCyld
        yldp_df <- drop.levels(subset(yld_df, select=c("Loc","Site","CO2yldp","CH4yldp","HCO3yldp","DICyldp","PICyldp","DOCyldp","POCyldp")))
        yldp_df$Loc <- factor(yldp_df$Loc, levels=c("AP","BB","PAP","PP"))
      # Calculate regional means and  standard error
        {
          yld_prop_df <- as.data.frame(matrix(nrow=12,ncol=5))
          names(yld_prop_df) <- c("Cspec","AP","BB","PAP","PP")
          yld_prop_df[1,1] <- "CO2yldp"
          yld_prop_df[1,2:5] <- round(tapply(yldp_df$CO2yldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[2,1] <- "CH4yldp"
          yld_prop_df[2,2:5] <- round(tapply(yldp_df$CH4yldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[3,1] <- "HCO3yldp"
          yld_prop_df[3,2:5] <- round(tapply(yldp_df$HCO3yldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[4,1] <- "DICyldp"
          yld_prop_df[4,2:5] <- round(tapply(yldp_df$DICyldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[5,1] <- "PICyldp"
          yld_prop_df[5,2:5] <- round(tapply(yldp_df$PICyldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[6,1] <- "DOCyldp"
          yld_prop_df[6,2:5] <- round(tapply(yldp_df$DOCyldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[7,1] <- "POCyldp"
          yld_prop_df[7,2:5] <- round(tapply(yldp_df$POCyldp, yldp_df$Loc, FUN=mean, na.rm=T),4)
          yld_prop_df[8,1] <- "CO2yldpse"
          yld_prop_df[8,2:5] <- round(tapply(yldp_df$CO2yldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          yld_prop_df[9,1] <- "CH4yldpse"
          yld_prop_df[9,2:5] <- round(tapply(yldp_df$CH4yldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          yld_prop_df[10,1] <- "HCO3yldpse"
          yld_prop_df[10,2:5] <- round(tapply(yldp_df$HCO3yldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          yld_prop_df[11,1] <- "DICyldpse"
          yld_prop_df[11,2:5] <- round(tapply(yldp_df$DICyldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          yld_prop_df[12,1] <- "PICyldpse"
          yld_prop_df[12,2:5] <- round(tapply(yldp_df$PICyldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          yld_prop_df[13,1] <- "DOCyldpse"
          yld_prop_df[13,2:5] <- round(tapply(yldp_df$DOCyldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          yld_prop_df[14,1] <- "POCyldpse"
          yld_prop_df[14,2:5] <- round(tapply(yldp_df$POCyldp, yldp_df$Loc, FUN=std.error, na.rm=T),4)
          write.csv(yld_prop_df, "/Users/szolkos/Desktop/yld_prop_df.csv", row.names=F)
        }
      # Read in data
        ylds_prop <- read.csv(paste0(dir,"Ch5/","Ch5_yld_prop_df_2021_01_17.csv"), header=T)
        ylds_prop <- drop.levels(subset(ylds_prop, Cspec != "CH4" & Cspec != "CO2" & Cspec != "HCO3"))
        ylds_prop$Loc <- factor(ylds_prop$Loc, levels=c("BB","PP","PAP","AP"))
        ylds_prop$Cspec <- factor(ylds_prop$Cspec, levels=c("DIC","PIC","DOC","POC"))
        
    # Plot it
      ggplot(ylds_prop, aes(x=Cspec, y=Mean, ymin=Mean-se, ymax=Mean+se)) +
        # Set axis limits
          scale_y_continuous(limits=c(0,0.9), breaks=seq(0,0.8,0.2)) +
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
          #theme(axis.title.x=element_blank()) +
          theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
          theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
          theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
          theme(legend.position="none") +
          theme(plot.background=element_rect(fill='white')) +
          theme(panel.background=element_rect(fill='white')) +
        # Specify colors and shapes of data points- discrete symbols by REGION and colors by C SPECIES
          scale_fill_manual(ylds_prop$Cspec, values=rep(regional_colors,4)) + # c("yellow","red","darkred","green3","darkgreen")
        # Specify colors and shapes of data points- discrete symbols by C SPECIES and colors by REGION
          scale_shape_manual(ylds_prop$Loc, values=c(22,23,25,21)) + # c(22,23,24,25)
        # Add axis labels
          labs(y=expression(Yield~(proportion~of~regional~total)), x="") +
          scale_x_discrete(labels=c(as.character(unique(ylds_prop$Cspec)))) # rep(c(as.character(unique(ylds_prop$Cspec))),4) # c("RM","PP","ML","TU")