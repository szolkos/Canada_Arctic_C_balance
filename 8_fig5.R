#========================================================================================================#
# 8_fig5.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Perform RDA, plot results
#========================================================================================================#

  # BACKGROUND ####

    # No units, functions, or other pertinent background information is summarized here

    
  # DATA PROCESSING AND ANALYSES ####

    # Subset variables
      c5mvs <- droplevels(na.omit(subset(c5means,
                                          select=c("Loc","Site","MeanEl","MeanGPP","SOCC100cm","MeanSlope",
                                                   "LakesPondpcnt","ColluvialPcnt","FluvialPcnt","MorainePcnt","OrganicPcnt","CarbonatePcnt","dexcess",
                                                   "jCO2","jCH4","DICyld","PICyld","DOCyld","POCyld","Runoff",
                                                   "Temp","Cond","DOpcnt","pH","TDNyld","ShedAream2"))))
      
    # Log-transform chemistry variables as needed, to improve normality
      c5mvs$jCO2 <- log(c5mvs$jCO2)
      c5mvs$jCH4 <- log(c5mvs$jCH4 + abs(min(c5mvs$jCH4)) + 1)
      c5mvs$DICyld <- log(c5mvs$DICyld)
      c5mvs$PICyld <- log(c5mvs$PICyld)
      c5mvs$DOCyld <- log(c5mvs$DOCyld)
      c5mvs$POCyld <- log(c5mvs$POCyld)
      c5mvs$Runoff <- log(c5mvs$Runoff)
      
    # Log-transform landscape variables as needed, to improve normality; GPP and dexcess inspected, not transformed
      c5mvs$Temp <- sqrt(c5mvs$Temp)
      c5mvs$Cond <- log(c5mvs$Cond)
      c5mvs$DOpcnt <- (c5mvs$DOpcnt)
      c5mvs$pH <- (c5mvs$pH)
      c5mvs$TDNyld <- log(c5mvs$TDNyld)
      c5mvs$MeanEl <- log(c5mvs$MeanEl)
      c5mvs$SOCC100cm <- log(c5mvs$SOCC100cm)
      c5mvs$MeanSlope <- log(c5mvs$MeanSlope)
      c5mvs$ShedAream2 <- log(c5mvs$ShedAream2)
      c5mvs$ColluvialPcnt <- c5mvs$ColluvialPcnt+0.001; c5mvs$ColluvialPcnt <- log(c5mvs$ColluvialPcnt)
      c5mvs$FluvialPcnt <- c5mvs$FluvialPcnt+0.001; c5mvs$FluvialPcnt <- log(c5mvs$FluvialPcnt)
      c5mvs$MorainePcnt <- c5mvs$MorainePcnt+0.001; c5mvs$MorainePcnt <- log(c5mvs$MorainePcnt)
      c5mvs$OrganicPcnt <- c5mvs$OrganicPcnt+0.001; c5mvs$OrganicPcnt <- log(c5mvs$OrganicPcnt)
      c5mvs$CarbonatePcnt <- c5mvs$CarbonatePcnt+0.001; c5mvs$CarbonatePcnt <- log(c5mvs$CarbonatePcnt)
      c5mvs$LakesPondpcnt <- c5mvs$LakesPondpcnt+0.001; c5mvs$LakesPondpcnt <- log(c5mvs$LakesPondpcnt)
      
    # Set sampling locations as factors
      c5mvs$Loc <- factor(c5mvs$Loc, levels=c("BB","PP","PAP","AP"))
      c5mvs$SiteID <- paste0(c5mvs$Loc,"-",c5mvs$Site)
    # Subset chemistry and landscape variables
      c5mvs_chem <- droplevels(subset(c5mvs, select=c("jCO2","jCH4","DICyld","PICyld","DOCyld","POCyld")))
      c5mvs_landscape <- droplevels(subset(c5mvs, select=c("Loc","Site","dexcess","Temp","Cond","DOpcnt","pH","Runoff","TDNyld","MeanEl","MeanSlope","MeanGPP","SOCC100cm","ColluvialPcnt","FluvialPcnt","MorainePcnt","OrganicPcnt","CarbonatePcnt","LakesPondpcnt","ShedAream2")))
    
    # Run RDA
      c5rda <- rda(c5mvs_chem ~ Temp + Cond + DOpcnt + pH + dexcess + TDNyld + MeanSlope + MeanGPP + SOCC100cm +  CarbonatePcnt + ColluvialPcnt + MorainePcnt + OrganicPcnt + LakesPondpcnt, c5mvs_landscape) #Runoff ShedAream2

    # Run ordistep
      ordistep(c5rda, permutations=5000, direction="backward") # https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/ordistep
      
    # Store trimmed RDA
      c5rda <- rda(c5mvs_chem ~ Cond + DOpcnt + pH + dexcess + TDNyld + MeanSlope + MeanGPP + SOCC100cm + CarbonatePcnt + OrganicPcnt + LakesPondpcnt, c5mvs_landscape)
      
    # Plot it, summarize
      #par(mar=c(4.5,4.5,1,1)); plot(c5rda)
      #summary(c5rda)
      #scores(c5rda)$sites # '$sites' or '$species'
      #summary(c5rda)[3] # constraints
      #summary(c5rda)[4] # biplot
      anova(c5rda, by="term")#, permutations=500) # determine significance of all explanatory terms in RDA
      anova(c5rda, by="axis")#, permutations=500) # determine significance of each RDA axis
      anova(c5rda, by="mar")#, permutations=500) # determine significance of marginal effects in RDA; i.e., what is the single best explanatory variable, if only one is used? (Zuur et al. 2007)
    
    # Optionally export summaries, to create summary tables
      #write.csv(anova(c5rda, by="term")[1:4], "/Users/szolkos/Desktop/RDA_terms.csv", row.names=T)
      #write.csv(anova(c5rda, by="axis")[1:4], "/Users/szolkos/Desktop/RDA_axes.csv", row.names=T)
      
    # Extract biplot scores for RDA1 and RDA2 as matrix
      biplot_scores <- as.data.frame(summary(c5rda)[[4]][,1:2])
      #rownames(biplot_scores) <- c("Temperature","TDN","Slope","GPP","Colluvial","Carbonate")
      biplot_scores$Parameter <- rownames(biplot_scores)
      
    # Plot it- export as 4" x 5" landscape PDF
      par(mar=c(4.5,4.5,1,1))
      plot(c5rda, cex.axis=1.1, cex.lab=1.2, type="n", ylim=c(-1.1,2.4), xlim=c(-1.1,2.4))
      # NOTE: below, use "scores(c5rda)$sites[,1]*-1" to invert points
      points(x=scores(c5rda)$sites[,1]*-1, y=scores(c5rda)$sites[,2], cex=2.5, lwd=2, asp=1, col=c("black"), pch=c(22,23,25,21)[c5mvs$Loc], bg=regional_colors[c5mvs$Loc], xlab="RDA1", ylab="RDA2", cex.axis=1.2, cex.lab=1.5) # Colored biplot
      text(x=scores(c5rda)$sites[,1]*-1, y=scores(c5rda)$sites[,2], pos=4, labels=c5mvs$Site)
      text(x=scores(c5rda)$species[,1]*-1, y=scores(c5rda)$species[,2], labels=rownames(scores(c5rda)$species), cex=0.8, pos=3, col="black")
      #text(x=scores(c5rda)$sites[,1], y=scores(c5rda)$sites[,2], labels=c5mvs$Loc, cex=0.7, pos=3)
      text(x=biplot_scores$RDA1*-1, y=biplot_scores$RDA2, labels=rownames(biplot_scores), cex=0.7, pos=3, col="blue")
      
    # Plot it w/ ggplot, export as 4"x5" landscape PDF
      # Subset data
      #c5rda_df <- cbind(c5mvs[1], scores(c5rda)$sites)
      c5rda_df <- cbind(c5mvs[1], scores(c5rda)$sites[,1]*-1, scores(c5rda)$sites[,2])
      names(c5rda_df) <- c("Loc","RDA1","RDA2")
      c5rda_df$Loc <- factor(c5rda_df$Loc, levels=c("BB","PP","PAP","AP"))
      c5rda_df_ap <- droplevels(subset(c5rda_df, c5rda_df$Loc=="AP"))
      c5rda_df_pap <- droplevels(subset(c5rda_df, c5rda_df$Loc=="PAP"))
      c5rda_df_pp <- droplevels(subset(c5rda_df, c5rda_df$Loc=="PP"))
      c5rda_df_bb <- droplevels(subset(c5rda_df, c5rda_df$Loc=="BB"))
      
      ggplot() +
        # Set axis limits
          scale_y_continuous(limits=c(-1.1,2.4), breaks=seq(-1,2,1)) +
          scale_x_continuous(limits=c(-1.1,2.4), breaks=seq(-1,3,1)) +
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
          ylab("RDA2") +
          xlab("RDA1") +
        # Add horizontal and vertical reference lines
          geom_hline(yintercept=0, lty=2, lwd=0.8, col="darkgray") +
          geom_vline(xintercept=0, lty=2, lwd=0.8, col="darkgray") +
        # Add data points
          geom_point(data=c5rda_df_ap, aes(x=RDA1, y=RDA2, color="black"), shape=21, fill="orange", size=5, stroke=1) +
          geom_point(data=c5rda_df_pap, aes(x=RDA1, y=RDA2, color="black"), shape=25, fill="green3", size=5, stroke=1) +
          geom_point(data=c5rda_df_pp, aes(x=RDA1, y=RDA2, color="black"), shape=23, fill="purple", size=5, stroke=1) +
          geom_point(data=c5rda_df_bb, aes(x=RDA1, y=RDA2, color="black"), shape=22, fill="blue", size=5, stroke=1) +
          scale_color_manual(values=rep(c("black"),15))
        # Add text
          geom_text(aes(x=scores(c5rda)$species[,1]*-1, y=scores(c5rda)$species[,2], label=rownames(scores(c5rda)$species)), size=4, col="black") +
          geom_text(aes(x=biplot_scores$RDA1*-1, y=biplot_scores$RDA2, label=rownames(biplot_scores)), size=3.5, col="blue")
          