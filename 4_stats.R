#========================================================================================================#
# 4_stats.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Statistics, as detailed in manuscript Methods
#========================================================================================================#

  # BACKGROUND ####

    # No units, functions, or other pertinent background information to summarize here


  # DATA PROCESSING AND ANALYSES ####

    # Regional & seasonal variation in constituents, using ANOVA & boxplots
      # Confirm that parameters of interested were measured during 
        unique(na.omit(subset(c5df, select=c("SmplPer","TCflux")))$SmplPer)
      # ANOVA to test for differences in desired parameter among sampling periods (1,2,3 = early, mid, late-summer) by region; e.g., see: https://www.scribbr.com/statistics/anova-in-r
        #blocked <- aov( () ~ SmplPer + Loc, data=c5df) # Blocked by sampling region ('Loc'), with no interaction between region and sampling period
        interac <- aov( log(POCflux) ~ SmplPer*Loc, data=c5df) # With interaction between region and sampling period
        #interac <- aov( log(HCO3uM) ~ Loc, data=c5df)
        aov_mdl <- interac # store model for testing
      # Evaluate model
        par(mfrow=c(2,2))
        plot(aov_mdl)
      # Summarize results: if p-value for F-statistic (i.e., Pr(>F)) < 0.05, then significant
        summary(aov_mdl)
        TukeyHSD(aov_mdl)
      # Plot it
        par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
        aov_cols <- c(rep("blue",3),rep("purple",3),rep("green3",3),rep("orange",3))
        boxplot( (c5df$MeanGPP) ~ c5df$SmplPer*c5df$Loc, xlab="", las=2, col=aov_cols)

        
        summary(lmer( log(HCO3uM) ~ 1 + 1|Loc + 1|SmplPer, data=c5df ))
        summary(lmer( log(HCO3uM) ~ 1 + 1|Loc, data=c5means ))
        
      # Test for equal variance; if not equal, use non-parametric
        bartlett.test( log(HCO3uM) ~ Loc, data=c5means)
        
      # Subset data
        pn_df <- na.omit(droplevels(subset(c5means, select=c("Loc","Site","HCO3uM"))))
        pn_df$Site <- factor(pn_df$Site, levels=seq(1,9,1))
      # Run permANOVA
        pn <- aovp( log(HCO3uM) ~ Loc, c5df, seqs=T)
        pn <- aovp( log(HCO3uM) ~ Loc*Site, data=pn_df)
      # Visual inspection of residual distributions- as needed, transform y and re-run aovp
        par(mar=c(4.5,4.5,1,1), mfrow=c(2,2)); plot(pn)
        # transform (ln): Cond, DOCuM, NH4uM, NO3uM, CO2uM, CH4uM, SUVA
      # Test permANOVA
        anova(pn)
        
        
      # Parameters vs mean watershed characteristics (elevation, area) ####
      
        # DOCuM ~ MeanEl
          ## Explore
            par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
            plot(log(c5means$DOCuM)~(c5means$MeanEl), pch=c(22,23,25,21)[c5means$Loc], col="black", ylab=expression(DOC~(µM)), xlab=expression(Mean~elevation~(m)), bg=regional_colors[c5means$Loc], lwd=1.5, cex=1.6)
          ## Build lm
            doc_lm <- lm(log(c5means$DOCuM)~(c5means$MeanEl))
            par(mar=c(2,2,1,1), mfrow=c(2,2))
            plot(doc_lm)
          ## Model summary
            summary(doc_lm)
          ## Finalize model
            predict()
            
        # DONuM ~ MeanEl
          ## Explore
            par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
            plot(log(c5means$DONuM)~(c5means$MeanEl), pch=c(22,23,25,21)[c5means$Loc], col="black", ylab=expression(DON~(µM)), xlab=expression(Mean~elevation~(m)), bg=regional_colors[c5means$Loc], lwd=1.5, cex=1.6)
          ## Build lm
            don_lm <- lm(log(c5means$DONuM)~(c5means$MeanEl))
            par(mar=c(2,2,1,1), mfrow=c(2,2))
            plot(don_lm)
          ## Model summary
            summary(don_lm)
          ## Plot predicted vs observed
            predict()
            
        # Q ~ ShedAream2
          ## Explore
            par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
            plot(c5means$Q~c5means$ShedAream2, log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", ylab=expression(italic(Q)~(m^3~s^-1)), xlab=expression(Watershed~area~(m^2)), bg=regional_colors[c5means$Loc], lwd=1.5, cex=1.6)
          ## Build lm
            q_lm <- lm(log(c5means$Q)~log(c5means$ShedAream2))
            par(mar=c(2,2,1,1), mfrow=c(2,2))
            plot(q_lm)
          ## Model summary
            summary(q_lm)
          ## Plot predicted vs observed
            predict()
            
        # W ~ ShedAream2
          ## Explore
            par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
            plot(c5means$W~c5means$ShedAream2, log="xy", pch=c(22,23,25,21)[c5means$Loc], col="black", bg=regional_colors[c5means$Loc], lwd=2, cex=1.6)
          ## Build lm
            w_lm <- lm(log(c5means$W)~log(c5means$ShedAream2))
            par(mar=c(2,2,1,1), mfrow=c(2,2))
          ## Model summary
            plot(w_lm)
            summary(w_lm)
          ## Plot predicted vs observed
            predict()
            
      # pCH4 vs. NO3uM
          ## Explore
            par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
            plot(log(c5means$pCH4) ~ log(c5means$NO3uM), pch=c(22,23,25,21)[c5means$Loc], col="black", ylab=expression(italic(p)*CH[4]~(µatm)), xlab=expression(NO[3]^"-"), bg=regional_colors[c5means$Loc], lwd=1.5, cex=1.6)
          ## Build lm
            pch_no3_lm <- lm( log(c5means$pCH4) ~ log(c5means$NO3uM))
            par(mar=c(2,2,1,1), mfrow=c(2,2))
            plot(pch_no3_lm)
          ## Model summary
            summary(pch_no3_lm)
            
      # pCH4 vs. SO4uM
          ## Explore
            par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))
            plot(log(c5means$pCH4) ~ log(c5means$SO4uM), pch=c(22,23,25,21)[c5means$Loc], col="black", ylab=expression(italic(p)*CH[4]~(µatm)), xlab=expression(SO[4]^"2-"), bg=regional_colors[c5means$Loc], lwd=1.5, cex=1.6)
          ## Build lm
            pch_so4_lm <- lm( log(c5means$pCH4) ~ log(c5means$SO4uM))
            par(mar=c(2,2,1,1), mfrow=c(2,2))
            plot(pch_so4_lm)
          ## Model summary
            summary(pch_so4_lm)
    