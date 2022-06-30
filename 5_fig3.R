#========================================================================================================#
# 5_fig4.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Plot 13C-DIC vs. pH
#========================================================================================================#

  # Upper end-member
    up.em.x <- c(4,5,6,7,8)
    up.em.y <- c(-7.97,-7.69,-5.60,-0.60,1.37)
    up.em  <- as.data.frame(cbind(up.em.x, up.em.y))
    names(up.em) <- c("x","y")
  ## Store splines
    up.em.spline <- as.data.frame(cbind(spline(x=up.em$x, y=up.em$y)$x, spline(x=up.em$x, y=up.em$y)$y))
    names(up.em.spline) <- c("x","y")
    
  # Lower end-member
    lw.em.x <- c(4,5,6,7,8)
    lw.em.y <- c(-26.84,-26.56,-24.46,-19.47,-17.50)
    lw.em  <- as.data.frame(cbind(lw.em.x, lw.em.y))
    names(lw.em) <- c("x","y")
  ## Store splines
    lw.em.spline <- as.data.frame(cbind(spline(x=lw.em$x, y=lw.em$y)$x, spline(x=lw.em$x, y=lw.em$y)$y))
    names(lw.em.spline) <- c("x","y")
  
  pH_DI13C <- drop.levels(subset(c5means, select=c("Loc","Site","pH","pHse","DI13C","DI13Crange")))
  pH_DI13C$DI13C[pH_DI13C$DI13C == "NaN"] <- "NA"
  #pH_DI13C$DI13Crange[pH_DI13C$DI13Crange == "NaN"] <- "NA"
  pH_DI13C <- drop.levels(subset(pH_DI13C, pH_DI13C$DI13C != "NA"))
  pH_DI13C$DI13C <- as.numeric(pH_DI13C$DI13C)
  pH_DI13C$Loc <- factor(pH_DI13C$Loc, levels=c("BB","PP","PAP","AP"))
  #pH_DI13C$Loc <- factor(c5means$Loc, levels=c("AP","PAP","PP","BB"))
  
  # export as 4" x 5" PDF
  
    ggplot(data=pH_DI13C, mapping=aes(x=pH, y=DI13C)) +
      # Set axis limits
        scale_x_continuous(limits=c(4,8)) +
        scale_y_continuous(limits=c(-28,4), breaks=seq(-28,4,4)) +
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
      # Add end-members
        geom_vline(xintercept=7.355, lwd=0.8, lty=1, col="darkgray") + # Reference line where HCO3:DIC > 0.9 (pH 7.34)
        geom_smooth(data=up.em.spline, aes(x=x, y=y), method=loess, se=F, col="darkgray", lwd=1.2) +
        geom_smooth(data=lw.em.spline, aes(x=x, y=y), method=loess, se=F, col="darkgray", lwd=1.2) +
        geom_rect(mapping=aes(xmin=7.375, xmax=8, ymin=0, ymax=2), color="transparent", fill="white") +
        geom_rect(mapping=aes(xmin=7.375, xmax=8, ymin=-22, ymax=-17.3), color="transparent", fill="white") +
        #geom_segment(aes(x=5, xend=6.8, y=-21, yend=-15), linetype="solid", size=0.75, colour="blue", arrow=arrow(length=unit(0.25,"cm"))) +
        # SACW
          geom_rect(mapping=aes(xmin=7.355, xmax=8, ymin=-0.7, ymax=-5.6), color="transparent", fill="darkgray") + # SACW range
        # CACW
          geom_rect(mapping=aes(xmin=7.355, xmax=8, ymin=-12.7, ymax=-17.3), color="transparent", fill="darkgray") + # CACW range
        # CASW
          geom_rect(mapping=aes(xmin=7.355, xmax=8, ymin=-28, ymax=-24.6), color="transparent", fill="darkgray") + # CASW/unreacted H2CO3 range
        # Add end-member annotations
          scale_color_manual(values=rep(c("black"),15)) +
          theme(axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0))) +
          theme(axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0))) +
          labs(y=expression(delta^"13"*"C-DIC"), x="pH") +
          annotate("text", x=4.5, y=-6.7, label=expression("Eq'm w/ atm CO"[2]), col="darkgray", size=3.5) +
          annotate("text", x=4.5, y=-25.5, label=expression("Eq'm w/ soil CO"[2]), col="darkgray", size=3.5) +
          #annotate("text", x=4.1, y=-5, label="Eq'm w/", col="darkgray", size=3.5) +
          #annotate("text", x=4.12, y=-7, label=expression("atm CO"[2]), col="darkgray", size=3.5) +
          #annotate("text", x=4.1, y=-23.5, label="Eq'm w/", col="darkgray", size=3.5) +
          #annotate("text", x=4.11, y=-25.5, label=expression("soil CO"[2]), col="darkgray", size=3.5) +
          annotate("text", x=7.535, y=-1.4, label="SACW", col="black", size=2.8) + # SACW label
          annotate("text", x=7.535, y=-13.4, label="CACW", col="black", size=2.8) + # CACW label
          annotate("text", x=7.535, y=-25.4, label="CASW", col="black", size=2.8) + # CASW label
      # Add data points
        geom_errorbar(aes(ymin=pH_DI13C$DI13C-pH_DI13C$DI13Crange, ymax=pH_DI13C$DI13C+pH_DI13C$DI13Crange), colour=c("blue","purple","green3","orange")[pH_DI13C$Loc]) + # , order=pH_DI13C$Loc, width=0.2, position=position_dodge(0.9)
        geom_errorbarh(aes(xmin=pH_DI13C$pH-pH_DI13C$pHse, xmax=pH_DI13C$pH+pH_DI13C$pHse), colour=c("blue","purple","green3","orange")[pH_DI13C$Loc]) +
        geom_point(aes(fill=Loc, shape=Loc, size=3.2), color="black", stroke=0.8) +
        scale_fill_manual(pH_DI13C$Loc, values=c("blue","purple","green3","orange")) +
        scale_shape_manual(pH_DI13C$Loc, values=c(22,23,25,21))
      # Add legend
        geom_point(data=pH_DI13C, aes(x=4.1, y=4, col="black"), shape=21, fill="orange", size=3.5, stroke=1) +
        annotate("text", x=4.84, y=4, label="Travaillant Uplands", col="black", size=3.5) +
        geom_point(data=pH_DI13C, aes(x=4.1, y=2, col="black"), shape=25, fill="green3", size=3.5, stroke=1) +
        annotate("text", x=4.91, y=2, label="Mackenzie Lowlands", col="black", size=3.5) +
        geom_point(data=pH_DI13C, aes(x=4.1, y=-0.3, col="black"), shape=23, fill="purple", size=3.5, stroke=1) +
        annotate("text", x=4.64, y=-0.3, label="Peel Plateau", col="black", size=3.5) +
        geom_point(data=pH_DI13C, aes(x=4.1, y=-2.2, col="black"), shape=22, fill="blue", size=3.5, stroke=1) +
        annotate("text", x=4.96, y=-2.2, label="Richardson Mountains", col="black", size=3.5)
