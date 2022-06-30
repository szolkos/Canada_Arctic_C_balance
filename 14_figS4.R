#========================================================================================================#
# 14_figS4.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-01-31
# Background: Plot Figure S2: Absorbance (a254) from Horiba Aqualog vs. Genesys10
#========================================================================================================#

    # Create and summarize lm of data
      par(mar=c(4.5,4.5,1,1))
      plot(c5abs$abs254aqlg~c5abs$abs254ari, ylab="Aqualog (abs at 254 nm)", xlab="ARI instrument (abs at 254 nm)", pch=21, bg="black", col="white", cex=1.5)
      abs_lm <- lm(c5abs$abs254aqlg~c5abs$abs254ari)
      par(mfrow=c(2,2))
      plot(abs_lm)
      summary(abs_lm)
    
    # Plot w/out error bars
      ggplot() +
        # Set axis limits
          scale_y_continuous(limits=c(0,3), breaks=seq(0,3,1)) +
          scale_x_continuous(limits=c(0,3), breaks=seq(0,3,1)) +
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
          geom_abline(intercept=0, slope=1, color="blue", linetype="solid", size=0.4) +
          geom_abline(intercept=0.0003288, slope=0.9830061, color="black", linetype="solid", size=0.5) +
        # Add data points and error bars
          geom_point(data=c5abs, aes(y=abs254aqlg, x=abs254ari, color="white"), shape=21, fill="black", size=5, stroke=0.6) +
          scale_color_manual(values=rep(c("white"),15)) +
        # Add axis labels
          labs(y=expression(italic(a)[254]~(m^-1*","~Horiba~Aqualog)), x=expression(italic(a)[254]~(m^-1*","~Genesys~"10")))
        