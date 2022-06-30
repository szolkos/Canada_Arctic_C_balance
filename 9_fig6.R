#========================================================================================================#
# 9_fig6.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2022-02-01
# Background: Plots of total fluvial C flux relative to NEE, by region
#========================================================================================================#
  
  # Read in data
    #fluvfluxpNEE <- read.csv(paste0(dir,"Ch5/fluvial_fluxes_prop_NEE_2020_03_31.csv"), header=T)
    fluvfluxpNEE <- read.csv(paste0(dir,"Ch5/fluvial_fluxes_prop_NEE_2022_02_01.csv"), header=T)
    fluvfluxpNEE$Loc <- factor(fluvfluxpNEE$Loc, levels=c("BB","PP","PAP","AP"))
    fluvfluxpNEE$LocSite <- paste0(fluvfluxpNEE$Loc,fluvfluxpNEE$Site)
    
  # Boxplot- total fluvial C : NEE
    x_labels <- paste0(c("Richardson","Peel","Mackenzie","Travaillant"),"\n",c("Mountains","Plateau","Lowlands","Uplands"))

    #ggplot(fluvfluxpNEE, aes(y=NEEp_mean, x=Loc, fill=factor(Loc))) +
    #ggplot(fluvfluxpNEE, aes(x=Loc, y=NEEp_mean, ymin=NEEp_min, ymax=NEEp_max, label=LocSite), fill=factor(Loc)) +
    ggplot(fluvfluxpNEE, aes(x=Loc, y=NEEp_mean, ymin=NEEp_min, ymax=NEEp_max)) +
      # Geom
        #geom_boxplot(aes(fill=factor(Flux))) +
      # Fill
        #scale_fill_manual(name="Flux", values=c("grey40","white")) +
        scale_fill_manual(name="Loc", values=c("blue","purple","green3","orange")) +
      # Shape
        scale_shape_manual(name="Flux", values=c(21,24)) +
      # Axis scales
        scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1)) +
      # Themes
        theme_bw() +
        theme(plot.margin=unit(c(0.1,0.1,0,0.2), "in"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              plot.background=element_rect(colour="white", fill="white", size=1),
              panel.border=element_rect(colour="black", fill=NA, size=0.8),
              panel.background=element_rect(fill='white'),
              text=element_text(size=13),
              axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black"),
              axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0)),
              axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black"),
              legend.position="none") +
      # Add points
        geom_pointrange(aes(fill=Loc, shape=Flux), size=1, stroke=0.8, position=position_jitterdodge(seed=0, jitter.width=1.3)) +
        #geom_text_repel() + # data=mtcars, aes(wt, mpg, label=label)
        #geom_point(aes(fill=Flux), size=0.9, stroke=0.8, position=position_jitterdodge(seed=0)) + # position=position_dodge(width=0.8)
        #geom_errorbar(aes(fill=Flux, ymin=NEEp_min, ymax=NEEp_max), color="grey40", width=0, position=position_jitterdodge(seed=0)) +
      # Axis labels
        labs(y="Fluvial C flux : NEE", x="") +
        #scale_x_discrete(labels=c("Richardson Mountains","Peel Plateau","Mackenzie Lowlands","Travaillant Uplands")) +
        scale_x_discrete(labels=x_labels)

    subset(fluvfluxpNEE, select=c("Loc","Site","Flux","NEEp_mean"), fluvfluxpNEE$Loc=="AP" & fluvfluxpNEE$Flux=="vertical")
