


#save(list=ls()[grepl("barplot",ls()) | grepl("area",ls())], file=file.path("D:","FBA","BENTHIS_2020", "ggplots.RData"))
load(file=file.path("D:","FBA","BENTHIS_2020", "ggplots.RData"))


 friendly_met_names <- function(dd){
     # a more friendly naming of metiers
     dd$met_desc1 <- NA # init
     dd$met_desc2 <- NA # init
     dd$met_desc3 <- NA # init
     dd[grepl("27.3",dd$LE_MET), "met_desc1"] <- "BS"
     dd[grepl("27.4",dd$LE_MET), "met_desc1"] <- "NS"
     dd[grepl("OTB",dd$LE_MET), "met_desc2"] <- "dem.trawl for\n"
     dd[grepl("PTB",dd$LE_MET), "met_desc2"] <- "paired trawl for\n"
     dd[grepl("OTB",dd$LE_MET), "met_desc2"] <- "dem.trawl for\n"
     dd[grepl("SDN",dd$LE_MET), "met_desc2"] <- "dem.seine for\n"
     dd[grepl("SSC",dd$LE_MET), "met_desc2"] <- "scot.seine for\n"
     dd[grepl("PS_",dd$LE_MET), "met_desc2"] <- "purse seine for\n"
     dd[grepl("GNS",dd$LE_MET), "met_desc2"] <- "gillnet for\n"
     dd[grepl("DRB",dd$LE_MET), "met_desc2"] <- "dredge for\n"
     dd[grepl("TBB",dd$LE_MET), "met_desc2"] <- "beam.trawl for\n"
     dd[grepl("TM",dd$LE_MET), "met_desc2"] <- "midw.trawl for\n"
     dd[grepl("OTHER",dd$LE_MET), "met_desc2"] <- "misc."
     dd[grepl(">=120_0_0",dd$LE_MET), "met_desc3"] <- "fish (>120mm)"
     dd[grepl(">=105_1_120",dd$LE_MET), "met_desc3"] <- "fish (105-120mm)"
     dd[grepl(">=105_1_110",dd$LE_MET), "met_desc3"] <- "fish (105-110mm)"
     dd[grepl("120-219_0",dd$LE_MET), "met_desc3"] <- "fish (120-219mm)"
     dd[grepl("90-104_0",dd$LE_MET), "met_desc3"] <- "fish (90-104mm)"
     dd[grepl("70-99_0",dd$LE_MET), "met_desc3"] <- "fish (70-99mm)"
     dd[grepl("90-119_0_0",dd$LE_MET), "met_desc3"] <- "fish (90-119mm)"
     dd[grepl("100-119_0_0",dd$LE_MET), "met_desc3"] <- "fish (100-119mm)"
     dd[grepl("120-219_0",dd$LE_MET), "met_desc3"] <- "fish (120-219mm)"
     dd[grepl("<16_0_0",dd$LE_MET), "met_desc3"] <- "forage fish (<16mm)"
     dd[grepl("SPF_16-31_0_0",dd$LE_MET), "met_desc3"] <- "pelagics (16-31mm)"
     dd[grepl("_PS_SPF_>0_0_0",dd$LE_MET), "met_desc3"] <- "pelagics"
     dd[grepl("SPF_32-69_0_0",dd$LE_MET), "met_desc3"] <- "pelagics (32-69mm)"
     dd[grepl("DEF_16-31_0_0",dd$LE_MET), "met_desc3"] <- "d/pelagics (16-31mm)"
     dd[grepl("CRU_32-69",dd$LE_MET), "met_desc3"] <- "crustaceans (32-69mm)"
     dd[grepl("CRU_80-99",dd$LE_MET), "met_desc3"] <- "crustaceans (80-99mm)"
     dd[grepl("CRU_>=120_0_0",dd$LE_MET), "met_desc3"] <- "crustaceans (>120mm)"
     dd[grepl("MOL",dd$LE_MET), "met_desc3"] <- "molluscs"
     dd[grepl("CSH",dd$LE_MET), "met_desc3"] <- "shrimp"
     dd$met_desc2[is.na(dd$met_desc2)] <- ""
     dd$met_desc3[is.na(dd$met_desc3)] <- ""

    return(paste(dd$met_desc1, dd$met_desc2, dd$met_desc3))
    }
    

some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

   load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
   met_set_1 <- friendly_met_names(aggResultPerMetAlly)
   load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
   met_set_2 <- friendly_met_names(aggResultPerMetAlly)
   library(wesanderson)
   names(wes_palettes)
   some_colors <- wes_palette("Darjeeling2", length( unique(c(met_set_1, met_set_2))), type = c("continuous"))
   #for(i in c(11:20)){
   #set.seed(i)
   set.seed(20)
   some_colors <- some_colors[sample(1:length(some_colors))]
   some_color_seg <- as.character(some_colors)
   names(some_color_seg) <- unique(c(met_set_1, met_set_2))
   some_color_seg <- some_color_seg[!is.na(names(some_color_seg))]
   print(p5_barplot_bottomfishing_dem_fpuc_per_stk + scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ guides(fill =guide_legend(ncol=3, position="right")) )
   #print(i)
   #browser()
   #}



 # paper
 a_width <- 8000 ; a_height <- 11500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "landAndFuel.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p1_barplot_bottomfishing_dem_land_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p2_barplot_bottomfishing_dem_fuel_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p1_barplot_bottomfishing_pel_land_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p2_barplot_bottomfishing_pel_fuel_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p1_barplot_pelfishing_pel_land_per_stk + guides(fill =guide_legend(ncol=3, position="right")) + scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12), axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p2_barplot_pelfishing_pel_fuel_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
        ncol=1, heights=c(1,1,1,1,1, 1), common.legend = FALSE, legend="right")

dev.off()

a_unit <- 1
 # paper
 a_width <- 8000 ; a_height <- 11500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "fpuc.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p5_barplot_bottomfishing_dem_fpuc + guides(fill =guide_legend(ncol=7, position="right"))+ ggtitle("Bottom fishing") + theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(margin=margin(t=40,b=-20))),
              p5_barplot_bottomfishing_pel_fpuc + guides(fill =guide_legend(ncol=7, position="right"))+ ggtitle("Bottom fishing small/no mesh") + theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_pelfishing_pel_fpuc + guides(fill =guide_legend(ncol=7, position="right"))+ ggtitle("Pelagic fishing") + theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_bottomfishing_dem_fpuc_per_stk + scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ ggtitle("Bottom fishing") + guides(fill =guide_legend(ncol=3, position="right"))+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_bottomfishing_pel_fpuc_per_stk + scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ ggtitle("Bottom fishing small/no mesh") + guides(fill =guide_legend(ncol=3, position="right"))+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_pelfishing_pel_fpuc_per_stk+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ ggtitle("Pelagic fishing")  + guides(fill =guide_legend(ncol=3, position="right"))+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
          ncol=1, heights=c(1.5,1.5,1.2,1,1, 1), common.legend = FALSE, legend="right")

dev.off()



