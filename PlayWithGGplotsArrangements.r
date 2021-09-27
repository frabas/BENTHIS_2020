


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
     dd[grepl("LLS",dd$LE_MET), "met_desc2"] <- "longline for\n"
     dd[grepl("LHP",dd$LE_MET), "met_desc2"] <- "handline for\n"
     dd[grepl("LLD",dd$LE_MET), "met_desc2"] <- "longline for\n"
     dd[grepl("FPN",dd$LE_MET), "met_desc2"] <- "pots for\n"
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
     dd[grepl("CRU_>0_0",dd$LE_MET), "met_desc3"] <- "crustaceans"
     dd[grepl("CRU_>=120_0_0",dd$LE_MET), "met_desc3"] <- "crustaceans (>120mm)"
     dd[grepl("LHP_FIF",dd$LE_MET), "met_desc3"] <- "fish"
     dd[grepl("ANA",dd$LE_MET), "met_desc3"] <- "mig. fish"
     dd[grepl("CAT",dd$LE_MET), "met_desc3"] <- "catadromus sp"
     dd[grepl("MOL",dd$LE_MET), "met_desc3"] <- "molluscs"
     dd[grepl("TBB_CRU_16-31",dd$LE_MET), "met_desc3"] <- "shrimp"
     dd[grepl("LLS_DEF_0_0_0",dd$LE_MET), "met_desc3"] <- "fish"
     dd[grepl("157_0_0",dd$LE_MET), "met_desc3"] <- "fish (>157mm)"
     dd[grepl("110-156_0_0",dd$LE_MET), "met_desc3"] <- "fish (110-156mm)"
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
   # alternative palette:
   #library(wesanderson)
   #names(wes_palettes)
   #some_colors <- wes_palette("Darjeeling1", length( unique(c(met_set_1, met_set_2))), type = c("continuous"))
   ##for(i in c(11:20)){
   ##set.seed(i)
   #set.seed(5)
   #some_colors <- some_colors[sample(1:length(some_colors))]
   #some_color_seg <- as.character(some_colors)
   #some_color_seg <- some_color_seg[1:length(unique(c(met_set_1, met_set_2)))]
   #names(some_color_seg) <- unique(c(met_set_1, met_set_2))
   #some_color_seg <- some_color_seg[!is.na(names(some_color_seg))]
   #print(p5_barplot_bottomfishing_dem_fpuc_per_stk + scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ guides(fill =guide_legend(ncol=3, position="right")) )
   ##print(i)
   ##browser()
   ##}
    library(colorspace)
   idx <- grep("dem.trawl for\n fish", unique(c(met_set_1, met_set_2)))
    some_color_seg[ idx  ] <- terrain_hcl(12)  [1:length(idx)]
    idx <- grep("dem.trawl for\n pelagics", unique(c(met_set_1, met_set_2)))
   some_color_seg[ idx  ] <- colorspace::rainbow_hcl(12)  [1:length(idx)]
    idx <- grep("dem.trawl for\n crustaceans", unique(c(met_set_1, met_set_2)))
  some_color_seg[ idx  ] <-  colorspace::heat_hcl(12)  [1:length(idx)]
  idx <- grep("midw.trawl", unique(c(met_set_1, met_set_2)))
   some_color_seg[ idx  ] <-  colorspace::diverge_hcl(12)  [1:length(idx)]
   idx <- grep("seine", unique(c(met_set_1, met_set_2)))
   some_color_seg[ idx  ] <-  colorspace::sequential_hcl(8)  [1:length(idx)]
   idx <- grep("gillnet", unique(c(met_set_1, met_set_2)))
   some_color_seg[ idx  ] <-  colorRampPalette(c("red","pink")) (2)  [1:length(idx)]
   idx <- grep("dredge", unique(c(met_set_1, met_set_2)))
   some_color_seg[ idx  ] <-  wes_palette("IsleofDogs1", 15, type = c("continuous"))[1:length(idx)]
   idx <- grep("beam", unique(c(met_set_1, met_set_2)))
     some_color_seg[ idx  ] <- colorRampPalette(c("pink","purple")) (5)  [1:length(idx)]
    idx <- unique(c(grep("NA", unique(c(met_set_1, met_set_2))), grep("misc.", unique(c(met_set_1, met_set_2)))))
     some_color_seg[ idx  ] <- colorRampPalette(c("black","white")) (10)  [1:length(idx)]

  # paper
 a_width <- 8000 ; a_height <- 11500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "landAndFuel.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p1_barplot_bottomfishing_dem_land_per_stk  + ggtitle("Bottom fishing") + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-55))),
              p2_barplot_bottomfishing_dem_fuel_per_stk  + ggtitle("Bottom fishing") + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-55))),
              p1_barplot_bottomfishing_pel_land_per_stk  + ggtitle("Bottom fishing with small/no mesh") + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-55))),
              p2_barplot_bottomfishing_pel_fuel_per_stk  + ggtitle("Bottom fishing with small/no mesh ") + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-55))),
              p1_barplot_pelfishing_pel_land_per_stk  + ggtitle("Pelagic fishing") + guides(fill =guide_legend(ncol=3, position="right")) + scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12), axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-55))),
              p2_barplot_pelfishing_pel_fuel_per_stk  + ggtitle("Pelagic fishing") + guides(fill =guide_legend(ncol=3, position="right"))+ scale_fill_manual(values=some_color_seg, name="Fleet-segments")+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-55))),
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


# split into two
#1
a_unit <- 1
 # paper
 a_width <- 6000 ; a_height <- 7500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "fpuc_per_fleet.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p5_barplot_bottomfishing_dem_fpuc + ggtitle("Bottom fishing") + guides(fill =guide_legend(ncol=2, position="right")) + theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), plot.title=element_text(margin=margin(t=40,b=-20))),
              p5_barplot_bottomfishing_pel_fpuc + ggtitle("Bottom fishing small/no mesh")+ guides(fill =guide_legend(ncol=2, position="right")) + theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_pelfishing_pel_fpuc + ggtitle("Pelagic fishing") + guides(fill =guide_legend(ncol=2, position="right")) + theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
          ncol=1, heights=c(1.5,1.5,1.2,1,1, 1), common.legend = FALSE, legend="right")

dev.off()

#2
 a_width <- 6000 ; a_height <- 7500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "fpuc_per_species.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p5_barplot_bottomfishing_dem_fpuc_per_stk+ ggtitle("Bottom fishing")+ scale_fill_manual(values=some_color_seg, name="Fleet-segments") + guides(fill =guide_legend(ncol=2, position="right"))+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_bottomfishing_pel_fpuc_per_stk+ ggtitle("Bottom fishing small/no mesh")+ scale_fill_manual(values=some_color_seg, name="Fleet-segments") + guides(fill =guide_legend(ncol=2, position="right"))+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p5_barplot_pelfishing_pel_fpuc_per_stk+ ggtitle("Pelagic fishing") + scale_fill_manual(values=some_color_seg, name="Fleet-segments") + guides(fill =guide_legend(ncol=2, position="right"))+ theme(legend.text = element_text(size = 12),axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
          ncol=1, heights=c(1.5,1.5,1.2,1,1, 1), common.legend = FALSE, legend="right")

dev.off()


if(FALSE){
# TODO: FIXME
 library(png)
 library()
 img <- readPNG (file.path("D:","FBA","BENTHIS_2020","FishSilhouettes", "Gadus_morhua.png"))
  plot(1:2, type='n')
 rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
 library(ggimage)
 p5_barplot_bottomfishing_dem_fpuc_per_stk  +
  #geom_col() +
  geom_image(aes(image=img, y = 1392), size=.2)


 the_agg_plot5$img <-file.path("D:","FBA","BENTHIS_2020","FishSilhouettes", "Gadus_morhua.png")
 p5_barplot_bottomfishing_dem_fpuc_per_stk <- ggplot(data=the_agg_plot5, aes(x=Stock, y=value/a_unit, fill=met_desc)) + geom_image(aes(image=img),angle=180, size=.1) + #  geom_bar(stat="identity", position=position_dodge())
    geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per kg catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  +
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))


 p5_barplot_bottomfishing_dem_fpuc_per_stk
 height_bar_max <- apply(tapply(p5_barplot_bottomfishing_dem_fpuc_per_stk$data$value,
                         list(p5_barplot_bottomfishing_dem_fpuc_per_stk$data$Stock, p5_barplot_bottomfishing_dem_fpuc_per_stk$data$met_desc),mean),
                            1, sum, na.rm=TRUE)
 img <- readPNG (file.path("D:","FBA","BENTHIS_2020","FishSilhouettes", "Gadus_morhua.png"))
 for(sp in 1:length(height_bar_max)){
 library("grid")
 grid.points(x = 0.5, y = 0.5, default.units = "npc", pch="+",gp=gpar(cex=5))
 grid.raster(img, vp = viewport(  x =seq( 0.08, 0.78, length=14)[sp], y = height_bar_max[sp]+0.1, angle=90), height=unit(1.5,"cm"))
}

                                 ,

dd <- ggplot_build  (p5_barplot_bottomfishing_dem_fpuc_per_stk)
 unique(dd$data[[1]]$xmin +   dd$data[[1]]$xmax) / 2

dd$layout$panel_scales_x[[1]]$range$range

}
