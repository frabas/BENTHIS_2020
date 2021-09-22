


#save(list=ls()[grepl("barplot",ls()) | grepl("area",ls())], file=file.path("D:","FBA","BENTHIS_2020", "ggplots.RData"))
load(file=file.path("D:","FBA","BENTHIS_2020", "ggplots.RData"))



 # paper
 a_width <- 8000 ; a_height <- 11500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "fpuc.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p5_barplot_bottomfishing_dem_fpuc_per_stk + guides(fill =guide_legend(ncol=3, position="right")),
              p5_barplot_bottomfishing_pel_fpuc_per_stk + guides(fill =guide_legend(ncol=3, position="right")),
              p5_barplot_pelfishing_pel_fpuc_per_stk + guides(fill =guide_legend(ncol=3, position="right")),
              p5_barplot_bottomfishing_dem_fpuc + guides(fill =guide_legend(ncol=7, position="right")),
              p5_barplot_bottomfishing_pel_fpuc + guides(fill =guide_legend(ncol=7, position="right")),
              p5_barplot_pelfishing_pel_fpuc + guides(fill =guide_legend(ncol=7, position="right")),
        ncol=1, heights=c(1,1,1,1,1, 1), common.legend = FALSE, legend="right")

dev.off()




 # paper
 a_width <- 8000 ; a_height <- 11500
 namefile <- paste0("ggplot.tif")
 tiff(filename=file.path("D:","FBA","BENTHIS_2020", "ggplots", "landAndFuel.tiff"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(    p1_barplot_bottomfishing_dem_land_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p2_barplot_bottomfishing_dem_fuel_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p1_barplot_bottomfishing_pel_land_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p2_barplot_bottomfishing_pel_fuel_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p1_barplot_pelfishing_pel_land_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
              p2_barplot_pelfishing_pel_fuel_per_stk + guides(fill =guide_legend(ncol=3, position="right"))+ theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)),
        ncol=1, heights=c(1,1,1,1,1, 1), common.legend = FALSE, legend="right")

dev.off()

