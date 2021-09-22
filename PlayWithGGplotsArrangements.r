


save(list=ls()[grepl("barplot",ls()) | grepl("area",ls())], file=file.path("D:","FBA","BENTHIS_2020", "ggplots.RData"))



 # paper
 a_width <- 8000 ; a_height <- 10500
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
        ncol=1, heights=c(1,1,1,1.5,1.5, 1.5), common.legend = FALSE, legend="right")

dev.off()


