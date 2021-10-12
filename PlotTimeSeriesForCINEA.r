
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

#REDO SOME STUFF FOR A STANDALONE CODE:
 setwd (file.path("D:","FBA","BENTHIS_2020"))   # adapt to your need
 library(doBy)
 years <- 2005:2019
 spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
 color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                    "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                    "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")

 some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                        "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                         "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                         "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                          "WIT"="red", "WHG"="yellow", "OTH"="blue")

 # caution: always same color for seg
  some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
   load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
   met_set_1 <- friendly_met_names(aggResultPerMetAlly)
   load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
   met_set_2 <- friendly_met_names(aggResultPerMetAlly)
   names(some_color_seg) <- unique(c(met_set_1, met_set_2))
   some_color_seg <- some_color_seg[!is.na(names(some_color_seg))]



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


 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly

 head(aggResultPerMetAlly[aggResultPerMetAlly$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0",])



 library(ggplot2)

 # compare prices e.g seine vs. trawls for cod
 #aggResultPerMetAlly[aggResultPerMetAlly$LE_MET=="LargeMesh_27.3_SDN_DEF_>=120_0_0","LE_MPRICE_COD"]
 #[1] 2.766031 3.310001 3.993804 3.396713 2.590793 3.018366 3.013149 3.092763 3.211551 2.927512 3.304162 3.328078 3.332026 3.047696 3.332285
 # aggResultPerMetAlly[aggResultPerMetAlly$LE_MET=="LargeMesh_27.3_OTB_DEF_>=105_1_120","LE_MPRICE_COD"]
 #[1] 1.156881 1.237244 1.215207 1.128402 1.081045 1.105845 1.171007 1.296540 1.363090 1.592291


 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     dd$met_desc <- friendly_met_names(dd)

      # debug SAN
     dd[grepl("LargeMesh",dd$LE_MET) , paste0(prefixes[count], "SAN")] <- 0 # because SAN creates misleading distorsion on plots...

     # debug outlier
     aggResultPerMetAlly[aggResultPerMetAlly$FPUCallsp>100, c("FPUCallsp", "FPUVallsp")] <- 0

     # get percent per stock for sectorisation
     PercentThisStk <- dd[paste0(prefixes[count],spp)] / apply(dd[paste0(prefixes[count],spp)], 1, sum, na.rm=TRUE)*100
     if(prefixes[count]=="LE_MPRICE_")   PercentThisStk <- (dd[paste0(prefixes[count],spp)]*dd[paste0("LE_KG_",spp)]) / apply(dd[paste0(prefixes[count],spp)]*dd[paste0("LE_KG_",spp)], 1, sum, na.rm=TRUE)*100

     colnames(PercentThisStk)  <- paste0("Percent_",spp)
     dd <- cbind.data.frame (dd, PercentThisStk)
     VarThisStk <- sweep(dd[,colnames(PercentThisStk)]/100, 1, dd[,a_variable], FUN="*")
     colnames(VarThisStk)  <- spp
     dd <- cbind.data.frame (dd, VarThisStk)
     # reshape
     library(data.table)
     long <- melt(setDT(dd[,c("met_desc","LE_MET", a_variable, colnames(VarThisStk))]), id.vars = c("met_desc","LE_MET", a_variable), variable.name = "Stock")
     colnames(long)[colnames(long)==a_variable] <- "Total"

     #as.data.frame(long)
     long <- long[complete.cases(long),]

     long$LE_MET <- paste(long$LE_MET, the_names[count])

     head(long[long$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0 (b)",])

     if(y==years[1]){
     agg <- cbind.data.frame(as.data.frame(long), year=y)
     }
      else{
      agg <- rbind.data.frame(as.data.frame(agg),
             cbind.data.frame(long, year=y))
     }

   }

     the_agg <- rbind.data.frame(as.data.frame(the_agg), as.data.frame(agg))
 }


  the_agg <- the_agg[!grepl("misc.",the_agg$met_desc),] # get rid of OTHER

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 6200 ; a_height <- 10500
 library(ggplot2)
 a_comment <- ""
 a_unit <- 1

# DEM
  the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])
  # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), year=levels(factor(the_agg_plot$year)))
 dd$met_desc <- friendly_met_names (dd)

 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

 the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)

  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 p1_cinea_bottomfishing_dem_land <- ggplot(the_agg_plot1, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
   geom_line(aes(color=Stock))  +    geom_point(aes(color=Stock), size=3) +   labs(y = "Landings (tons)", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p1_cinea_bottomfishing_dem_land)
 
      ## TODO:
      


  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_cinea_bottomfishing_dem_cpuf <- ggplot(the_agg_plot3, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
  scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_cinea_bottomfishing_dem_vpuf <- ggplot(the_agg_plot4, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p4)


  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
 p5_cinea_bottomfishing_dem_fpuc <- ggplot(the_agg_plot5, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)


  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p6_cinea_bottomfishing_dem_fpuv <- ggplot(the_agg_plot6, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock))+    geom_point(aes(color=Stock), size=3)   +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p6)

 ### ADD-ON for bottom fishing: revenue per swept area
  the_agg_plot7 <- as.data.frame(the_agg_plot[grep("(g)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot7$LE_MET <- gsub("\\(g)","", the_agg_plot7$LE_MET)
 p7_cinea_bottomfishing_dem_vswpa <- ggplot(the_agg_plot7, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "Euro catch per km-sq", x = "Year")   +
  scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p7)


 ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p8_cinea_bottomfishing_dem_price <- ggplot(the_agg_plot8, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p8)

 # for paper:
 namefile <- paste0("ts_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_DEM_CINEAplot_land_and_FPUE_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_cinea_bottomfishing_dem_land, p4_cinea_bottomfishing_dem_vpuf, p5_cinea_bottomfishing_dem_fpuc, ncol=3, common.legend = TRUE, legend="right")

dev.off()




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##FORAGE FISH or SMALL MESHES

 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     dd$met_desc <- friendly_met_names(dd)

      # debug SAN
     dd[grepl("LargeMesh",dd$LE_MET) , paste0(prefixes[count], "SAN")] <- 0 # because SAN creates misleading distorsion on plots...

     # debug outlier
     aggResultPerMetAlly[aggResultPerMetAlly$FPUCallsp>100, c("FPUCallsp", "FPUVallsp")] <- 0

     # get percent per stock for sectorisation
     PercentThisStk <- dd[paste0(prefixes[count],spp)] / apply(dd[paste0(prefixes[count],spp)], 1, sum, na.rm=TRUE)*100
     if(prefixes[count]=="LE_MPRICE_")   PercentThisStk <- (dd[paste0(prefixes[count],spp)]*dd[paste0("LE_KG_",spp)]) / apply(dd[paste0(prefixes[count],spp)]*dd[paste0("LE_KG_",spp)], 1, sum, na.rm=TRUE)*100

     colnames(PercentThisStk)  <- paste0("Percent_",spp)
     dd <- cbind.data.frame (dd, PercentThisStk)
     VarThisStk <- sweep(dd[,colnames(PercentThisStk)]/100, 1, dd[,a_variable], FUN="*")
     colnames(VarThisStk)  <- spp
     dd <- cbind.data.frame (dd, VarThisStk)
     # reshape
     library(data.table)
     long <- melt(setDT(dd[,c("met_desc","LE_MET", a_variable, colnames(VarThisStk))]), id.vars = c("met_desc","LE_MET", a_variable), variable.name = "Stock")
     colnames(long)[colnames(long)==a_variable] <- "Total"

     #as.data.frame(long)
     long <- long[complete.cases(long),]

     long$LE_MET <- paste(long$LE_MET, the_names[count])

     head(long[long$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0 (b)",])

     if(y==years[1]){
     agg <- cbind.data.frame(as.data.frame(long), year=y)
     }
      else{
      agg <- rbind.data.frame(as.data.frame(agg),
             cbind.data.frame(long, year=y))
     }

   }

     the_agg <- rbind.data.frame(as.data.frame(the_agg), as.data.frame(agg))
 }


  the_agg <- the_agg[!grepl("misc.",the_agg$met_desc),] # get rid of OTHER


 a_width <- 6200 ; a_height <- 12500
 library(ggplot2)

  the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), year=levels(factor(the_agg_plot$year)))
 dd$met_desc <- friendly_met_names (dd)

 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)

  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 p1_cinea_bottomfishing_pel_land <-  ggplot(the_agg_plot1, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
   geom_line(aes(color=Stock))  +    geom_point(aes(color=Stock), size=3) +   labs(y = "Landings (tons)", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p1)

  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_cinea_bottomfishing_dem_cpuf <- ggplot(the_agg_plot3, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
  scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_cinea_bottomfishing_pel_vpuf <- ggplot(the_agg_plot4, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
 p5_cinea_bottomfishing_pel_fpuc <-ggplot(the_agg_plot5, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)


  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  p6_cinea_bottomfishing_pel_fpuv <- ggplot(the_agg_plot6, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock))+    geom_point(aes(color=Stock), size=3)   +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p6_area_bottomfishing_pel_fpuv)

  ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p8_cinea_bottomfishing_pel_price <- ggplot(the_agg_plot8, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_line(aes(color=Stock)) +    geom_point(aes(color=Stock), size=3)  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p8)



# PEL: for paper:
 a_width <- 6000 ; a_height <- 6500
 namefile <- paste0("ts_fuel_efficiency_", "mean_fuel_efficiency", "_", years[1], "-", years[length(years)],  a_comment, "_PEL_CINEAplot_land_and_FPUE_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_cinea_bottomfishing_pel_land, p4_cinea_bottomfishing_pel_vpuf, p5_cinea_bottomfishing_pel_fpuc, ncol=3, common.legend = TRUE, legend="right")

dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##





