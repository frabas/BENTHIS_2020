

setwd (file.path("D:","FBA","BENTHIS_2020"))   # adapt to your need

years <- 2005:2019


# retrieve all Fs and co and bio reference points from ICES data and collected and processed by STECF 20-01
saeu <- read.csv(file=file.path(getwd(), "STECF 20-01 adhoc - all supporting data and code", "analysis", "saeu.csv"), header=TRUE)

# check
westcod <- saeu[saeu$FishStock=="cod.27.22-24",]
plot (westcod$Year, westcod$FishingPressure, type="l")


# a handmade look-up table for compatibility in naming stocks
# levels(saeu$FishStock)
 spp_lookup <- c("ane.27.8"="ane.27.8",                "bli.27.5b67"="bli.27.5b67",        "cod.27.22-24"="COD.2224",            "cod.27.47d20"="COD.nsea",            "cod.27.6a"="cod.27.6a",          "cod.27.7e-k"="cod.27.7e-k",   "dgs.27.nea"="DGS.nsea",              "had.27.46a20"="HAD.nsea",
 "had.27.6b"="had.27.6b",               "had.27.7a"="had.27.7a",             "had.27.7b-k"="had.27.7b-k",          "her.27.20-24"="HER.3a22",            "her.27.25-2932"="HER.3031",      "her.27.28"="HER.281",         "her.27.3a47d"="HER.nsea",            "her.27.irls"="her.27.irls",
 "her.27.nirs"="her.27.nirs",             "hke.27.3a46-8abd"="HKE.nsea",     "hke.27.8c9a"="hke.27.8c9a",          "hom.27.2a4a5b6a7a-ce-k8"="hom.27.2a4a5b6a7a-ce-k8",                    "hom.27.9a"="hom.27.9a",       "ldb.27.8c9a"="ldb.27.8c9a",          "lez.27.4a6a"="lez.27.4a6a",     "mac.27.nea"="MAC.nsea",
 "meg.27.7b-k8abd"="meg.27.7b-k8abd",         "meg.27.8c9a"="meg.27.8c9a",   "mon.27.78abd"="mon.27.78abd",        "mon.27.8c9a"="mon.27.8c9a",          "nep.fu.11"="NEP.nsea",           "nep.fu.12"="NEP.nsea",        "nep.fu.13"="NEP.nsea",               "nep.fu.14"="NEP.nsea",
 "nep.fu.15"="NEP.nsea",               "nep.fu.16"="NEP.nsea",               "nep.fu.17"="NEP.nsea",               "nep.fu.19"="NEP.nsea",               "nep.fu.2021"="NEP.kask",         "nep.fu.22"="NEP.nsea",        "nep.fu.6"="NEP.nsea",                "nep.fu.7"="NEP.nsea",
 "nep.fu.8"="NEP.nsea",                "nep.fu.9"="NEP.nsea",                 "nop.27.3a4"="NOP.nsea",              "ple.27.21-23"="PLE.2123",             "ple.27.420"="PLE.nsea",          "ple.27.7a"="ple.27.7a",       "ple.27.7d"="ple.27.7d",              "pok.27.3a46"="POK.nsea",
 "pra.27.3a4a"="PRA.nsea",             "san.sa.1r"="SAN.nsea",               "san.sa.2r"="SAN.nsea2",               "san.sa.3r"="SAN.nsea3",               "san.sa.4"="SAN.nsea4",            "sol.27.20-24"="SOL.2024",     "sol.27.4"="SOL.nsea",                "sol.27.7a"="sol.27.7a",
 "sol.27.7e"="sol.27.7e",               "sol.27.7fg"="sol.27.7fg",           "sol.27.8ab"="sol.27.8ab",            "spr.27.22-32"="SPR.2232",            "spr.27.3a4"="SPR.nsea",          "tur.27.4"="TUR.nsea",         "whb.27.1-91214"="whb.27.1-91214",    "whg.27.47d"="WHG.nsea",
 "whg.27.6a"="whg.27.6a",               "whg.27.7a"="whg.27.7a",             "whg.27.7b-ce-k"="whg.27.7b-ce-k",
 "wit.27.3a47d"="WIT.nsea"
 )

 saeu$Stock <- spp_lookup[as.character(saeu$FishStock)]
 saeu$value <- saeu$FishingPressure / saeu$Fref  # F/FMSY ratio

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # BOTTOM-CONTACTING GEARS  - LARGE MESH
 # cross with energy use and economic efficiency from the Danish fleet
  a_var <- "VPUF"
a_var <- "CPUF"
  if(a_var=="VPUF"){ load(file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBotAllyAndStocks",years[1],"-",years[length(years)],".RData", sep="")))
  }else{  load(file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndRatios",a_var,"ForBotAllyAndStocks",years[1],"-",years[length(years)],".RData", sep="")))}     # from generateTable.R
  # x
  xx <- tapply(x$value, list(x$Stock, x$Year), mean, na.rm=TRUE) # avrage across grid cells and met...

  xx <- cbind.data.frame(xx, Stock=rownames(xx))
  library(data.table)
  long <- melt(setDT(xx[, c("Stock", as.character(years))]), id.vars = c("Stock"), variable.name = "Year")
  long <- as.data.frame(long[complete.cases(long),])


  f_and_vpuf <- rbind.data.frame(
    cbind.data.frame(saeu[,c("Stock","Year", "value")], Var="F/FMSY"),
    cbind.data.frame(long[,c("Stock","Year", "value")], Var=a_var)
    )

 f_and_vpuf$Region <- sapply(strsplit(as.character(f_and_vpuf$Stock), split="\\."), function(x) x[2])
 f_and_vpuf$StockAndVar <- paste(f_and_vpuf$Stock,f_and_vpuf$Var, sep=".")

  
  # bottom contacting gears
  dat <- f_and_vpuf[f_and_vpuf$Stock%in%c("COD.nsea", "PLE.nsea", "SOL.nsea", "PRA.nsea", "NEP.kask", "HER.nsea", "POK.nsea", "HAD.nsea", "HKE.nsea", "NEP.kask", "COD.2224", "PLE.2123", "SOL.2024"),]
  dat$Region <- factor(dat$Region)
  
  collected_data <- cbind.data.frame(dat, Seg="BottomTrawl") 
  
  
  # compute diff for figuring out the anomalies
  dat<- do.call("rbind.data.frame", lapply(split(dat, dat$StockAndVar), function(x) {
     x$diff <- c(0, diff(as.numeric(x$value))) # + mean(x$value)
     x })  
     )
     
     dat$col <- factor(ifelse(dat$diff > 0, 2, 1))
   
   
  namefile <- paste0("ts_anomalies_f_and_",a_var,"_for_dem1_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 7000; a_height=6000
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)
  # spp <- c("HER.nsea", "HAD.nsea", "POK.nsea", "SOL.2024") # select those with significant cross-correlation
   spp <- c("COD.nsea", "PRA.nsea", "POK.nsea", "SOL.nsea", "PLE.2123") # select those with significant cross-correlation
   plots <- list(NULL)
   count <- 0
   for (sp in 1: length(spp)){
     count <- count +1
     plots[[count]] <-  ggplot(dat[dat$Var=="F/FMSY" & dat$Stock==spp[sp],], aes(x=Year, y=diff,  group=StockAndVar, fill=col)) +   labs(title="",x="Year", y = paste("F/FMSY", spp[sp])) +
       geom_bar(stat = "identity", show.legend=FALSE)  + scale_fill_manual(values=c("blue", "red"))  + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
     count <- count +1
     plots[[count]] <- ggplot(dat[dat$Var=="CPUF" & dat$Stock==spp[sp],], aes(x=Year, y=diff,  group=StockAndVar, fill=col)) +   labs(title="",x="Year", y = paste("CPUF", spp[sp])) +
       geom_bar(stat = "identity", show.legend=FALSE)  + scale_fill_manual(values=c("blue", "red"))  + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
} 
    cowplot::plot_grid(plotlist = plots, ncol = 2,
                   align = 'v', labels = rep(c("F/FMSY", "CPUF"), each= length(sp)))
dev.off()

 
 
 
  namefile <- paste0("ts_f_and_",a_var,"_for_dem_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 9000; a_height=3500
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)
  
    # do al list of plot to avoid using facet_wrap(~Region, scales="free_y")
    ggList <- lapply(split(dat, dat$Region), function(i) {
       ggplot(i, aes(x=Year, y=value,  group=StockAndVar, color=StockAndVar)) +   labs(title="",x="Year", y = "CPUF or F/FMSY") +
       geom_line(size=2)  + scale_color_brewer(palette="Paired") + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
    } )
  # plot as grid in 1 columns
  cowplot::plot_grid(plotlist = ggList, ncol = 2,
                   align = 'v', labels = levels(long$Region))

  dev.off()

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## CROSS CORRELATION
 f_and_vpuf <- f_and_vpuf[!is.na(f_and_vpuf$value),]
 f_and_vpuf$value_detrended <- NA
 dd <- lapply(split(f_and_vpuf, f=f_and_vpuf$Stock), function(x){
         x<-lapply(split(x, f=x$Var), function(x){
          print(x)
           if(nrow(x)>0) x$value_detrended <- x$value -  (predict(lm(x$value~as.numeric(x$Year))))
          return(x)
        })
      x <- do.call("rbind", x)
      return(x)
     })
    x <- do.call("rbind", dd)


 # maybe worth to detrending on F/FMSY ts given we know the MP is targetting a value of 1 deliberately
 spp <- c("COD.nsea", "PLE.nsea", "SOL.nsea", "PRA.nsea", "HER.nsea", "HAD.nsea", "HKE.nsea",  "POK.nsea", "NEP.kask", "COD.2224", "PLE.2123", "SOL.2024")
 a_width <- 4500; a_height=2000
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  "crosscorrelation.tif"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=400, compression = c("lzw"))
 par(mfrow=c(2, length(spp)/2))
 for (a_spp in spp)
  ccf(x[x$Var=="F/FMSY" & x$Year %in% as.character(2012:2019) & x$Stock %in% a_spp,"value"],  # ideally we would detrended F/FMSY ts but the ts is too short that it creates misleading outcomes if we do so (given we are close to 1 in recent years...)
       x[x$Var==a_var & x$Year %in% as.character(2012:2019) & x$Stock %in% a_spp,"value"],
         lag.max = 5, type = c("correlation"),
     plot = TRUE, na.action = na.pass, xlab="", main = a_spp, ci.type = "white")
  dev.off()


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # BOTTOM-CONTACTING GEARS  - SMALL MESH
 # cross with energy use and economic efficiency from the Danish fleet
  a_var <- "VPUF"
a_var <- "CPUF"
  if(a_var=="VPUF"){ load(file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBotAllyAndStocks",years[1],"-",years[length(years)],".RData", sep="")))
  }else{  load(file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndRatios",a_var,"ForBotAllyAndStocks",years[1],"-",years[length(years)],".RData", sep="")))}
  # x
  xx <- tapply(x$value, list(x$Stock, x$Year), mean, na.rm=TRUE) # avrage across grid cells and met...

  xx <- cbind.data.frame(xx, Stock=rownames(xx))
  library(data.table)
  long <- melt(setDT(xx[, c("Stock", as.character(years))]), id.vars = c("Stock"), variable.name = "Year")
  long <- as.data.frame(long[complete.cases(long),])


  f_and_vpuf <- rbind.data.frame(
    cbind.data.frame(saeu[,c("Stock","Year", "value")], Var="F/FMSY"),
    cbind.data.frame(long[,c("Stock","Year", "value")], Var=a_var)
    )

 f_and_vpuf$Region <- sapply(strsplit(as.character(f_and_vpuf$Stock), split="\\."), function(x) x[2])
 f_and_vpuf$StockAndVar <- paste(f_and_vpuf$Stock,f_and_vpuf$Var, sep=".")


  # bottom contacting gears small mesh
  dat <- f_and_vpuf[f_and_vpuf$Stock%in%c("SAN.nsea", "NOP.nsea", "HER.nsea"),]
  dat$Region <- factor(dat$Region)
 
  collected_data <- rbind.data.frame (collected_data, cbind.data.frame(dat, Seg="BottomTrawlSmallMesh")) 
 
  
    # compute diff for figuring out the anomalies
  dat<- do.call("rbind.data.frame", lapply(split(f_and_vpuf, f_and_vpuf$StockAndVar), function(x) {
     x$diff <- c(0, diff(as.numeric(x$value))) # + mean(x$value)
     x })  
     )
     
     dat$col <- factor(ifelse(dat$diff > 0, 2, 1))
   
   

  
  
  namefile <- paste0("ts_f_and_",a_var,"_for_demsmallmesh_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 9000; a_height=3500
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)
  
  # small mesh - 
   dat <- f_and_vpuf[f_and_vpuf$Stock%in%c("SAN.nsea", "NOP.nsea", "HER.nsea"),]
    dat$Region <- factor(dat$Region)
  
    # do al list of plot to avoid using facet_wrap(~Region, scales="free_y")
    ggList <- lapply(split(dat, dat$Region), function(i) {
       ggplot(i, aes(x=Year, y=value,  group=StockAndVar, color=StockAndVar)) +   labs(title="",x="Year", y = "CPUF or F/FMSY") +
       geom_line(size=2)  + scale_color_brewer(palette="Paired") + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
    } )
  # plot as grid in 1 columns
  cowplot::plot_grid(plotlist = ggList, ncol = 2,
                   align = 'v', labels = levels(long$Region))

  dev.off()

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## CROSS CORRELATION
 f_and_vpuf <- f_and_vpuf[!is.na(f_and_vpuf$value),]
 f_and_vpuf$value_detrended <- NA
 dd <- lapply(split(f_and_vpuf, f=f_and_vpuf$Stock), function(x){
         x<-lapply(split(x, f=x$Var), function(x){
          print(x)
           if(nrow(x)>0) x$value_detrended <- x$value -  (predict(lm(x$value~as.numeric(x$Year))))
          return(x)
        })
      x <- do.call("rbind", x)
      return(x)
     })
    x <- do.call("rbind", dd)


 # maybe worth to detrending on F/FMSY ts given we know the MP is targetting a value of 1 deliberately
 spp <- c("SAN.nsea", "NOP.nsea", "HER.nsea")
  a_width <- 6000; a_height=2500
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  "crosscorrelation_smallmesh.tif"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=400, compression = c("lzw"))
 par(mfrow=c(2, length(spp)/2))
 for (a_spp in spp)
  ccf(x[x$Var=="F/FMSY" & x$Year %in% as.character(2005:2019) & x$Stock %in% a_spp,"value"],  # ideally we would detrended F/FMSY ts but the ts is too short that it creates misleading outcomes if we do so (given we are close to 1 in recent years...)
       x[x$Var==a_var & x$Year %in% as.character(2005:2019) & x$Stock %in% a_spp,"value"],
         lag.max = 5, type = c("correlation"),
     plot = TRUE, na.action = na.pass, xlab="", main = a_spp, ci.type = "white")
  dev.off()




 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # PELAGIC GEARS
 # cross with energy use and economic efficiency from the Danish fleet
 a_var <- "VPUF"
a_var <- "CPUF"
  if(a_var=="VPUF"){ load(file=file.path(getwd(), "outputs2020_pel", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPelAllyAndStocks",years[1],"-",years[length(years)],".RData", sep="")))
  }else{ load(file=file.path(getwd(), "outputs2020_pel", paste("AggregatedSweptAreaPlusMet6AndRatios",a_var,"ForPelAllyAndStocks",years[1],"-",years[length(years)],".RData", sep=""))) }
  # x
  xx <- tapply(x$value, list(x$Stock, x$Year), mean, na.rm=TRUE)

  xx <- cbind.data.frame(xx, Stock=rownames(xx))
  long <- melt(setDT(xx[, c("Stock", as.character(years))]), id.vars = c("Stock"), variable.name = "Year")
  long <- as.data.frame(long[complete.cases(long),])

  long$value <- long$value /5 ## CAUTION ##
 
  f_and_vpuf <- rbind.data.frame(
    cbind.data.frame(saeu[,c("Stock","Year", "value")], Var="F/FMSY"),
    cbind.data.frame(long[,c("Stock","Year", "value")], Var=a_var)
    )

 f_and_vpuf$Region <- sapply(strsplit(as.character(f_and_vpuf$Stock), split="\\."), function(x) x[2])
 f_and_vpuf$StockAndVar <- paste(f_and_vpuf$Stock,f_and_vpuf$Var, sep=".")


  # Pelagic small mesh
  dat <- f_and_vpuf[f_and_vpuf$Stock%in%c( "HER.nsea", "MAC.nsea", "SPR.2232"),]
  dat$Region <- factor(dat$Region)
 
  collected_data <- rbind.data.frame (collected_data, cbind.data.frame(dat, Seg="PelagicTrawl")) 


   # compute diff for figuring out the anomalies
  dat<- do.call("rbind.data.frame", lapply(split(f_and_vpuf, f_and_vpuf$StockAndVar), function(x) {
     x$diff <- c(0, diff(as.numeric(x$value))) # + mean(x$value)
     x })  
     )
     
     dat$col <- factor(ifelse(dat$diff > 0, 2, 1))
 
 #!#!#!#!#!#!#
 #! PAPER
 #!#!#!#!#!#!#

   namefile <- paste0("ts_anomalies_f_and_",a_var,"_for_pel_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 7000; a_height=3500
  tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)
   spp <- c("SPR.2232", "MAC.nsea") # select those with significant cross-correlation
   plots <- list(NULL)
   count <- 0
   for (sp in 1: length(spp)){
     count <- count +1
     plots[[count]] <-  ggplot(dat[dat$Var=="F/FMSY" & dat$Stock==spp[sp],], aes(x=Year, y=diff,  group=StockAndVar, fill=col)) +   labs(title="",x="Year", y = paste("F/FMSY", spp[sp])) +
       geom_bar(stat = "identity", show.legend=FALSE)  + scale_fill_manual(values=c("blue", "red"))  + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
     count <- count +1
     plots[[count]] <- ggplot(dat[dat$Var=="CPUF" & dat$Stock==spp[sp],], aes(x=Year, y=diff,  group=StockAndVar, fill=col)) +   labs(title="",x="Year", y = paste("CPUF", spp[sp])) +
       geom_bar(stat = "identity", show.legend=FALSE)  + scale_fill_manual(values=c("blue", "red"))  + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
} 
    cowplot::plot_grid(plotlist = plots, ncol = 2,
                   align = 'v', labels = rep(c("F/FMSY", "CPUF"), each= length(sp)))
dev.off()



  namefile <- paste0("ts_f_and_",a_var,"_for_pel_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 9000; a_height=3500
  tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)

  dat <- f_and_vpuf[f_and_vpuf$Stock%in%c( "HER.nsea", "MAC.nsea", "SPR.2232"),]
  dat$Region <- factor(dat$Region)

    # do al list of plot to avoid using facet_wrap(~Region, scales="free_y")
    ggList <- lapply(split(dat, dat$Region), function(i) {
       ggplot(i, aes(x=Year, y=value,  group=StockAndVar, color=StockAndVar)) +   labs(title="",x="Year", y = paste0(a_var, " or F/FMSY")) +
       geom_line(size=2)  + scale_color_brewer(palette="Paired") + theme_minimal()  + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) # + ylim(c(0,10))
    } )
  # plot as grid in 1 columns
  cowplot::plot_grid(plotlist = ggList, ncol = 2,
                   align = 'v', labels = levels(long$Region))

  dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## CROSS CORRELATION ??
 f_and_vpuf <- f_and_vpuf[!is.na(f_and_vpuf$value),]
 f_and_vpuf$value_detrended <- NA
 dd <- lapply(split(f_and_vpuf, f=f_and_vpuf$Stock), function(x){
         x<-lapply(split(x, f=x$Var), function(x){
          print(x)
           if(nrow(x)>0) x$value_detrended <- x$value -  (predict(lm(x$value~as.numeric(x$Year))))
          return(x)
        })
      x <- do.call("rbind", x)
      return(x)
     })
    x <- do.call("rbind", dd)



# detrending on F/FMSY ts given we know the MP is targetting a value of 1 deliberately
 spp <- c("SPR.2232", "HER.nsea", "MAC.nsea")
 a_width <- 3000; a_height=1000
  tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  "crosscorrelation.tif"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=400, compression = c("lzw"))
 par(mfrow=c(1, length(spp)))
 for (a_spp in spp)
  ccf(x[x$Var=="F/FMSY" & x$Year %in% as.character(2012:2019) & x$Stock %in% a_spp,"value"],  # ideally we would detrended F/FMSY ts but the ts is too short that it creates misleading outcomes if we do so (given we are close to 1 in recent years...)
       x[x$Var==a_var & x$Year %in% as.character(2012:2019) & x$Stock %in% a_spp,"value"],
         lag.max = 5, type = c("correlation"),
     plot = TRUE, na.action = na.pass, xlab="", main = a_spp, ci.type = "white")
dev.off()




 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # SMALL VESSELS (0-12m)
 # cross with energy use and economic efficiency from the Danish fleet
#a_var <- "VPUF"
a_var <- "CPUF"
  if(a_var=="VPUF"){ load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("AggregatedEflaloWithSmallVidsAndStocks",years[1],"-",years[length(years)],".RData", sep="")))
  }else{load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("AggregatedEflaloWith",a_var,"SmallVidsAndStocks",years[1],"-",years[length(years)],".RData", sep="")))}

  # x
  xx <- tapply(x$value, list(x$Stock, x$Year), mean, na.rm=TRUE)

  xx <- cbind.data.frame(xx, Stock=rownames(xx))
  long <- melt(setDT(xx[, c("Stock", as.character(years))]), id.vars = c("Stock"), variable.name = "Year")
  long <- as.data.frame(long[complete.cases(long),])

  #long$value <- long$value /5 ## CAUTION ##

  f_and_vpuf <- rbind.data.frame(
    cbind.data.frame(saeu[,c("Stock","Year", "value")], Var="F/FMSY"),
    cbind.data.frame(long[,c("Stock","Year", "value")], Var=a_var)
    )

     f_and_vpuf$Region <- sapply(strsplit(as.character(f_and_vpuf$Stock), split="\\."), function(x) x[2])
   f_and_vpuf$StockAndVar <- paste(f_and_vpuf$Stock,f_and_vpuf$Var, sep=".")

   # Samll vessels
  dat <- f_and_vpuf[f_and_vpuf$Stock%in%c("COD.nsea", "PLE.nsea", "SOL.nsea", "COD.2224", "PLE.2123"),]
  dat$Region <- factor(dat$Region)
 
  collected_data <- rbind.data.frame (collected_data, cbind.data.frame(dat, Seg="SmallVessels")) 

    # compute diff for figuring out the anomalies
  dat<- do.call("rbind.data.frame", lapply(split(f_and_vpuf, f_and_vpuf$StockAndVar), function(x) {
     x$diff <- c(0, diff(as.numeric(x$value))) # + mean(x$value)
     x })  
     )
     
     dat$col <- factor(ifelse(dat$diff > 0, 2, 1))
 
 #!#!#!#!#!#!#
 #! PAPER
 #!#!#!#!#!#!#

   namefile <- paste0("ts_anomalies_f_and_",a_var,"_for_smallvids_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 7000; a_height=3500
  tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)
   spp <- c("SOL.nsea", "COD.2224") # select those with significant cross-correlation
   plots <- list(NULL)
   count <- 0
   for (sp in 1: length(spp)){
     count <- count +1
     plots[[count]] <-  ggplot(dat[dat$Var=="F/FMSY" & dat$Stock==spp[sp],], aes(x=Year, y=diff,  group=StockAndVar, fill=col)) +   labs(title="",x="Year", y = paste("F/FMSY", spp[sp])) +
       geom_bar(stat = "identity", show.legend=FALSE)  + scale_fill_manual(values=c("blue", "red"))  + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
     count <- count +1
     plots[[count]] <- ggplot(dat[dat$Var=="CPUF" & dat$Stock==spp[sp],], aes(x=Year, y=diff,  group=StockAndVar, fill=col)) +   labs(title="",x="Year", y = paste("CPUF", spp[sp])) +
       geom_bar(stat = "identity", show.legend=FALSE)  + scale_fill_manual(values=c("blue", "red"))  + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  # + ylim(c(0,10))
} 
    cowplot::plot_grid(plotlist = plots, ncol = 2,
                   align = 'v', labels = rep(c("F/FMSY", "CPUF"), each= length(sp)))
dev.off()                                       0


 

  namefile <- paste0("ts_f_and_",a_var,"_for_lgbkonly_gridcells",years[1],"-",years[length(years)],".tif")
  a_width <- 9000; a_height=3500
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggplot2)

  dat <- f_and_vpuf[f_and_vpuf$Stock%in%c("COD.nsea", "PLE.nsea", "SOL.nsea", "COD.2224", "PLE.2123"),]
  dat$Region <- factor(dat$Region)

    # do al list of plot to avoid using facet_wrap(~Region, scales="free_y")
    ggList <- lapply(split(dat, dat$Region), function(i) {
       ggplot(i, aes(x=Year, y=value,  group=StockAndVar, color=StockAndVar)) +   labs(title="",x="Year", y = paste0(a_var, " or F/FMSY")) +
       geom_line(size=2)  + scale_color_brewer(palette="Paired") + theme_minimal()   + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))   # + ylim(c(0,10))
    } )
  # plot as grid in 1 columns
  cowplot::plot_grid(plotlist = ggList, ncol = 2,
                   align = 'v', labels = levels(long$Region))

  dev.off()



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## CROSS CORRELATION ??
 f_and_vpuf <- f_and_vpuf[!is.na(f_and_vpuf$value),]
 f_and_vpuf$value_detrended <- NA
 dd <- lapply(split(f_and_vpuf, f=f_and_vpuf$Stock), function(x){
         x<-lapply(split(x, f=x$Var), function(x){
          print(x)
           if(nrow(x)>0) x$value_detrended <- x$value  -  (predict(lm(x$value~as.numeric(x$Year))))
          return(x)
        })
      x <- do.call("rbind", x)
      return(x)
     })
    x <- do.call("rbind", dd)


 # detrending on F/FMSY ts given we know the MP is targetting a value of 1 deliberately
 spp <- c("COD.nsea", "PLE.nsea", "SOL.nsea", "COD.2224", "PLE.2123")
 a_width <- 4000; a_height=1000
  tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  "crosscorrelation.tif"),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=400, compression = c("lzw"))
 par(mfrow=c(1, length(spp)))
 for (a_spp in spp)
  ccf(x[x$Var=="F/FMSY" & x$Year %in% as.character(2012:2019) & x$Stock %in% a_spp,"value"],  # ideally we would detrended F/FMSY ts but the ts is too short that it creates misleading outcomes if we do so (given we are close to 1 in recent years...)
       x[x$Var==a_var & x$Year %in% as.character(2012:2019) & x$Stock %in% a_spp,"value"],
         lag.max = 5, type = c("correlation"),
     plot = TRUE, na.action = na.pass, xlab="", main = a_spp, ci.type = "white")
 dev.off()






###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
 # a plot for a paper 
 #save(collected_data, file=file.path(getwd(), "outputs2020","forFFMSYvsCPUF.RData"))
 load(file=file.path(getwd(), "outputs2020","forFFMSYvsCPUF.RData")) # collected_data
 
 
 
  a_width <- 4800; a_height=4800
  namefile <- "CPUFvsFFMSY.tif"
  tiff(filename=file.path(getwd(), "outputs2020",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 spp <- c("COD.nsea",  "COD.2224", "CSH","DAB","ELE","FLE","HAD.nsea","HER.nsea","HKE.nsea","HOM","LEM","MAC.nsea","MON","MUS","NEP.kask","NOP","PLE.nsea","PLE.2123","POK.nsea","PRA.nsea", "SAN","SOL.nsea","SOL.2024","SPR.2232","TUR","WHB","WIT","WHG","OTH")
 color_species <- c("#E69F00", "#E69F05", "hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                   "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                   "green", "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#008B7B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")
  Stocknames <-  c("COD.nsea"="North Sea cod", "COD.2224"="west Baltic cod", "CSH"="brown shrimp", "DAB"="dab", "ELE"="eel", "FLE"="flounder",
                        "HAD"="haddock",  "HAD.nsea"="North Sea haddock", "HER"="herring", "HER.nsea"="North Sea herring", "HKE"="hake","HKE.nsea"="North Sea hake", 
                        "HOM"="Horse mackerel", "LEM"="lemon sole",
                         "MAC"="mackerel", "MAC.nsea"="North Sea mackerel", "MON"="monkfish", "MUS"="mussel", "NEP"="Nephrops", "NEP.kask"="Kattegat Nephrops", 
                         "NOP"="Norway pout", "PLE"="plaice", "PLE.nsea"="North Sea plaice", "PLE.2123"="Kattegat plaice",
                         "POK"="saithe", "POK.nsea"="North Sea saithe", "PRA"="boreal shrimp", "PRA.nsea"="North Sea boreal shrimp", "SAN"="sandeel", "SOL"="sole","SOL.nsea"="North Sea sole",  
                         "SOL.2024"="Baltic sole", "SPR"="sprat", "SPR.2232"="Baltic sprat", "TUR"="turbot", "WHB"="blue whiting",
                          "WIT"="witch flounder", "WHG"="whiting", "OTH"="other",
                          "COC"="cokle", "OYF"="oyster", "LUM"="lumpfish", "SAL"="salmon", "BLL"="brill", "GAR"="garfish")                         
  some_color_speciesnames <- c("cod"="#E69F00", "brown shrimp"="hotpink", "dab"="#56B4E9", "eel"="#F0E442", "flounder"="green",
                        "haddock"="#0072B2", "herring"="mediumorchid4", "hake"="#CC79A7","Horse mackerel"="indianred2", "lemon sole"="#EEC591",
                         "mackerel"="#458B00", "monkfish"="#F0F8FF", "mussel"="black", "Nephrops"="#e3dcbf", "Norway pout"="#CD5B45", "plaice"="lightseagreen",
                         "saithe"="#6495ED", "boreal shrimp"="#CDC8B1", "sandeel"="#00FFFF", "sole"="#8B0000", "sprat"="#008B8B", "turbot"="#A9A9A9", "blue whiting"="#76a5c4",
                          "witch flounder"="red", "whiting"="yellow", "other"="blue",
                          "cockle"="#108291", "oyster"="#6a9110", "lumpfish"="red", "salmon"="#c2a515", "brill"="cyan", "garfish"="grey")

the_colors <- cbind(spp, color_species) ; rownames(the_colors) <- spp 
 # a quick visual
 par(mfrow=c(2,2))
 par(mar=c(5,4,2.2,1))
 dat1 <- collected_data[collected_data$Seg =="BottomTrawl" & collected_data$Stock %in% c("COD.nsea", "PLE.nsea", "SOL.nsea", "PRA.nsea", "HER.nsea", "POK.nsea", "HAD.nsea", "HKE.nsea"), ]
 dat2 <- collected_data[collected_data$Seg =="BottomTrawl" & collected_data$Stock %in% c("NEP.kask", "COD.2224", "PLE.2123", "SOL.2024"), ]
 dat3 <- collected_data[collected_data$Seg =="PelagicTrawl" & collected_data$Stock %in% c("HER.nsea", "MAC.nsea", "SPR.2232"), ]
 dat4 <- collected_data[collected_data$Seg =="SmallVessels" & collected_data$Stock %in% c("COD.2224", "COD.nsea", "PLE.2123", "PLE.nsea", "SOL.nsea"), ]
 # plot1
 plot(0,0, xlab="F/FMSY", ylab="CPUF", cex.lab=1.2, axes=FALSE, xlim=range(dat1[dat1$Var=="F/FMSY" & dat1$Year %in% 2005:2018, "value"]), ylim=c(0,2), type="n")
 mtext(side=3,paste("(","a", ")", sep=''),line=1.0, adj=0, cex=1.5)
 for (stk in dat1$Stock)
   {
   x <- dat1[dat1$Stock==stk,] 
   yrange <- intersect (x[x$Var=="F/FMSY", "Year"], x[x$Var=="CPUF", "Year"])
   points(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"],  x[x$Var=="CPUF" & x$Year %in% yrange, "value"], type="p", pch=16, xlab="F/FMSY", ylab="CPUF", col=the_colors[stk,2])
   lines(lowess(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"], x[x$Var=="CPUF" & x$Year %in% yrange, "value"]),  col=the_colors[stk,2], lwd=2.5, f=0.9)
   }
   axis(1);axis(2, las=2) ; box() 
   legend("topright", legend= Stocknames[unique(dat1$Stock)], lty=1, lwd=2, col=as.character(the_colors[unique(dat1$Stock),2]), bty="n")
 # plot2
 plot(0,0, xlab="F/FMSY", ylab="CPUF", cex.lab=1.2, axes=FALSE, xlim=range(dat2[dat2$Var=="F/FMSY" & dat2$Year %in% 2005:2018, "value"]), ylim=range(dat2[dat2$Var=="CPUF" & dat2$Year %in% 2005:2018, "value"]), type="n")
 mtext(side=3,paste("(","b", ")", sep=''),line=1.0, adj=0, cex=1.5)
 for (stk in dat2$Stock)
   {
   x <- dat2[dat2$Stock==stk,] 
   yrange <- intersect (x[x$Var=="F/FMSY", "Year"], x[x$Var=="CPUF", "Year"])
   points(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"],  x[x$Var=="CPUF" & x$Year %in% yrange, "value"], type="p", pch=16, xlab="F/FMSY", ylab="CPUF", col=the_colors[stk,2])
   lines(lowess(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"], x[x$Var=="CPUF" & x$Year %in% yrange, "value"]),  col=the_colors[stk,2], lwd=2.5, f=0.9)
   }
   axis(1);axis(2, las=2)  ; box()
   legend("topleft", legend= Stocknames[unique(dat2$Stock)], lty=1, lwd=2, col=as.character(the_colors[unique(dat2$Stock),2]), bty="n")
 # plot3
 plot(0,0, xlab="F/FMSY", ylab="CPUF", cex.lab=1.2, axes=FALSE, xlim=range(dat3[dat3$Var=="F/FMSY", "value"]), ylim=range(dat3[dat3$Var=="CPUF", "value"]), type="n")
 mtext(side=3,paste("(","c", ")", sep=''),line=1.0, adj=0, cex=1.5)
 for (stk in dat3$Stock)
   {
   x <- dat3[dat3$Stock==stk,] 
   yrange <- intersect (x[x$Var=="F/FMSY", "Year"], x[x$Var=="CPUF", "Year"])
   points(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"],  x[x$Var=="CPUF" & x$Year %in% yrange, "value"], type="p", pch=16, xlab="F/FMSY", ylab="CPUF", col=the_colors[stk,2])
   lines(lowess(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"], x[x$Var=="CPUF" & x$Year %in% yrange, "value"]),  col=the_colors[stk,2], lwd=2.5, f=0.9)
   }
   axis(1);axis(2, las=2) ; box() 
   legend("topleft", legend= Stocknames[unique(dat3$Stock)], lty=1, lwd=2, col=as.character(the_colors[unique(dat3$Stock),2]), bty="n")
 # plot4
 plot(0,0, xlab="F/FMSY", ylab="CPUF", cex.lab=1.2, axes=FALSE, xlim=range(dat4[dat4$Var=="F/FMSY", "value"]), ylim=range(dat4[dat4$Var=="CPUF", "value"]), type="n")
 mtext(side=3,paste("(","d", ")", sep=''),line=1.0, adj=0, cex=1.5)
 for (stk in dat4$Stock)
   {
   x <- dat4[dat4$Stock==stk,] 
   yrange <- intersect (x[x$Var=="F/FMSY", "Year"], x[x$Var=="CPUF", "Year"])
   points(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"],  x[x$Var=="CPUF" & x$Year %in% yrange, "value"], type="p", pch=16, xlab="F/FMSY", ylab="CPUF", col=the_colors[stk,2])
   lines(lowess(x[x$Var=="F/FMSY" & x$Year %in% yrange, "value"], x[x$Var=="CPUF" & x$Year %in% yrange, "value"]),  col=the_colors[stk,2], lwd=2.5, f=0.9)
   }
   axis(1);axis(2, las=2) ; box() 
   legend("bottomright", legend= Stocknames[unique(dat4$Stock)], lty=1, lwd=2, col=as.character(the_colors[unique(dat4$Stock),2]), bty="n")

dev.off()
      
      
      
      
      
      
      
      
      
      
        