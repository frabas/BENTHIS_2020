
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


if(FALSE){
years <- 2012:2019
AllAggResultsBot <- NULL
for (y in years){
  # bottom contacting gears dem and pel
   load(file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_", y, ".RData", sep="")))
    aggResult$totkg <-  apply(aggResult[,grepl("LE_KG_", colnames(aggResult))], 1, sum)
    aggResult$toteuros <-  apply(aggResult[,grepl("LE_EURO_", colnames(aggResult))], 1, sum)
    AllAggResultsBot <- rbind.data.frame(
                            AllAggResultsBot,
                            cbind.data.frame(Year=y, aggResult)
                            )

 } # end y
  save(AllAggResultsBot, file=file.path(getwd(), "outputs2020",  paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBotAlly.RData", sep="")))
  ##---

AllAggResultsPel <- NULL
for (y in years){
   # pelagic gears
    load(file=file.path(getwd(), "outputs2020_pel", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_", y, ".RData", sep="")))
     aggResult$totkg <-  apply(aggResult[,grepl("LE_KG_", colnames(aggResult))], 1, sum)
     aggResult$toteuros <-  apply(aggResult[,grepl("LE_EURO_", colnames(aggResult))], 1, sum)
     AllAggResultsPel <- rbind.data.frame(
                            AllAggResultsPel,
                            cbind.data.frame(Year=y, aggResult)
                            )

} # end y
  save(AllAggResultsPel, file=file.path(getwd(), "outputs2020_pel",  paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPelAlly.RData", sep="")))



} # end FALSE


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # general tables  and plot
   load(file=file.path(getwd(), "outputs2020_pel",  paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBotAlly.RData", sep="")))
  
  # BOTTOM CONTACTING GEARS
  # Fuel use per metier over the period
  dd <- round(tapply(AllAggResultsBot$LE_KG_LITRE_FUEL, list(AllAggResultsBot$LE_MET, AllAggResultsBot$Year ), sum))/1e6 # millions litre fuel
  dd  <- cbind.data.frame(dd, AllMLitres=apply(dd, 1, mean, na.rm=TRUE))
  dd   <- cbind.data.frame(dd, AllMLitres_cv=apply(dd, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  dd2 <- round(tapply(AllAggResultsBot$totkg, list(AllAggResultsBot$LE_MET, AllAggResultsBot$Year ), sum))/1e6 # '000 tons
  dd2  <- cbind.data.frame(dd2, AllmTons=apply(dd2, 1, mean, na.rm=TRUE))
  dd2   <- cbind.data.frame(dd2, AllmTons_cv=apply(dd2, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  dd3 <- round(tapply(AllAggResultsBot$toteuros, list(AllAggResultsBot$LE_MET, AllAggResultsBot$Year ), sum)/1e6,2) # millions
  dd3  <- cbind.data.frame(dd3, AllMEuros=apply(dd3, 1, mean, na.rm=TRUE))
  dd3   <- cbind.data.frame(dd3, AllMEuros_cv=apply(dd3, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  all_indic <- cbind.data.frame( AllMLitres=dd[,"AllMLitres"], AllMLitres_cv=dd[,"AllMLitres_cv"],
                                 AllmTons=dd2[,"AllmTons"] , AllmTons_cv=dd2[,"AllmTons_cv"],
                                 AllMEuros=dd3[,"AllMEuros"], AllMEuros_cv=dd3[,"AllMEuros_cv"])
  rownames(all_indic) <- rownames(dd)
  all_indic <- orderBy(~ - AllMLitres, all_indic)
  
  top5 <- rownames(all_indic[1:5,])


  dd$LE_MET <- rownames(dd)
  library(data.table)
  long <- melt(setDT(dd[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long$Var <- "MLitres"
  
  dd2$LE_MET <- rownames(dd2)
  library(data.table)
  long2 <- melt(setDT(dd2[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long2$Var <- "mTons"

  dd3$LE_MET <- rownames(dd3)
  library(data.table)
  long3 <- melt(setDT(dd3[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long3$Var <- "MEuros"

  a_data <- rbind.data.frame(long, long2, long3)
  a_data <- as.data.frame(a_data)

  a_data_to_plot <- a_data[a_data$LE_MET %in% top5,]
  a_data_to_plot$LE_MET <- factor(a_data_to_plot$LE_MET)
  a_data_to_plot$LE_MET <- with(a_data_to_plot, reorder(LE_MET,value, median)) # reorder

 # plot
 a_width <- 5000 ; a_height <- 2000
 namefile <- paste0("boxplot_top5_metiers_bottom_contacting_gears",years[1], years[length(years)],".tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                    units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  p <- ggplot(a_data_to_plot, aes(x=LE_MET, y=value, fill=Var)) +    coord_flip() +   labs(title="",x="Top 5 Fleet-segments", y = "Value") +
    geom_boxplot(outlier.colour="black", outlier.shape="", outlier.size=2, notch=FALSE) + ylim(c(0,75)) + scale_fill_brewer(palette="RdBu") + theme_minimal()
  print(p)
  dev.off()
  

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # PELAGIC GEARS
   load(file=file.path(getwd(), "outputs2020_pel",  paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPelAlly.RData", sep="")))

  # Fuel use per metier over the period
  dd <- round(tapply(AllAggResultsPel$LE_KG_LITRE_FUEL, list(AllAggResultsPel$LE_MET, AllAggResultsPel$Year ), sum))/1e6 # millions litre fuel
  dd  <- cbind.data.frame(dd, AllMLitres=apply(dd, 1, mean, na.rm=TRUE))
  dd   <- cbind.data.frame(dd, AllMLitres_cv=apply(dd, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  dd2 <- round(tapply(AllAggResultsPel$totkg, list(AllAggResultsPel$LE_MET, AllAggResultsPel$Year ), sum))/1e6 # '000 tons
  dd2  <- cbind.data.frame(dd2, AllmTons=apply(dd2, 1, mean, na.rm=TRUE))
  dd2   <- cbind.data.frame(dd2, AllmTons_cv=apply(dd2, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  dd3 <- round(tapply(AllAggResultsPel$toteuros, list(AllAggResultsPel$LE_MET, AllAggResultsPel$Year ), sum)/1e6,2) # millions
  dd3  <- cbind.data.frame(dd3, AllMEuros=apply(dd3, 1, mean, na.rm=TRUE))
  dd3   <- cbind.data.frame(dd3, AllMEuros_cv=apply(dd3, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  all_indic <- cbind.data.frame( AllMLitres=dd[,"AllMLitres"], AllMLitres_cv=dd[,"AllMLitres_cv"],
                                 AllmTons=dd2[,"AllmTons"] , AllmTons_cv=dd2[,"AllmTons_cv"],
                                 AllMEuros=dd3[,"AllMEuros"], AllMEuros_cv=dd3[,"AllMEuros_cv"])
  rownames(all_indic) <- rownames(dd)
  all_indic <- orderBy(~ - AllMLitres, all_indic)

  top5 <- rownames(all_indic[1:5,])


  dd$LE_MET <- rownames(dd)
  library(data.table)
  long <- melt(setDT(dd[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long$Var <- "MLitres"

  dd2$LE_MET <- rownames(dd2)
  library(data.table)
  long2 <- melt(setDT(dd2[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long2$Var <- "mTons"

  dd3$LE_MET <- rownames(dd3)
  library(data.table)
  long3 <- melt(setDT(dd3[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long3$Var <- "MEuros"

  a_data <- rbind.data.frame(long, long2, long3)
  a_data <- as.data.frame(a_data)

  a_data_to_plot <- a_data[a_data$LE_MET %in% top5,]
  a_data_to_plot$LE_MET <- factor(a_data_to_plot$LE_MET)
  a_data_to_plot$LE_MET <- with(a_data_to_plot, reorder(LE_MET,value, median)) # reorder

 # plot
 a_width <- 5000 ; a_height <- 2000
 library(ggplot2)
 namefile <- paste0("boxplot_top5_metiers_pelagic_gears",years[1], years[length(years)],".tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                    units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  p <- ggplot(a_data_to_plot, aes(x=LE_MET, y=value, fill=Var)) +    coord_flip() +   labs(title="",x="Top 5 Fleet-segments", y = "Value") +
    geom_boxplot(outlier.colour="black", outlier.shape="", outlier.size=2, notch=FALSE) + ylim(c(0,200)) + scale_fill_brewer(palette="RdBu") + theme_minimal()
  print(p)
  dev.off()


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## SMALL VESSELS (0-12m)
  load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("AggregatedEflaloWithSmallVids.RData", sep="")))
   #eflalo


  # Fuel use per metier over the period
  dd <- round(tapply(eflalo$LE_KG_LITRE_FUEL, list(eflalo$LE_MET, eflalo$Year ), sum))/1e6 # millions litre fuel
  dd  <- cbind.data.frame(dd, AllMLitres=apply(dd, 1, mean, na.rm=TRUE))
  dd   <- cbind.data.frame(dd, AllMLitres_cv=apply(dd, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  dd2 <- round(tapply(eflalo$LE_KG_SPECS, list(eflalo$LE_MET, eflalo$Year ), sum))/1e6 # '000 tons
  dd2  <- cbind.data.frame(dd2, AllmTons=apply(dd2, 1, mean, na.rm=TRUE))
  dd2   <- cbind.data.frame(dd2, AllmTons_cv=apply(dd2, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  dd3 <- round(tapply(eflalo$LE_EURO_SPECS, list(eflalo$LE_MET, eflalo$Year ), sum)/1e6,2) # millions
  dd3  <- cbind.data.frame(dd3, AllMEuros=apply(dd3, 1, mean, na.rm=TRUE))
  dd3   <- cbind.data.frame(dd3, AllMEuros_cv=apply(dd3, 1, FUN=function(x) sqrt(var(x, na.rm=TRUE))/mean(x, na.rm=TRUE)))

  all_indic <- cbind.data.frame( AllMLitres=dd[,"AllMLitres"], AllMLitres_cv=dd[,"AllMLitres_cv"],
                                 AllmTons=dd2[,"AllmTons"] , AllmTons_cv=dd2[,"AllmTons_cv"],
                                 AllMEuros=dd3[,"AllMEuros"], AllMEuros_cv=dd3[,"AllMEuros_cv"])
  rownames(all_indic) <- rownames(dd)
  all_indic <- orderBy(~ - AllMLitres, all_indic)

  top5 <- rownames(all_indic[1:5,])


  dd$LE_MET <- rownames(dd)
  library(data.table)
  long <- melt(setDT(dd[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long$Var <- "MLitres"

  dd2$LE_MET <- rownames(dd2)
  library(data.table)
  long2 <- melt(setDT(dd2[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long2$Var <- "mTons"

  dd3$LE_MET <- rownames(dd3)
  library(data.table)
  long3 <- melt(setDT(dd3[, c("LE_MET", as.character(years))]), id.vars = c("LE_MET"), variable.name = "Year")
  long3$Var <- "MEuros"

  a_data <- rbind.data.frame(long, long2, long3)
  a_data <- as.data.frame(a_data)

  a_data_to_plot <- a_data[a_data$LE_MET %in% top5,]
  a_data_to_plot$LE_MET <- factor(a_data_to_plot$LE_MET)
  a_data_to_plot$LE_MET <- with(a_data_to_plot, reorder(LE_MET,value, median)) # reorder

 # plot
 a_width <- 5000 ; a_height <- 2000
 namefile <- paste0("boxplot_top5_metiers_small_vessels",years[1], years[length(years)],".tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                    units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  p <- ggplot(a_data_to_plot, aes(x=LE_MET, y=value, fill=Var)) +    coord_flip() +   labs(title="",x="Top 5 Fleet-segments", y = "Value") +
    geom_boxplot(outlier.colour="black", outlier.shape="", outlier.size=2, notch=FALSE) + ylim(c(0,15)) + scale_fill_brewer(palette="RdBu") + theme_minimal()
  print(p)
  dev.off()



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # assign area coding to retrieve stock from species
  
  load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("AggregatedEflaloWithSmallVids.RData", sep="")))
  eflalo <- cbind.data.frame(eflalo,
                  vmstools::ICESrectangle2LonLat(statsq=eflalo$LE_RECT, midpoint=TRUE)
                  )
  eflalo <- eflalo[!is.na(eflalo$SI_LATI),]  # some leaks of data there. i.e. total effort is not complete, so do not use it
  x <- eflalo
  
  
  # code F_SUBAREA (time consuming code...)
   # Convert all points first to SpatialPoints first
   library(rgdal)
   library(raster)
   # search in Baltic and North Sea                      # do not use "27.3.a" "27.3.d","27.3.b, c" cause overruling finer division...
   library(rgdal)
   fao_areas  <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
   fao_areas  <- fao_areas[ grepl("27", fao_areas$F_AREA) & fao_areas$F_SUBAREA %in% c("27.3", "27.4", "27.2", "27.5","27.6", "27.7",  "27.8", "27.14") & fao_areas$F_LEVEL!="MAJOR",] # caution with the MAJOR overidding the over()
   fao_areas <- fao_areas[fao_areas$F_CODE %in% c("27.8.c","27.8.b","27.8.e","27.8.d","27.8.a","27.7.j","27.7.h","27.7.f","27.7.g","27.7.k","27.4.c","27.7.c","27.4.b","27.6.b",
                                                  "27.5.b","27.5.a","27.14.b","27.2.a","27.2.b","27.14.a","27.7.e","27.7.d","27.6.a","27.7.b","27.7.a","27.4.a","27.8.e.1",
                                                  "27.8.d.1","27.8.d.2","27.7.j.1","27.7.k.1","27.7.k.2","27.7.j.2","27.7.c.1","27.7.c.2","27.3.d.24","27.3.b.23","27.3.d.26","27.6.b.1",
                                                  "27.6.b.2","27.5.b.1","27.5.a.1", "27.14.b.1","27.5.a.2","27.14.b.2","27.2.a.1","27.2.b.1","27.2.b.2","27.8.e.2","27.5.b.2","27.2.a.2","27.3.c.22","27.3.d.27","27.3.d.28","27.3.d.29",
                                                  "27.3.d.32","27.3.d.30","27.3.d.31","27.3.d.25","27.3.a.21","27.3.a.20","27.5.b.1.a","27.5.b.1.b"),]
   an <- function(x) as.numeric(as.character(x))
   coords <- SpatialPoints(cbind(SI_LONG=an(x[, "SI_LONG"]), SI_LATI=an(x[, "SI_LATI"])))
   fao_areas <- spTransform(fao_areas, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))    # convert to longlat
   projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
   idx <- over(coords, fao_areas)
   x$F_CODE <- idx[,"F_CODE"]

   # check
   plot(fao_areas[fao_areas$F_CODE %in% c("27.3.d","27.3.b, c","27.3.d.24","27.3.b.23"),])
   points( x[is.na(x$F_CODE), c("SI_LONG","SI_LATI")], col=3, pch="+")
   #=> pblm with discret pos and coastline...so brute search below to fix:
   for(multi in seq(1,3,by=0.2))  {
   for(a_sign1 in c(-1,1))  {
   for(a_sign2 in c(-1,1))  {
   dx <- 0.2
   dy <- 0.2
   print(length(which(is.na(x$F_CODE))))

   coords <- SpatialPoints(cbind(SI_LONG=an(x[ which(is.na(x$F_CODE)), "SI_LONG"])+dx*multi*a_sign1, SI_LATI=an(x[ which(is.na(x$F_CODE)), "SI_LATI"])+dy*multi*a_sign2))
   projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
   idx <- over(coords, fao_areas)
   eflalo[which(is.na(x$F_CODE)),]$F_CODE <- idx[,"F_CODE"]

   length(which(is.na(x$F_CODE)))
  }}}
  x <- x[!is.na(x$F_CODE),] # remove very few records



  #  convert to long
  x$ID <- paste0(x$VE_REF, x$Year, x$LE_ID)
  library(data.table)
  long <- melt(setDT(x[, c("ID", "VE_REF", "FT_DDATIM", "VE_LEN", "VE_KW", "VE_TON", "LE_MET", "F_CODE", "Year", colnames(x)[grepl("LE_VPUF_", colnames(x))])]),
                 id.vars = c("ID", "VE_REF","FT_DDATIM", "VE_LEN", "VE_KW", "VE_TON", "LE_MET", "F_CODE", "Year"), variable.name = "Var")
  long$Species <- sapply(strsplit(as.character(long$Var), split="_"), function (x) x[3])


  x<- long
  
  # find out areas to Retrieve Stocks from Species
   x$code_area <- as.character(x$F_CODE)
   x[(x$code_area %in% c('27.4.a', '27.4.b', '27.4.c', '27.4')), 'code_area'] <- 'nsea'
   x[(x$code_area %in% c('27.3.a', '27.3.a.21','27.3.a.20')), 'code_area'] <- 'kask'
   x[(x$code_area %in% c('27.3.c.22',  '27.3.d.24', '27.3.b, c','27.3.d', '27.3.b.23')), 'code_area'] <- '2224'
   x[(x$code_area %in% c('27.3.d.25', '27.3.d.26', '27.3.d.28', '27.3.d.28.1', '27.3.d.28.2', '27.3.d.28_NK','27.3.d.29', '27.3.d.32', '27.3.d.30', '27.3.d.31')), 'code_area'] <- '2532'

   x$Stock <- paste0(x$Species, ".", x$code_area)
   
  # correct names for special cases (those across management areas)
   x[ x$Species %in% c("COD") & (x$F_CODE %in% c('27.4.a', '27.4.b', '27.4.c', '27.4')), "Stock"] <- 'COD.nsea'
   x[ x$Species %in% c("COD")& (x$F_CODE %in% c('27.3.a')), "Stock"] <- 'COD.kat'
   #head(x[x$Species=="COD.kat",])
   x[ x$Species %in% c("HER") & (x$F_CODE %in% c('27.3.a', '27.3.a.20', '27.3.c.22')), "Stock"] <- 'HER.3a22'
   x[ x$Species %in% c("HER") & (x$F_CODE %in% c('27.3.d.28.1')), "Stock"] <- 'HER.281'
   x[ x$Species %in% c("HER") & (x$F_CODE %in% c('27.3.d.30','27.3.d.31')), "Species"] <- 'HER.3031'
   x[ x$Species %in% c("PLE") & (x$F_CODE %in% c('27.3.a.21','27.3.a.20', '27.3.c.22', '27.3.b.23')), "Stock"] <- 'PLE.2123'
   x[ x$Species %in% c("PLE") & (x$F_CODE %in% c('27.3.d.24', '27.3.d.25', '27.3.d.26', '27.3.d.28', '27.3.d.28.1', '27.3.d.28.2', '27.3.d.28_NK','27.3.d.29', '27.3.d.32', '27.3.d.30', '27.3.d.31')), "Stock"] <- 'PLE.2432'
   x[ x$Stock %in% c("BLL.2224", "BLL.2532"), "Stock"] <- 'BLL.2232'
   x[ x$Stock %in% c("TUR.2224", "TUR.2532"), "Stock"] <- 'TUR.2232'
   x[ x$Stock %in% c("DAB.2224", "DAB.2532"), "Stock"] <- 'DAB.2232'
   x[ x$Stock %in% c("SPR.2224", "SPR.2532"), "Stock"] <- 'SPR.2232'
   x[ x$Stock %in% c("WHG.2224", "WHG.2532"), "Stock"] <- 'WHG.2232'
   x[ x$Species %in% c("FLE") & (x$F_CODE %in% c('27.3.c.22')), "Stock"] <- 'FLE.2223'
   x[ x$Species %in% c("FLE") & (x$F_CODE %in% c('27.3.d.24', '27.3.d.25')), "Stock"] <- 'FLE.2425'
   x[ x$Species %in% c("FLE") & (x$F_CODE %in% c('27.3.d.26', '27.3.d.28', '27.3.d.28.1', '27.3.d.28.2')), "Stock"] <- 'FLE.2628'
   x[ x$Species %in% c("FLE") & (x$F_CODE %in% c('27.3.d.29', '27.3.d.32', '27.3.d.30', '27.3.d.31')), "Stock"] <- 'FLE.2732'
   x[ x$Species %in% c("SOL") & (x$F_CODE %in% c('27.3.a.20','27.3.c.22','27.3.d.24')), "Stock"] <- 'SOL.2024'

  
   # e.g. a table...
   tapply(x$value, list(x$Stock, x$Year), mean, na.rm=TRUE)

  #.....ready to cross with e.g. STECF CFP monitoring data
  # TODO
  
  
  