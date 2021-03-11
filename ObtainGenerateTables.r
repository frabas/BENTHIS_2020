


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


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
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
 namefile <- paste0("boxplot_top5_metiers_pelagic_gears",years[1], years[length(years)],".tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                    units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  p <- ggplot(a_data_to_plot, aes(x=LE_MET, y=value, fill=Var)) +    coord_flip() +   labs(title="",x="Top 5 Fleet-segments", y = "Value") +
    geom_boxplot(outlier.colour="black", outlier.shape="", outlier.size=2, notch=FALSE) + ylim(c(0,100)) + scale_fill_brewer(palette="RdBu") + theme_minimal()
  print(p)
  dev.off()


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # capture an export for quickmap2020.r
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




  # assign area coding to retrieve stock from species
  
  
  
  
  