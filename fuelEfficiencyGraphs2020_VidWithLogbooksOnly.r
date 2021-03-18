rm(list=ls())
library(vmstools)
library(maps)
library(mapdata)

if(.Platform$OS.type == "windows") {
 codePath  <- "D:/FBA/BENTHIS_2020/"
 dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
 outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
 polPath   <- "D:/FBA/BENTHIS/BalanceMaps"

 #a_year      <- 2005
 #a_year      <- 2006
 #a_year      <- 2007
 #a_year      <- 2008
 #a_year      <- 2009
 #a_year      <- 2010
 #a_year      <- 2011
 #a_year      <- 2012
 #a_year      <- 2013
 #a_year      <- 2014
 #a_year      <- 2015
 #a_year      <- 2016
 #a_year      <- 2017
 #a_year      <- 2018
 ##a_year      <- 2019
 }


if(FALSE){


eflalo_small_vids <- NULL
for (a_year in c(2012:2019))    {  # on WINDOWS system...

dir.create(file.path(outPath))
dir.create(file.path(outPath, a_year))

library(vmstools)


  load(file.path(dataPath,paste("eflalo_", a_year,".RData", sep=''))); # get the eflalo object
  if(a_year>=2016){
    eflalo <- formatEflalo(get(paste0("eflalo_", a_year))) # format each of the columns to the specified class
    } else{
    eflalo <- formatEflalo(get(paste0("eflalo"))) # format each of the columns to the specified class
    }


  # keep all vessels with < 12m in length (i.e. most of them without VMS)
  eflalo <- eflalo[as.numeric(as.character(eflalo$VE_LEN)) <= 11.99,]



  # marginal sums
  idx                   <- kgeur(colnames(eflalo))
  eflalo$LE_KG_SPECS    <- rowSums(eflalo[,grep("LE_KG_",colnames(eflalo))],na.rm=T)
  eflalo$LE_EURO_SPECS  <- rowSums(eflalo[,grep("LE_EURO_",colnames(eflalo))],na.rm=T)


  # Remove non-unique trip numbers
  eflalo            <- eflalo[!duplicated(paste(eflalo$LE_ID,eflalo$LE_CDAT,sep="-")),]

  # Remove impossible time stamp records
  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")

  eflalo            <- eflalo[!(is.na(eflalo$FT_DDATIM) |is.na(eflalo$FT_LDATIM)),]   # some leaks of data there.

  # Remove trip starting before 1st Jan
  # year              <- min(year(eflalo$FT_DDATIM))  # deprecated?
  eflalo            <- eflalo[eflalo$FT_DDATIM>=strptime(paste(a_year,"-01-01 00:00:00",sep=''),
                                                             "%Y-%m-%d %H:%M"),]

  # Remove records with arrival date before departure date
  eflalop           <- eflalo
  eflalop$FT_DDATIM <- as.POSIXct(paste(eflalo$FT_DDAT,  eflalo$FT_DTIME,   sep=" "),
                                tz="GMT", format="%d/%m/%Y  %H:%M")
  eflalop$FT_LDATIM <- as.POSIXct(paste(eflalo$FT_LDAT,  eflalo$FT_LTIME,   sep=" "),
                                tz="GMT", format="%d/%m/%Y  %H:%M")
  idx               <- which(eflalop$FT_LDATIM >= eflalop$FT_DDATIM)
  eflalo            <- eflalo[idx,]


  # compute effort of the trip
  eflalo                <- subset(eflalo,FT_REF != 0)
  eflalo                <- orderBy(~VE_REF+FT_DDATIM+FT_REF, data=eflalo)
  eflalo$ID             <- paste(eflalo$VE_REF,eflalo$FT_REF,sep="")
  eflalo$LE_EFF         <- an(difftime(eflalo$FT_LDATIM, eflalo$FT_DDATIM, units="hours"))
  eflalo$dummy          <- 1
  eflalo$LE_EFF         <- eflalo$LE_EFF / merge(eflalo,aggregate(eflalo$dummy,by=list(eflalo$ID),FUN=sum),by.x="ID",by.y="Group.1",all.x=T)$x


  eflalo <- cbind.data.frame(eflalo, Year=a_year)

  if (a_year==years[1]) {  eflalo_small_vids <- eflalo }

  cols <- unique(colnames(eflalo)[colnames(eflalo) %in% colnames(eflalo_small_vids)],
     colnames(eflalo_small_vids)[ colnames(eflalo_small_vids)  %in% colnames(eflalo)])

 if (a_year>years[1]) {
  eflalo_small_vids <- rbind.data.frame(eflalo_small_vids[,cols], eflalo[,cols])
  }
 } # end y
 
 save(eflalo_small_vids, file=file.path(dataPath ,paste("eflalo_small_vids_ally.RData", sep='')))

} # end FALSE

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  load(file=file.path(dataPath ,paste("eflalo_small_vids_ally.RData", sep='')))
  eflalo <- eflalo_small_vids

  years <- 2012:2019
  spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
  color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                   "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                   "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")

  some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                       "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                        "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                        "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                         "WIT"="red", "WHG"="yellow", "OTH"="blue",
                         "COC"="#108291", "OYF"="#6a9110", "LUM"="red", "SAL"="#c2a515")  # specific to small vids

  per_metier_level6 <- TRUE
  per_vessel_size <- TRUE
  per_region     <- TRUE

  # search in Baltic and North Sea
  library(rgdal)
  fao_areas  <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
  fao_areas  <- fao_areas[ grepl("27", fao_areas$F_AREA) & fao_areas$F_SUBAREA %in% c("27.3", "27.4", "27.2") & fao_areas$F_LEVEL!="MAJOR",] # caution with the MAJOR overidding the over()

  
  eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=TRUE)

  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  eflalo <- cbind.data.frame(eflalo,
                  vmstools::ICESrectangle2LonLat(statsq=eflalo$LE_RECT, midpoint=TRUE)
                  )
  eflalo <- eflalo[!is.na(eflalo$SI_LATI),]  # some leaks of data there. i.e. total effort is not complete, so do not use it


  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # find metiers lvl6-Vsize to merge
 if(per_metier_level6 && per_vessel_size){

   # code F_SUBAREA (time consuming code...)
   # Convert all points first to SpatialPoints first
   library(rgdal)
   library(raster)
   an <- function(x) as.numeric(as.character(x))
   coords <- SpatialPoints(cbind(SI_LONG=an(eflalo[, "SI_LONG"]), SI_LATI=an(eflalo[, "SI_LATI"])))
   fao_areas <- spTransform(fao_areas, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))    # convert to longlat
   projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
   idx <- over(coords, fao_areas)
   eflalo$F_SUBAREA <- idx[,"F_SUBAREA"]
   eflalo[is.na(eflalo$F_SUBAREA), "F_SUBAREA"] <- "27.4" # few points on coastline
   eflalo$F_DIVISION <- idx[,"F_DIVISION"]
   eflalo[is.na(eflalo$F_DIVISION), "F_DIVISION"] <- paste0(eflalo[is.na(eflalo$F_DIVISION), "F_SUBAREA"],".a") # few points on coastline

   # code small vs large mesh
   eflalo$target <- factor(eflalo$LE_MET) # init
   code <- sapply(strsplit(levels(eflalo$target), split="_"), function(x) x[3])
   levels(eflalo$target) <- code
   levels(eflalo$target)[levels(eflalo$target) %in% c(">=105","100-119","90-119",">=120","90-104", ">=220", "70-89", ">=157", ">=156", "110-156", "120-219", "90-99")] <- "LargeMesh"
   levels(eflalo$target)[!levels(eflalo$target) %in% "LargeMesh"] <- "SmallMesh"

   dd <- tapply(eflalo$LE_EFF, paste0(eflalo$target, "_", eflalo$F_SUBAREA, "_", eflalo$LE_MET, "_", eflalo$VesselSize), sum)


   pel <- dd[grep("SmallMesh",names(dd))]
   pel <- pel[order(pel, decreasing=TRUE)]
   oth_mets_pel <-  names(pel)[cumsum(pel)/sum(pel)>.9]
   oth_mets_pel <- c(oth_mets_pel, "NA_27.3_No_Matrix6_(0,12]", "NA_27.4_No_Matrix6_(0,12]")

   dem <- dd[grep("LargeMesh",names(dd))]
   dem <- dem[order(dem, decreasing=TRUE)]
   oth_mets_dem <-  names(dem)[cumsum(dem)/sum(dem)>.9]
   oth_mets_dem <- c(oth_mets_dem, "NA_27.3_No_Matrix6_(0,12]", "NA_27.4_No_Matrix6_(0,12]")


 }
  eflalo$LE_MET_init <- eflalo$LE_MET
  eflalo$LE_MET <-  factor(paste0(eflalo$target, "_", eflalo$F_SUBAREA, "_", eflalo$LE_MET, "_", eflalo$VesselSize))
  levels(eflalo$LE_MET)[levels(eflalo$LE_MET) %in% oth_mets_pel] <- "SmallMesh_OTHER_0_0_0"
  levels(eflalo$LE_MET)[levels(eflalo$LE_MET) %in% oth_mets_dem] <- "LargeMesh_OTHER_0_0_0"

  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # fuel comsumption
  table.fuelcons.per.engine       <-  read.table(file= file.path(dataPath, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
  linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
  eflalo$LE_KG_LITRE_FUEL         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(eflalo$VE_KW)))) * eflalo$LE_EFF # Liter per hour * effort this trip in hour




  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # OTH species

   spp <- gsub("LE_KG_", "", colnames(eflalo)[grepl("LE_KG", colnames(eflalo))])
   spp <- spp[!spp %in% c("SPECS", "LITRE_FUEL")]


   dd <- apply(eflalo[,paste0("LE_EURO_",spp)], 2, sum, na.rm=TRUE)
   dd <- dd[order(dd, decreasing=TRUE)]
   main_species <-gsub("LE_EURO_", "", names(dd[cumsum(dd)/sum(dd)<0.9]) )
   oth_species <- gsub("LE_EURO_", "", names(dd[cumsum(dd)/sum(dd)>=0.9]) )
   eflalo$LE_EURO_OTH <- apply(eflalo[,paste0("LE_EURO_",oth_species)], 1, sum, na.rm=TRUE)
   eflalo$LE_KG_OTH <- apply(eflalo[,paste0("LE_KG_",oth_species)], 1, sum, na.rm=TRUE)

   spp <- c(main_species, "OTH")
   eflalo <- eflalo[,!colnames(eflalo) %in% paste0("LE_EURO_",oth_species)]
   eflalo <- eflalo[,!colnames(eflalo) %in% paste0("LE_KG_",oth_species)]
   

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # compute some ratios (to plot across spp)
  dd <- sweep(eflalo[, paste0('LE_KG_', spp)], 1,  eflalo$LE_EFF, FUN="/")
  colnames(dd) <- paste0('LE_CPUE_', spp)
  dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
  eflalo <- cbind.data.frame (eflalo, dd)
  eflalo$CPUEallsp <- apply (eflalo[, paste0('LE_CPUE_', spp)], 1, sum, na.rm=TRUE)

  dd <- sweep(eflalo[, paste0('LE_KG_', spp)], 1,  eflalo$LE_KG_LITRE_FUEL, FUN="/")
  colnames(dd) <- paste0('LE_CPUF_', spp)
  dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
  eflalo <- cbind.data.frame (eflalo, dd)
  eflalo$CPUFallsp <- apply (eflalo[, paste0('LE_CPUF_', spp)], 1, sum, na.rm=TRUE)

  dd <- sweep(eflalo[, paste0('LE_EURO_', spp)], 1,   eflalo$LE_KG_LITRE_FUEL, FUN="/")
  colnames(dd) <- paste0('LE_VPUF_', spp)
  dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
  eflalo <- cbind.data.frame (eflalo, dd)
  eflalo$VPUFallsp <- apply (eflalo[, paste0('LE_VPUF_', spp)], 1, sum, na.rm=TRUE)



  dd <- apply (eflalo, 1, function (x) {
               idx_cols <- grepl("LE_VPUF_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_VPUF_", "", names(x[idx_cols])[idx])
               })
  eflalo$sp_with_max_vpuf <- dd

  dd <- apply (eflalo, 1, function (x) {
               idx_cols <- grepl("LE_CPUE_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_CPUE_", "", names(x[idx_cols])[idx])
               })
  eflalo$sp_with_max_cpue <- dd

  dd <- apply (eflalo, 1, function (x) {
               idx_cols <- grepl("LE_CPUF_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_CPUF_", "", names(x[idx_cols])[idx])
               })
  eflalo$sp_with_max_cpuf <- dd


  # capture an export for quickmap2020.r
  save(eflalo, file=file.path(getwd(), "outputs2020_lgbkonly", paste("AggregatedEflaloWithSmallVids.RData", sep="")))

   # just for info and a rough approximation
    total_kg1 <- tapply(eflalo$LE_KG_SPECS, list(eflalo$LE_MET), sum, na.rm=TRUE)
    total_kg2 <- tapply(eflalo$LE_KG_SPECS, list(eflalo$LE_MET, as.character(eflalo$sp_with_max_cpue)), sum, na.rm=TRUE)
    an_order <- total_kg1 [order(total_kg1, decreasing=TRUE)]
    xx<- round(total_kg2[names(an_order),])   # cumul over period


   # agg
    agg_by <- c("Year","LE_MET")

   # aggregate ("sum" if absolute value, "mean" if ratio)
    nm <- names(eflalo)
    library(data.table)
    # for sum
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('LE_EFF', nm)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort)
    DT  <- data.table(eflalo)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    a_by <- c.listquote(  agg_by )
    aggResultPerMet <- DT[,eval(eq1),by=eval(a_by)]
    aggResultPerMet <- data.frame(aggResultPerMet)
    colnames(aggResultPerMet) <- c(agg_by, colnames(eflalo)[idx.col] )
    library(doBy)
    aggResultPerMet <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=aggResultPerMet) # order to ensure same order when collating....

    # for average
    idx.col.1     <- grep('CPUEallsp', nm, fixed=TRUE)
    idx.col.2     <- grep('CPUFallsp', nm, fixed=TRUE)
    idx.col.3     <- grep('VPUFallsp', nm, fixed=TRUE)
    idx.col.cpue     <- grep('LE_CPUE_', nm)
    idx.col.cpuf     <- grep('LE_CPUF_', nm)
    idx.col.vpuf     <- grep('LE_VPUF_', nm)
    idx.col <- c(idx.col.1, idx.col.2, idx.col.3, idx.col.cpue,idx.col.cpuf, idx.col.vpuf)
    a_mean <- function(x, na.rm) mean(x[x!=0], na.rm=na.rm) # modify the mean() so that 0 are first removed....
    eq1  <- c.listquote( paste ("a_mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
    DT  <- data.table(eflalo)
    a_by <- c.listquote(  agg_by )
    aggResultPerMet2 <- DT[,eval(eq1),by=eval(a_by)]
    aggResultPerMet2 <- data.frame(aggResultPerMet2)
    colnames(aggResultPerMet2) <- c(agg_by, colnames(eflalo)[idx.col] )
    aggResultPerMet2 <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=aggResultPerMet2) # order to ensure same order when collating....

    # collate
    aggResultPerMet <- cbind(aggResultPerMet, aggResultPerMet2[,-c(1:length(agg_by))])


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
 ## QUICK & EASY TABLES
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
  # capture an export for later doing some quick table
  aggResultPerMetAlly <- aggResultPerMet
  #save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids.RData", sep=""))) 


  load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids.RData", sep="")))  # aggResultPerMetAlly
  library(doBy)
  
  spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]
  
  PercentThisStk <- aggResultPerMetAlly[paste0("LE_KG_",spp)] / apply(aggResultPerMetAlly[paste0("LE_KG_",spp)], 1, sum, na.rm=TRUE)*100
  colnames(PercentThisStk)  <- paste0("Percent_",spp)
  aggResultPerMetAlly <- cbind.data.frame (aggResultPerMetAlly, PercentThisStk)
  VarThisStk <- sweep(PercentThisStk[,colnames(PercentThisStk)]/100, 1, aggResultPerMetAlly[,"LE_KG_LITRE_FUEL"], FUN="*")
  colnames(VarThisStk)  <- paste0("LE_LITRE_",spp)
  aggResultPerMetAlly <- cbind.data.frame (aggResultPerMetAlly, VarThisStk)
     
  # a quick informative table (for kg)
  nm <- colnames(aggResultPerMetAlly)
  tot <- aggregate(aggResultPerMetAlly[, c(paste0("LE_KG_", spp), "LE_KG_LITRE_FUEL") ], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
  tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
  tot[,-1] <- round(tot[,-1]/1e6,2) # millions litres or thousand tons or euros
  head(tot, 5)

  # same but for euros
  aggResultPerMetAlly$LE_EURO_LITRE_FUEL <- aggResultPerMetAlly$LE_KG_LITRE_FUEL  # a tip for ordering
  nm <- colnames(aggResultPerMetAlly)
  tot <- aggregate(aggResultPerMetAlly[,  c(paste0("LE_EURO_", spp),"LE_EURO_LITRE_FUEL")], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
  tot <- orderBy(~ - LE_EURO_LITRE_FUEL, tot)
  tot[,-1] <- round(tot[,-1]/1e6,2) # millions litres or thousand tons or euros
  head(tot, 5)
  
  # same but for litre fuel
  aggResultPerMetAlly$LE_LITRE_FUEL <- aggResultPerMetAlly$LE_KG_LITRE_FUEL  # a tip for ordering
  nm <- colnames(aggResultPerMetAlly)
  tot <- aggregate(aggResultPerMetAlly[,  c(paste0("LE_LITRE_", spp), "LE_LITRE_FUEL")], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
  tot <- orderBy(~ - LE_LITRE_FUEL, tot)
  tot[,-1] <- round(tot[,-1]/1e6,2) # millions litres or thousand tons or euros
  head(tot, 5)
 
  nm <- colnames(aggResultPerMetAlly)
  sum_y_kg <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(Year=aggResultPerMetAlly$Year), sum)  # annual average
  nm <- colnames(sum_y_kg)
  average_y_kg <- apply(sum_y_kg[, grepl("LE_KG_", nm)], 2, mean)  # annual average
  info1 <- round(average_y_kg[order(average_y_kg, decreasing=TRUE)]/1e6,2)

  nm <- colnames(aggResultPerMetAlly)
  sum_y_euros <- aggregate(aggResultPerMetAlly[, grepl("LE_EURO_", nm)], list(Year=aggResultPerMetAlly$Year), sum)  # annual average
  nm <- colnames(sum_y_euros)
  average_y_euros <- apply(sum_y_euros[, grepl("LE_EURO_", nm)], 2, mean)  # annual average
  round(average_y_euros[order(average_y_euros, decreasing=TRUE)]/1e6,2)
  info2 <-  round(average_y_euros[order(average_y_euros, decreasing=TRUE)]/1e6,2)

  nm <- colnames(aggResultPerMetAlly)
  sum_y_litres <- aggregate(aggResultPerMetAlly[, grepl("LE_LITRE_", nm)], list(Year=aggResultPerMetAlly$Year), sum, na.rm=TRUE)  # annual average
  nm <- colnames(sum_y_litres)
  average_y_litres <- apply(sum_y_litres[, grepl("LE_LITRE_", nm)], 2, mean)  # annual average
  round(average_y_litres[order(average_y_litres, decreasing=TRUE)]/1e6,2)
  info3 <-  round(average_y_litres[order(average_y_litres, decreasing=TRUE)]/1e6,2)

  spp <- sapply(strsplit(as.character(names(info3)), split="_"), function(x) x[3])  # give the order on the plot
  spp <- spp[spp!="FUEL"]
  a_summary <- rbind.data.frame(info1[paste0("LE_KG_", spp)], info2[paste0("LE_EURO_", spp)], info3[paste0("LE_LITRE_", spp)] )
  colnames(a_summary) <- spp
  rownames(a_summary) <- c("Thousands tons", "Millions euros", "Millions litres")

  # dem
  a_width <- 5000;  a_height <- 2000
  namefile <- paste0("barplot_fuel_efficiency_smallvids_per_species.tif")
  tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
    library(data.table)
    long1 <- melt(setDT(sum_y_kg[,c("Year", paste0("LE_KG_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long2 <- melt(setDT(sum_y_euros[,c("Year", paste0("LE_EURO_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long3 <- melt(setDT(sum_y_litres[,c("Year", paste0("LE_LITRE_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long <- rbind.data.frame(long1, long2, long3)
    long$Species <- sapply(strsplit(as.character(long$Var), split="_"), function(x) x[3])
    long$VarType <- factor(sapply(strsplit(as.character(long$Var), split="_"), function(x) x[2]))
    long$value   <- long$value  /1e6 # millions
    levels(long$VarType) <- c("Millions Euros", "Thousánd Tons", "Millions Litres")
    long$Species <- with(long, reorder(Species, value, median)) # reorder
    long$Species <- factor(long$Species, levels=rev(levels(long$Species))) # reverse
    
   p <- ggplot(data=long[!long$Species %in% c("TODO"),], aes(x=Species, y=value))  +
           geom_boxplot(outlier.size = -1, fill='#A4A4A4', color="black") +  scale_color_grey() + facet_wrap(~VarType, scales="free")   + labs(y = "", x = "Species") + 
           theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  print(p)
  
  dev.off()
  #write.table(a_summary, "clipboard", sep="\t", row.names=TRUE, col.names=TRUE)



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # plot sectored per species
 
 long <- NULL
 agg <- NULL

 variables <- c("LE_KG_LITRE_FUEL", "CPUEallsp", "CPUFallsp", "VPUFallsp")
 prefixes  <- c("LE_KG_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_")

 count <- 0
 for(a_variable in variables){
    count <- count+1

    dd <- get(paste0("aggResultPerMet"))
    # get percent per stock for sectorisation


    PercentThisStk <- dd[paste0(prefixes[count],spp)] / apply(dd[paste0(prefixes[count],spp)], 1, sum, na.rm=TRUE)*100
    colnames(PercentThisStk)  <- paste0("Percent_",spp)
    dd <- cbind.data.frame (dd, PercentThisStk)
    VarThisStk <- sweep(dd[,colnames(PercentThisStk)]/100, 1, dd[,a_variable], FUN="*")
    colnames(VarThisStk)  <- spp
    dd <- cbind.data.frame (dd, VarThisStk)
    # reshape
    library(data.table)
    long <- melt(setDT(dd[,c("LE_MET",a_variable, "Year", colnames(VarThisStk))]), id.vars = c("LE_MET",a_variable, "Year"), variable.name = "Stock")

    #as.data.frame(long)
    long <- long[complete.cases(long),]




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 library(ggplot2)
 if(a_variable=="LE_KG_LITRE_FUEL") {a_ylab <- "Fuel use (litre)"; ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="CPUEallsp") {a_ylab <- "CPUE (kg per effort)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="CPUFallsp") {a_ylab <- "CPUF (kg per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFallsp") {a_ylab <- "VPUF  (euro per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFSWAallsp") {a_ylab <- "VPUFSWA  (euro per swept area)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}

  a_width <- 9000 ; a_height <- 4000
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")

 # dem
 namefile <- paste0("barplot_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)], a_comment, "_DEM.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  the_agg <- long[grep("LargeMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
  p <- ggplot(data=the_agg, aes(x=LE_MET, y=value, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat="identity")   + labs(y = a_ylab, x = "Fleet-segments")  + #ylim(ylims[1], ylims[2]) +
       scale_fill_manual(values=some_color_species) + facet_grid(. ~ Year) + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  print(p)
dev.off()

 # pel
 namefile <- paste0("barplot_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)], a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
  p <- ggplot(data=the_agg, aes(x=LE_MET, y=value, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat="identity")   + labs(y = a_ylab, x = "Fleet-segments")  + #ylim(ylims[1], ylims[2]) +
       scale_fill_manual(values=some_color_species) + facet_grid(. ~ Year) + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 9000 ; a_height <- 4000
 library(ggplot2)

 # dem
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("LargeMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=Stock), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=Stock), size=3)   +
   scale_color_manual(values=some_color_species) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=Stock), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=Stock), size=3)   + scale_color_manual(values=some_color_species) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 9000 ; a_height <- 4000
 library(ggplot2)

 # dem
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

 the_agg <- long[grep("LargeMesh",long$LE_MET),]

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg$LE_MET)), Stock=levels(factor(the_agg$Stock)), Year=levels(factor(the_agg$Year)))
 dd$value <- 0
 dd[,a_variable] <- 0
 dd <- dd[,colnames(the_agg)]
 rownames(the_agg) <- paste0(the_agg$LE_MET,the_agg$Stock,the_agg$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg),]
 the_agg <- rbind.data.frame(the_agg, dd)
 #---

  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes( fill=Stock))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_species) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg$LE_MET)), Stock=levels(factor(the_agg$Stock)), Year=levels(factor(the_agg$Year)))
 dd$value <- 0
 dd[,a_variable] <- 0
 dd <- dd[,colnames(the_agg)]
 rownames(the_agg) <- paste0(the_agg$LE_MET,the_agg$Stock,the_agg$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg),]
 the_agg <- rbind.data.frame(the_agg, dd)
 #---

  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes(fill=Stock))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_species) +
    xlab("")
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##





} # end a_variable








 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # plot THIS TIME SECTORED PER METIER

 long <- NULL
 agg <- NULL

 variables <- c("LE_KG_LITRE_FUEL", "CPUEallseg", "CPUFallseg", "VPUFallseg")
 prefixes  <- c("LE_LITRE_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_")


 count <- 0
 for(a_variable in variables){
    count <- count+1

    dd <- get(paste0("aggResultPerMet"))
    # get percent per stock for sectorisation


    if(length(grep("LE_LITRE_", colnames(dd)))==0){
       dd$sumallkgs  <- apply(dd[,paste0("LE_KG_", spp)],1,sum, na.rm=TRUE) # marginal sum
       litre <- dd[ , paste0("LE_KG_", spp) ]/ dd$sumallkgs * dd$LE_KG_LITRE_FUEL
       colnames(litre) <- paste0("LE_LITRE_", spp)
       dd <- cbind.data.frame(dd, litre  )
     }


    # reshape first
    library(data.table)
    long <- melt(setDT(dd[,c("LE_MET", "Year", paste0(prefixes[count], spp))]), id.vars = c("LE_MET", "Year"), variable.name = "Var")
    long$Stock <- sapply(strsplit(as.character(long$Var), split="_"), function(x) x[3])

    an_agg <- aggregate(long$value, list(long$Stock, long$Year), sum, na.rm=TRUE)
    colnames(an_agg) <- c("Stock", "Year", a_variable)
    long <- merge(long, an_agg)
    
    #as.data.frame(long)
    long <- long[complete.cases(long),]




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 library(ggplot2)
 if(a_variable=="LE_KG_LITRE_FUEL") {a_unit <- 1e6; a_ylab <- "Fuel use (millions litres)"; ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="CPUEallsp") {a_unit <- 1; a_ylab <- "CPUE (kg per effort)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="CPUFallsp") {a_unit <- 1; a_ylab <- "CPUF (kg per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFallsp") {a_unit <- 1; a_ylab <- "VPUF  (euro per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFSWAallsp") {a_unit <- 1; a_ylab <- "VPUFSWA  (euro per swept area)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="CPUEallseg") {a_unit <- 1; a_ylab <- "CPUE (kg per effort)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="CPUFallseg") {a_unit <- 1; a_ylab <- "CPUF (kg per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="VPUFallseg") {a_unit <- 1; a_ylab <- "VPUF  (euro per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}

  a_width <- 9000 ; a_height <- 4000
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")


   some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")



 # dem
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("LargeMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)

  # caution filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !(grepl("OYF", the_agg$Stock)  | grepl("MUS", the_agg$Stock)  | grepl("COC", the_agg$Stock)   | grepl("SAL", the_agg$Stock) | grepl("HER", the_agg$Stock) | grepl("SPR", the_agg$Stock) | grepl("ELE", the_agg$Stock)  ),]


 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=LE_MET), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=LE_MET), size=3)   +
  scale_color_manual(values=some_color_seg) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=LE_MET), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=LE_MET), size=3)   +
  scale_color_manual(values=some_color_seg) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 9000 ; a_height <- 4000
 library(ggplot2)

 # dem
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

 the_agg <- long[grep("LargeMesh",long$LE_MET),]

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg$LE_MET)), Stock=levels(factor(the_agg$Stock)), Year=levels(factor(the_agg$Year)))
 dd$value <- 0
 dd[,a_variable] <- 0
 dd[,"Var"] <- 0
 dd <- dd[,colnames(the_agg)]
 rownames(the_agg) <- paste0(the_agg$LE_MET,the_agg$Stock,the_agg$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg),]
 the_agg <- rbind.data.frame(the_agg, dd)
 #---

  # caution filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !(grepl("OYF", the_agg$Stock)  | grepl("MUS", the_agg$Stock)  | grepl("COC", the_agg$Stock)   | grepl("SAL", the_agg$Stock) | grepl("HER", the_agg$Stock) | grepl("SPR", the_agg$Stock) | grepl("ELE", the_agg$Stock)  ),]


  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes( fill=LE_MET))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_seg) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg$LE_MET)), Stock=levels(factor(the_agg$Stock)), Year=levels(factor(the_agg$Year)))
 dd$value <- 0
 dd[,a_variable] <- 0
 dd[,"Var"] <- 0
 dd <- dd[,colnames(the_agg)]
 rownames(the_agg) <- paste0(the_agg$LE_MET,the_agg$Stock,the_agg$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg),]
 the_agg <- rbind.data.frame(the_agg, dd)
 #---

  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes( fill=LE_MET))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_seg) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##





} # end a_variable


