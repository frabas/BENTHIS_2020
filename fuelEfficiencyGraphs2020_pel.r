
 setwd (file.path("D:","FBA","BENTHIS_2020"))   # adapt to your need

#TODO
# aggregation per metier

# aggregation per metier per region
#  overlay with D:\FBA\BENTHIS_2020\FAO_AREAS
 
# and compare with AER fleet segmentation and footprint estimates:
# in D:\FBA\ADVICES\STECF\AERdata\STECF 19-06 - AER - Economic and Transversal data tables
# "2019-08_STECF 19-06 - EU Fleet Landings species_FAO sub-region_all years.csv"
 
# separate pelagic vs demersal

# panel plot for years
 
# independent workflow for passive gears (just using (Danish) eflalo files)

 library(doBy)
 library(rgdal)
 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  #-1 Aggregate the results by metier and grid ID
    c.listquote <- function (...)
   {
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
        if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
            "list")) {
            i <- eval(substitute(i), sys.frame(sys.parent()))
        }
        if (class(i) == "call" && i[[1]] == "list") {
            lstquote <- c(lstquote, as.list(i)[-1])
        }
        else if (class(i) == "character") {
            for (chr in i) {
                lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
            }
        }
        else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
            class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
   }
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


#years <- 2012:2019
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

 per_metier_level6 <- TRUE
 per_vessel_size <- FALSE
 per_region     <- TRUE
 
 # search in Baltic and North Sea
 library(rgdal)
 fao_areas  <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 fao_areas  <- fao_areas[ grepl("27", fao_areas$F_AREA) & fao_areas$F_SUBAREA %in% c("27.3", "27.4", "27.2") & fao_areas$F_LEVEL!="MAJOR",] # caution with the MAJOR overidding the over()
 
 # caution: in case of the pelagic fleet, a lot of effort is deployed in the Nort-East Atlantic Waters, i.e. beyond limits of the North Sea....
     
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
  #---
 # find metiers lvl6-Vsize to merge
 if(per_metier_level6 && per_vessel_size){
   res <- NULL
   for (y in years){
      load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsize_",y,".RData") ))  # aggResult
      aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
      levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
     
      # debug
      aggResult <- aggResult[aggResult$LE_KG_LITRE >0,]
   
       # code F_SUBAREA (time consuming code...)
      # Convert all points first to SpatialPoints first
      library(rgdal)
      library(raster)
      an <- function(x) as.numeric(as.character(x))
      coords <- SpatialPoints(cbind(SI_LONG=an(aggResult[, "CELL_LONG"]), SI_LATI=an(aggResult[, "CELL_LATI"])))
      fao_areas <- spTransform(fao_areas, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))    # convert to longlat
      projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
      idx <- over(coords, fao_areas)
      aggResult$F_SUBAREA <- idx[,"F_SUBAREA"]
      aggResult[is.na(aggResult$F_SUBAREA), "F_SUBAREA"] <- "27.4" # few points on coastline
      aggResult$F_DIVISION <- idx[,"F_DIVISION"]
      aggResult[is.na(aggResult$F_DIVISION), "F_DIVISION"] <- paste0(aggResult[is.na(aggResult$F_DIVISION), "F_SUBAREA"],".a") # few points on coastline      
      
      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
    
      dd <- tapply(aggResult$effort_mins, paste0(aggResult$target, "_", aggResult$F_SUBAREA, "_", aggResult$LE_MET_init, "_", aggResult$VesselSize), sum)
      res <- rbind.data.frame(res, cbind.data.frame(names(dd), dd))
    
      # for info:
      #aggResult <- cbind.data.frame(aggResult, code=paste0(aggResult$target, "_", aggResult$F_SUBAREA, "_", aggResult$LE_MET_init, "_", aggResult$VesselSize))
      #print(y)
      #print(head(aggResult[aggResult$code=="NA_27.4_No_Matrix6_[40,100)",]))

      }
      
   pel <- res[grep("SmallMesh",res[,1]),]
   pel <- aggregate(pel$dd, list(pel[,1]), sum)
   pel <- orderBy(~ -x, pel)
   oth_mets_pel <- as.character(pel[cumsum(pel[,2])/sum(pel[,2])>.91,1]) # 90% in effort in pel
  
   dem <- res[grep("LargeMesh",res[,1]),]   # all dem are misplaced and should be removed here because we don´t want these metiers in this routine here. Also likely the result of fishermen spelling the wrong gear in logbooks (OTB for OTM), or polyvalent pelagic vessels.

 # met to keep
 oth_mets <- c(oth_mets_pel, as.character(dem[,1]), 
                                "NA_27.3_No_Matrix6_[12,18)","27.4_No_Matrix6_[12,18)", "NA_27.3_No_Matrix6_[18,24)","NA_27.4_No_Matrix6_[18,24)", 
                                "NA_27.4_No_Matrix6_[40,100)", "NA_27.2_No_Matrix6_[40,100)",
                                                      paste0("NA","_",levels(aggResult$VesselSize)) )
 }
 
 
 
 # find metiers lvl6-Vsize to merge
 if(per_metier_level6 && !per_vessel_size){
   res <- NULL
   for (y in years){
      load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult
      aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
      levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
     
      # debug
      aggResult <- aggResult[aggResult$LE_KG_LITRE >0,]
   
       # code F_SUBAREA (time consuming code...)
      # Convert all points first to SpatialPoints first
      library(rgdal)
      library(raster)
      an <- function(x) as.numeric(as.character(x))
      coords <- SpatialPoints(cbind(SI_LONG=an(aggResult[, "CELL_LONG"]), SI_LATI=an(aggResult[, "CELL_LATI"])))
      fao_areas <- spTransform(fao_areas, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))    # convert to longlat
      projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
      idx <- over(coords, fao_areas)
      aggResult$F_SUBAREA <- idx[,"F_SUBAREA"]
      aggResult[is.na(aggResult$F_SUBAREA), "F_SUBAREA"] <- "27.4" # few points on coastline
      aggResult$F_DIVISION <- idx[,"F_DIVISION"]
      aggResult[is.na(aggResult$F_DIVISION), "F_DIVISION"] <- paste0(aggResult[is.na(aggResult$F_DIVISION), "F_SUBAREA"],".a") # few points on coastline      
      
      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
    
      dd <- tapply(aggResult$effort_mins, paste0(aggResult$target, "_", aggResult$F_SUBAREA, "_", aggResult$LE_MET_init), sum)
      res <- rbind.data.frame(res, cbind.data.frame(names(dd), dd))
    
      # for info:
      #aggResult <- cbind.data.frame(aggResult, code=paste0(aggResult$target, "_", aggResult$F_SUBAREA, "_", aggResult$LE_MET_init, "_", aggResult$VesselSize))
      #print(y)
      #print(head(aggResult[aggResult$code=="NA_27.4_No_Matrix6_[40,100)",]))

      }
      
   pel <- res[grep("SmallMesh",res[,1]),]
   pel <- aggregate(pel$dd, list(pel[,1]), sum)
   pel <- orderBy(~ -x, pel)
   oth_mets_pel <- as.character(pel[cumsum(pel[,2])/sum(pel[,2])>.92,1]) # 90% in effort in pel
  
   dem <- res[grep("LargeMesh",res[,1]),]   # all dem are misplaced and should be removed here because we don´t want these metiers in this routine here. Also likely the result of fishermen spelling the wrong gear in logbooks (OTB for OTM), or polyvalent pelagic vessels.

 # met to keep
 oth_mets <- c(oth_mets_pel, as.character(dem[,1]), 
                                "NA_27.3_No_Matrix6","27.4_No_Matrix6_[12,18)", "NA_27.3_No_Matrix6","NA_27.4_No_Matrix6", 
                                "NA_27.4_No_Matrix6", "NA_27.2_No_Matrix6",
                                                      paste0("NA") )
 }
 #----
 
   sum(res[res[,1] %in% oth_mets,2])
   sum(res[!res[,1] %in% oth_mets,2])
   
 sauv <- oth_mets 
     
 
      
 # aggregation per metier this year
 for (y in years){
   
     if(per_metier_level6 && per_vessel_size) {
       load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsize_",y,".RData") ))  # aggResult
         
       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       colnames(aggResult)[ colnames(aggResult) =="LE_MET_init"] <- "LE_MET"
        
       ##!#!#!#!caution#!#!#!#!
       ##!! remove the difference among OTM and PTM
       aggResult[grepl("PTM",aggResult$LE_MET), "LE_KG_LITRE_FUEL"] <-  aggResult[grepl("PTM",aggResult$LE_MET), "LE_KG_LITRE_FUEL"] *2 # because pair trawling
       levels(aggResult$LE_MET) <- gsub("OTM", "TM", levels(aggResult$LE_MET))
       levels(aggResult$LE_MET) <- gsub("PTM", "TM", levels(aggResult$LE_MET))
       oth_mets  <- gsub("OTM", "TM", oth_mets) 
       oth_mets  <- gsub("PTM", "TM", oth_mets) 
       ##!#!#!#!caution#!#!#!#!
      
     
      # code F_SUBAREA (time consuming code...)
      # Convert all points first to SpatialPoints first
      library(rgdal)
      library(raster)
      an <- function(x) as.numeric(as.character(x))
      coords <- SpatialPoints(cbind(SI_LONG=an(aggResult[, "CELL_LONG"]), SI_LATI=an(aggResult[, "CELL_LATI"])))
      fao_areas <- spTransform(fao_areas, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))    # convert to longlat
      projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
      idx <- over(coords, fao_areas)
      aggResult$F_SUBAREA <- idx[,"F_SUBAREA"]
      aggResult[is.na(aggResult$F_SUBAREA), "F_SUBAREA"] <- "27.4" # few points on coastline
      aggResult$F_DIVISION <- idx[,"F_DIVISION"]
      aggResult[is.na(aggResult$F_DIVISION), "F_DIVISION"] <- paste0(aggResult[is.na(aggResult$F_DIVISION), "F_SUBAREA"],".a") # few points on coastline      
    
      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
  
       # remove
       aggResult <- aggResult[!aggResult$target %in% "LargeMesh",] # we don´t want to see the LargeMesh in this routine.
       aggResult <- aggResult[!grepl("OTB", aggResult$LE_MET),] # we don´t want to see the LargeMesh in this routine.
       
 
      aggResult$LE_MET <- factor(paste0(aggResult$target, "_", aggResult$F_SUBAREA,"_", aggResult$LE_MET, "_", aggResult$VesselSize))
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% oth_mets] <- "SmallMesh_OTHER_0_0_0"
       
  }
  

  if(per_metier_level6 && !per_vessel_size) {
       load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult
         
       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       colnames(aggResult)[ colnames(aggResult) =="LE_MET_init"] <- "LE_MET"
        
       ##!#!#!#!caution#!#!#!#!
       ##!! remove the difference among OTM and PTM
       aggResult[grepl("PTM",aggResult$LE_MET), "LE_KG_LITRE_FUEL"] <-  aggResult[grepl("PTM",aggResult$LE_MET), "LE_KG_LITRE_FUEL"] *2 # because pair trawling
       levels(aggResult$LE_MET) <- gsub("OTM", "TM", levels(aggResult$LE_MET))
       levels(aggResult$LE_MET) <- gsub("PTM", "TM", levels(aggResult$LE_MET))
       oth_mets  <- gsub("OTM", "TM", oth_mets) 
       oth_mets  <- gsub("PTM", "TM", oth_mets) 
       ##!#!#!#!caution#!#!#!#!
      
     
      # code F_SUBAREA (time consuming code...)
      # Convert all points first to SpatialPoints first
      library(rgdal)
      library(raster)
      an <- function(x) as.numeric(as.character(x))
      coords <- SpatialPoints(cbind(SI_LONG=an(aggResult[, "CELL_LONG"]), SI_LATI=an(aggResult[, "CELL_LATI"])))
      fao_areas <- spTransform(fao_areas, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))    # convert to longlat
      projection(coords) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")   # a guess!
      idx <- over(coords, fao_areas)
      aggResult$F_SUBAREA <- idx[,"F_SUBAREA"]
      aggResult[is.na(aggResult$F_SUBAREA), "F_SUBAREA"] <- "27.4" # few points on coastline
      aggResult$F_DIVISION <- idx[,"F_DIVISION"]
      aggResult[is.na(aggResult$F_DIVISION), "F_DIVISION"] <- paste0(aggResult[is.na(aggResult$F_DIVISION), "F_SUBAREA"],".a") # few points on coastline      
    
      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
  
       # remove
       aggResult <- aggResult[!aggResult$target %in% "LargeMesh",] # we don´t want to see the LargeMesh in this routine.
       aggResult <- aggResult[!grepl("OTB", aggResult$LE_MET),] # we don´t want to see the LargeMesh in this routine.
       
 
      aggResult$LE_MET <- factor(paste0(aggResult$target, "_", aggResult$F_SUBAREA,"_", aggResult$LE_MET))
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% oth_mets] <- "SmallMesh_OTHER_0_0_0"
       
  }

    #head(aggResult)
    #range(aggResult$effort_mins)
 
     # debug
    aggResult <- aggResult[aggResult$LE_KG_LITRE >0,]

    aggResult$KKGallsp <- apply (aggResult[, paste0('LE_KG_', spp)], 1, sum, na.rm=TRUE) /1e3 # in tons
    aggResult$KEUROallsp <- apply (aggResult[, paste0('LE_EURO_', spp)], 1, sum, na.rm=TRUE) / 1e3 # in thousands euros

    # compute some ratios
    dd <- sweep(aggResult[, paste0('LE_KG_', spp)], 1,  aggResult$effort_mins, FUN="/")
    colnames(dd) <- paste0('LE_CPUE_', spp)
    dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
    aggResult <- cbind.data.frame (aggResult, dd)
    aggResult$CPUEallsp <- apply (aggResult[, paste0('LE_CPUE_', spp)], 1, sum, na.rm=TRUE)

    dd <- sweep(aggResult[, paste0('LE_KG_', spp)], 1,  aggResult$LE_KG_LITRE_FUEL, FUN="/")
    colnames(dd) <- paste0('LE_CPUF_', spp)
    dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
    aggResult <- cbind.data.frame (aggResult, dd)
    aggResult$CPUFallsp <- apply (aggResult[, paste0('LE_CPUF_', spp)], 1, sum, na.rm=TRUE)

    dd <- sweep(aggResult[, paste0('LE_EURO_', spp)], 1,   aggResult$effort_mins, FUN="/")
    colnames(dd) <- paste0('LE_VPUE_', spp)
    dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
    aggResult <- cbind.data.frame (aggResult, dd)
    aggResult$VPUEallsp <- apply (aggResult[, paste0('LE_VPUE_', spp)], 1, sum, na.rm=TRUE)

    dd <- sweep(aggResult[, paste0('LE_EURO_', spp)], 1,   aggResult$LE_KG_LITRE_FUEL, FUN="/")
    colnames(dd) <- paste0('LE_VPUF_', spp)
    dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
    aggResult <- cbind.data.frame (aggResult, dd)
    aggResult$VPUFallsp <- apply (aggResult[, paste0('LE_VPUF_', spp)], 1, sum, na.rm=TRUE)

    dd <- sweep(aggResult[, paste0('LE_EURO_', spp)], 1,   aggResult$SWEPT_AREA_KM2, FUN="/")
    colnames(dd) <- paste0('LE_VPUFSWA_', spp)
    dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
    aggResult <- cbind.data.frame (aggResult, dd)
    aggResult$VPUFSWAallsp <- apply (aggResult[, paste0('LE_VPUFSWA_', spp)], 1, sum, na.rm=TRUE)

    dd <-aggResult[, paste0('LE_EURO_', spp)]/aggResult[, paste0('LE_KG_', spp)]
    colnames(dd) <- paste0('LE_MPRICE_', spp)
    dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
    aggResult <- cbind.data.frame (aggResult, dd)
    aggResult$mpriceallsp <- apply (aggResult[, paste0('LE_MPRICE_', spp)], 1, mean, na.rm=TRUE)


   idx_cols <- grepl("LE_VPUF_", names(aggResult))    
    dd <- apply (aggResult[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x))[1]
               })
    aggResult$sp_with_max_vpuf <-   gsub("LE_VPUF_", "", names(aggResult[,idx_cols])[dd])          

    idx_cols <- grepl("LE_CPUE_", names(aggResult))    
    dd <- apply (aggResult[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x))[1]
               })
    aggResult$sp_with_max_cpue <-   gsub("LE_CPUE_", "", names(aggResult[,idx_cols])[dd])          

   idx_cols <- grepl("LE_CPUF_", names(aggResult))    
   dd <- apply (aggResult[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x)) [1]
               })
   aggResult$sp_with_max_cpuf <-   gsub("LE_CPUF_", "", names(aggResult[,idx_cols])[dd])          

    
   idx_cols <- grepl("LE_VPUFSWA_", names(aggResult))    
   dd <- apply (aggResult[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x))[1]
               })
   aggResult$sp_with_max_vpufswa <-   gsub("LE_VPUFSWA_", "", names(aggResult[,idx_cols])[dd])          


    # capture an export for quickmap2020.r   (and for DISPLACE North Sea)
     if(per_metier_level6 && per_vessel_size) {
       save(aggResult, file=file.path(getwd(), "outputs2020_pel", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_", y, ".RData", sep=""))) 
       }
     if(per_metier_level6 && !per_vessel_size) {
       save(aggResult, file=file.path(getwd(), "outputs2020_pel", paste("AggregatedSweptAreaPlusMet6AndRatiosForPel_", y, ".RData", sep=""))) 
       }

    # then, do the aggregation for following ggplots
    agg_by <- "LE_MET"

   # aggregate ("sum" if absolute value, "mean" if ratio)
    nm <- names(aggResult)
    library(data.table)
    # for sum
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('effort_mins', nm)
    idx.col.kkg       <- grep('KKGallsp', nm, fixed=TRUE)
    idx.col.keuro     <- grep('KEUROallsp', nm, fixed=TRUE)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort, idx.col.kkg, idx.col.keuro)
    DT  <- data.table(aggResult)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    a_by <- c.listquote(  agg_by )
    aggResultPerMet <- DT[,eval(eq1),by=eval(a_by)]
    aggResultPerMet <- data.frame(aggResultPerMet)
    colnames(aggResultPerMet) <- c(agg_by, colnames(aggResult)[idx.col] )
    library(doBy)
    aggResultPerMet <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=aggResultPerMet) # order to ensure same order when collating....

    # for average
    idx.col.1     <- grep('CPUEallsp', nm, fixed=TRUE)
    idx.col.2     <- grep('CPUFallsp', nm, fixed=TRUE)
    idx.col.3     <- grep('VPUFallsp', nm, fixed=TRUE)
    idx.col.4     <- grep('VPUFSWAallsp', nm, fixed=TRUE)
    idx.col.5     <- grep('mpriceallsp', nm, fixed=TRUE)
    idx.col.cpue     <- grep('LE_CPUE_', nm)
    idx.col.cpuf     <- grep('LE_CPUF_', nm)
    idx.col.vpuf     <- grep('LE_VPUF_', nm)
    idx.col.vpuswa     <- grep('LE_VPUFSWA_', nm)
    idx.col.mprice     <- grep('LE_MPRICE_', nm)
    idx.col <- c(idx.col.1, idx.col.2, idx.col.3, idx.col.4, idx.col.5, idx.col.cpue,idx.col.cpuf, idx.col.vpuf, idx.col.vpuswa, idx.col.mprice)
    a_mean <- function(x, na.rm) mean(x[x!=0], na.rm=na.rm) # modify the mean() so that 0 are first removed....
    eq1  <- c.listquote( paste ("a_mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
    DT  <- data.table(aggResult)
    a_by <- c.listquote(  agg_by )
    aggResultPerMet2 <- DT[,eval(eq1),by=eval(a_by)]
    aggResultPerMet2 <- data.frame(aggResultPerMet2)
    colnames(aggResultPerMet2) <- c(agg_by, colnames(aggResult)[idx.col] )
    aggResultPerMet2 <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=aggResultPerMet2) # order to ensure same order when collating....

    # collate
    aggResultPerMet <- cbind(aggResultPerMet, aggResultPerMet2[,-c(1:length(agg_by))])

    # then do some estimates, caution, after the aggregation for those ones 
    # litre per kilo catch
    aggResultPerMet$FPUCallsp <- aggResultPerMet$LE_KG_LITRE_FUEL/(aggResultPerMet$KKGallsp*1000) 
    aggResultPerMet$FPUCallsp [is.infinite(aggResultPerMet$FPUCallsp)] <- 0

    # litre per euro catch
    aggResultPerMet$FPUVallsp <-  aggResultPerMet$LE_KG_LITRE_FUEL/(aggResultPerMet$KEUROallsp*1000)
    aggResultPerMet$FPUVallsp [is.infinite(aggResultPerMet$FPUVallsp)] <- 0

   assign(paste0("aggResultPerMet_", y), aggResultPerMet)
   cat(paste0("saved for ", y, "\n"))
  } # end y


 # check
 unique(aggResultPerMet$LE_MET)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
 ## QUICK & EASY TABLES
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
  # capture an export for later doing some quick table
  aggResultPerMetAlly <- NULL
  for (y in years){
     dd <- get(paste0("aggResultPerMet_", y))
     dd <- cbind.data.frame(Year=y, dd)
     aggResultPerMetAlly <- rbind.data.frame(aggResultPerMetAlly, dd)
  }   
  # save:
  if(per_metier_level6 && !per_vessel_size) save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep=""))) 
  if(per_metier_level6 && per_vessel_size) save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep=""))) 
 

  #-----------------
  #-----------------
  load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
  
  # add a fuel efficiency metric
  aggResultPerMetAlly$LPUE <- aggResultPerMetAlly$LE_KG_LITRE_FUEL / (aggResultPerMetAlly$effort_mins/60)
  aggResultPerMetAlly$LE_MESH_GROUP <- NA
  aggResultPerMetAlly[grepl("LargeMesh",aggResultPerMetAlly$LE_MET), "LE_MESH_GROUP"] <- "LargeMesh" 
  aggResultPerMetAlly[grepl("SmallMesh",aggResultPerMetAlly$LE_MET), "LE_MESH_GROUP"] <- "SmallMesh" 
  aggResultPerMetAlly$LE_MET <- gsub("LargeMesh_", "", aggResultPerMetAlly$LE_MET)
  aggResultPerMetAlly$LE_MET <- gsub("SmallMesh_", "", aggResultPerMetAlly$LE_MET)
  
  ### plot LPUE
  library(ggplot2)
  dd <- aggResultPerMetAlly[,c("Year", "LPUE", "LE_MET", "LE_MESH_GROUP")]
  dd <-  dd[!(grepl("OTHER",aggResultPerMetAlly$LE_MET) | grepl("NA",aggResultPerMetAlly$LE_MET)),]
  dd <-  dd[dd$LE_MET %in%names(table(dd$LE_MET))[table(dd$LE_MET)==length(years)] ,] # keep complete ts only
  a_lpue_plot <- ggplot(dd, aes(x=as.character(Year), y=as.numeric(LPUE), group=as.character(LE_MET))) +     
                             geom_line(aes(color=LE_MET), size=1.5) + 
     facet_wrap(. ~ LE_MESH_GROUP, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
     labs(y = "Fuel efficiency (Litre fuel per hour)", x = "Year")   +
   scale_colour_manual(values=some_color_seg, name="Fleet-segment") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
  print(a_lpue_plot)
  # for paper:
  a_width <- 6000;  a_height <- 4000
  namefile <- paste0("ts_LPUE_", years[1], "-", years[length(years)],  "_PEL.tif")
  tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(a_lpue_plot, ncol=1, common.legend = TRUE, legend="right")

  dev.off()

  
  #-----------------
  #-----------------
  load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
  spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")

  
  library(doBy)
  
  spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]

  # split fuel
  PercentThisStk <- aggResultPerMetAlly[paste0("LE_KG_",spp)] / apply(aggResultPerMetAlly[paste0("LE_KG_",spp)], 1, sum, na.rm=TRUE)*100
  colnames(PercentThisStk)  <- paste0("Percent_",spp)
  aggResultPerMetAlly <- cbind.data.frame (aggResultPerMetAlly, PercentThisStk)
  VarThisStk <- sweep(PercentThisStk[,colnames(PercentThisStk)]/100, 1, aggResultPerMetAlly[,"LE_KG_LITRE_FUEL"], FUN="*")
  colnames(VarThisStk)  <- paste0("LE_LITRE_",spp)
  aggResultPerMetAlly <- cbind.data.frame (aggResultPerMetAlly, VarThisStk)
 
   # split effort
  VarThisStk <- sweep(PercentThisStk[,colnames(PercentThisStk)]/100, 1, aggResultPerMetAlly[,"effort_mins"], FUN="*")
  colnames(VarThisStk)  <- paste0("LE_EFFORT_",spp)
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

 
  nm <- colnames(aggResultPerMetAlly)
  sum_y_hoursatsea <- aggregate(aggResultPerMetAlly[, grepl("LE_EFFORT_", nm)]/60, list(Year=aggResultPerMetAlly$Year), sum, na.rm=TRUE)  # annual average
  nm <- colnames(sum_y_hoursatsea)
  average_y_hoursatsea <- apply(sum_y_hoursatsea[, grepl("LE_EFFORT_", nm)]/60, 2, mean)  # annual average
  round(average_y_hoursatsea[order(average_y_hoursatsea, decreasing=TRUE)]/1e3,2)
  info4 <-  round(average_y_hoursatsea[order(average_y_hoursatsea, decreasing=TRUE)]/1e3,3) # thousands of hours at sea

 
  spp <- sapply(strsplit(as.character(names(info3)), split="_"), function(x) x[3])  # give the order on the plot
  spp <- spp[spp!="FUEL"]
  a_summary <- rbind.data.frame(info1[paste0("LE_KG_", spp)], info2[paste0("LE_EURO_", spp)], info3[paste0("LE_LITRE_", spp)], info4[paste0("LE_EFFORT_", spp)] )
  colnames(a_summary) <- spp
  rownames(a_summary) <- c("Thousands tons", "Millions euros", "Millions litres", "Thousands hours at sea")
  a_summary
   #=> supplementary data
   
  # pelagic gears
  a_width <- 2000;  a_height <- 5500
  namefile <- paste0("barplot_fuel_efficiency_pelagics_per_species-2005-2019.tif")
  tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
    library(data.table)
    long1 <- melt(setDT(sum_y_kg[,c("Year", paste0("LE_KG_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long1$value   <- long1$value  /1e6 # thousand tons
    long2 <- melt(setDT(sum_y_euros[,c("Year", paste0("LE_EURO_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long2$value   <- long2$value  /1e6 # millions
    long3 <- melt(setDT(sum_y_litres[,c("Year", paste0("LE_LITRE_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long3$value   <- long3$value  /1e6 # millions
    long4 <- melt(setDT(sum_y_hoursatsea[,c("Year", paste0("LE_EFFORT_", spp))]), id.vars = c("Year"), variable.name = "Var")
    long4$value <-  long4$value /1e3  # thousands hours
    long <- rbind.data.frame(long1, long2, long3, long4)
    long$Species <- sapply(strsplit(as.character(long$Var), split="_"), function(x) x[3])
    long$VarType <- factor(sapply(strsplit(as.character(long$Var), split="_"), function(x) x[2]))
    levels(long$VarType) <- c( "Thousands hours at sea", "Millions Euros", "Thousand Tons", "Millions Litres")
    long$Species <- with(long, reorder(Species, value, median)) # reorder
    long$Species <- factor(long$Species, levels=rev(levels(long$Species))) # reverse
    
    var_names <- c( "Thousands hours at sea"="Thousands hours at sea", "Millions Euros"="Millions Euros", "Thousand Tons"="Thousand Tons", "Millions Litres"="Millions Litres")
    
   p <- ggplot(data=long[long$Species %in% c("HER", "MAC", "SPR", "WHB", "HOM", "OTH", "SAN", "NOP", "NOP", "WHB"),], aes(x=Species, y=value))  +
           geom_boxplot(outlier.size = -1, fill='#A4A4A4', color="black") +  scale_color_grey() + 
            facet_wrap(~VarType, scales="free", ncol=1, labeller=as_labeller(var_names),  strip.position = "left") +  
            labs(y = "", x = "Species") + 
           theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8), strip.background = element_blank(),strip.placement = "outside")
  print(p)
  
  dev.off()
  #write.table(a_summary, "clipboard", sep="\t", row.names=TRUE, col.names=TRUE)


     

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 





 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 # plot THIS TIME SECTORED PER METIER


 variables <- c("LE_KG_LITRE_FUEL", "CPUEallseg", "CPUFallseg", "VPUFallseg", "KKGallseg", "KEUROallseg")
 prefixes  <- c("LE_LITRE_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_")


 count <- 0
 for(a_variable in variables){
    count <- count+1

  a_long <- NULL
  long <- NULL
  for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
 
    # get percent per stock for sectorisation

    if(length(grep("LE_LITRE_", colnames(dd)))==0){
       dd$sumallkgs  <- apply(dd[,paste0("LE_KG_", spp)],1,sum, na.rm=TRUE) # marginal sum
       litre <- dd[ , paste0("LE_KG_", spp) ]/ dd$sumallkgs * dd$LE_KG_LITRE_FUEL 
       colnames(litre) <- paste0("LE_LITRE_", spp)
       dd <- cbind.data.frame(dd, litre  )
     }
     
    dd <- cbind.data.frame(Year=y, dd)
   
    # reshape first
    library(data.table)
    a_long <- melt(setDT(dd[,c("LE_MET", "Year",  paste0(prefixes[count], spp))]), id.vars = c("LE_MET", "Year"), variable.name = "Var")
    a_long$Stock <- sapply(strsplit(as.character(a_long$Var), split="_"), function(x) x[3])

    an_agg <- aggregate(a_long$value, list(a_long$Stock, a_long$Year), sum, na.rm=TRUE)   # CAUTION ABOUT THE INTERPRETATION HERE AS WE ARE SUMMING VPUEs...
    colnames(an_agg) <- c("Stock", "Year", a_variable)
    a_long <- merge(a_long, an_agg)
    
    #as.data.frame(long)
    a_long <- a_long[complete.cases(a_long),]

    long <- rbind.data.frame(long, a_long)
    
   } # end y


   # assign area to species as a proxy of stock
   long$Stock <- paste(long$Stock, sapply(strsplit(as.character(long$LE_MET), split="_"), function(x) x[2]))


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
 if(a_variable=="KKGallseg") {a_unit <- 1e3; a_ylab <- "Landings (tons)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="KEUROallseg") {a_unit <- 1e3; a_ylab <- "Landings  (keuros)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}

  a_width <- 9000 ; a_height <- 4000
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")



# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)

     # caution filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !(grepl("OTHER", the_agg$Stock)  | grepl("OTH", the_agg$Stock)   | grepl("CSH", the_agg$Stock) | grepl("MUS", the_agg$Stock) | grepl("SAN 27.2", the_agg$Stock) | grepl("SPR 27.2", the_agg$Stock)  | grepl("WHG 27.2", the_agg$Stock) |   grepl("MAC 27.3", the_agg$Stock) | grepl("NOP 27.2", the_agg$Stock) | grepl("COD", the_agg$Stock)  | grepl("HAD", the_agg$Stock) |  grepl("PRA", the_agg$Stock) |  grepl("HOM", the_agg$Stock) | grepl("HKE", the_agg$Stock) | grepl("DAB", the_agg$Stock)  | grepl("FLE", the_agg$Stock)  | grepl("HOM 27.3", the_agg$Stock)  | grepl("LEM", the_agg$Stock)  | grepl("NEP", the_agg$Stock)  | grepl("NOP 27.3", the_agg$Stock) | grepl("PLE", the_agg$Stock) | grepl("SOL", the_agg$Stock) | grepl("TUR", the_agg$Stock) | grepl("WIT", the_agg$Stock) | grepl("POK", the_agg$Stock) | grepl("WHB", the_agg$Stock) | grepl("CSH 27.3", the_agg$Stock) | grepl("MON", the_agg$Stock) | grepl("ELE", the_agg$Stock) ),]


 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=LE_MET), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=LE_MET), size=3)   +
  scale_color_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1)) + 
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

 
# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- as.data.frame(long[grep("SmallMesh",long$LE_MET),])

 
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
  
    # caution filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !(  grepl("CSH", the_agg$Stock) | grepl("MUS", the_agg$Stock) | grepl("SAN 27.2", the_agg$Stock) | grepl("SPR 27.2", the_agg$Stock)  | grepl("WHG 27.2", the_agg$Stock) |   grepl("MAC 27.3", the_agg$Stock) | grepl("NOP 27.2", the_agg$Stock) | grepl("COD", the_agg$Stock)  | grepl("HAD", the_agg$Stock) |  grepl("PRA", the_agg$Stock) |  grepl("HOM", the_agg$Stock) | grepl("HKE", the_agg$Stock) | grepl("DAB", the_agg$Stock)  | grepl("FLE", the_agg$Stock)  | grepl("HOM 27.3", the_agg$Stock)  | grepl("LEM", the_agg$Stock)  | grepl("NEP", the_agg$Stock)  | grepl("NOP 27.3", the_agg$Stock) | grepl("PLE", the_agg$Stock) | grepl("SOL", the_agg$Stock) | grepl("TUR", the_agg$Stock) | grepl("WIT", the_agg$Stock) | grepl("POK", the_agg$Stock) | grepl("WHB", the_agg$Stock) | grepl("CSH 27.3", the_agg$Stock) | grepl("MON", the_agg$Stock) | grepl("ELE", the_agg$Stock) ),]
  
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes( fill=LE_MET))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1)) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##





} # end a_variable































 
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
#load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]


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
     dd[grepl("TBB_CRU_16-31",dd$LE_MET), "met_desc3"] <- "shrimp"
     dd$met_desc2[is.na(dd$met_desc2)] <- ""
     dd$met_desc3[is.na(dd$met_desc3)] <- ""

    return(paste(dd$met_desc1, dd$met_desc2, dd$met_desc3))
    }



 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_",  "LE_MPRICE_")
 the_names <- c("(z)","(a)","(b)", "(c)", "(d)", "(e)", "(f)",  "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     dd$met_desc <- friendly_met_names(dd)
   
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
     
     if(y==years[1]){
     agg <- cbind.data.frame(long, year=y)
     }
      else{
      agg <- rbind.data.frame(agg,
             cbind.data.frame(long, year=y))
     }
     
   }

     the_agg <- rbind.data.frame(as.data.frame(the_agg), as.data.frame(agg))
 }




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 6200 ; a_height <- 7500
 library(ggplot2)
 a_unit <- 1
 a_comment <- ""

# pel
 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), year=levels(factor(the_agg_plot$year)))
 dd$met_desc <- friendly_met_names(dd)
  
 
 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)
  
  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET),])
 p1_area_pelfishing_pel_land <- ggplot(the_agg_plot1, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p1)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET),])
 p3_area_pelfishing_pel_cpuf <- ggplot(the_agg_plot3, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET),])
 p4_area_pelfishing_pel_vpuf <- ggplot(the_agg_plot4, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p4)

 
  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
 p5_area_pelfishing_pel_fpuc <- ggplot(the_agg_plot5, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p6_area_pelfishing_pel_fpuv <- ggplot(the_agg_plot6, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p6)
 
 ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p6_area_pelfishing_pel_price <- ggplot(the_agg_plot8, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p8)



 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot_land_and_FPUE_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
ggarrange(p1_area_pelfishing_pel_land, p4_area_pelfishing_pel_vpuf, p5_area_pelfishing_pel_fpuc, ncol=3, common.legend = TRUE, legend="right")

dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##


     

##!!!!!!!!!!!!!!!!!!!!!!##
## SUMMARY BARPLOT WITH AVERAGE OVER THE PERIOD 
##!!!!!!!!!!!!!!!!!!!!!!##

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
 Stocknames <-  c("COD"="cod", "CSH"="brown shrimp", "DAB"="dab", "ELE"="eel", "FLE"="flounder",
                        "HAD"="haddock", "HER"="herring", "HKE"="hake","HOM"="Horse mackerel", "LEM"="lemon sole",
                         "MAC"="mackerel", "MON"="monkfish", "MUS"="mussel", "NEP"="Nephrops", "NOP"="Norway pout", "PLE"="plaice",
                         "POK"="saithe", "PRA"="boreal shrimp", "SAN"="sandeel", "SOL"="sole", "SPR"="sprat", "TUR"="turbot", "WHB"="blue whiting",
                          "WIT"="witch flounder", "WHG"="whiting", "OTH"="other")                         
  some_color_speciesnames <- c("cod"="#E69F00", "brown shrimp"="hotpink", "dab"="#56B4E9", "eel"="#F0E442", "flounder"="green",
                        "haddock"="#0072B2", "herring"="mediumorchid4", "hake"="#CC79A7","Horse mackerel"="indianred2", "lemon sole"="#EEC591",
                         "mackerel"="#458B00", "monkfish"="#F0F8FF", "mussel"="black", "Nephrops"="#e3dcbf", "Norway pout"="#CD5B45", "plaice"="lightseagreen",
                         "saithe"="#6495ED", "boreal shrimp"="#CDC8B1", "sandeel"="#00FFFF", "sole"="#8B0000", "sprat"="#008B8B", "turbot"="#A9A9A9", "blue whiting"="#76a5c4",
                          "witch flounder"="red", "whiting"="yellow", "other"="blue")

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



#load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]


 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_",  "LE_MPRICE_")
 the_names <- c("(a)","(b)", "(c)", "(d)", "(e)", "(f)",  "(h)")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)","(h)")


 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     dd$met_desc <- friendly_met_names(dd)
   
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
     
     if(y==years[1]){
     agg <- cbind.data.frame(long, year=y)
     }
      else{
      agg <- rbind.data.frame(agg,
             cbind.data.frame(long, year=y))
     }
     
   }

     the_agg <- rbind.data.frame(as.data.frame(the_agg), as.data.frame(agg))
 }

 
 

##!!!!!!!!!!!!!!!!!!!!!!##
## PEL 
 library(ggplot2)

 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])


 the_agg_plot$Stockname <- Stocknames[the_agg_plot$Stock]


 # a visual fix adding all combi--
 #dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), year=levels(factor(the_agg_plot$year)))
 #dd$met_desc <- friendly_met_names(dd)
   
 #dd$value <- 0
 #dd[,"Total"] <- 0
 #dd <- dd[,colnames(the_agg_plot)]
 #rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$year)
 #rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$year)
 #dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 #the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)
  
  # filter out small contribution species 
  ss <- the_agg_plot[grepl ( "(a)",  the_agg_plot$LE_MET)   & as.numeric(as.character(the_agg_plot$value))>200, ]  # < 200tons?
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% unique(ss$Stock),]

  
  the_agg_plot$LE_MET <- factor(the_agg_plot$LE_MET )
  the_agg_plot$met_desc <- factor(the_agg_plot$met_desc )
  the_agg_plot$Stock <- factor(the_agg_plot$Stock )

    # find order of met
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$LE_MET), mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered <- as.character(dd[,1])

 # find order of met_desc
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$met_desc), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered2 <- as.character(dd[,1])

 
  a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}
 


# PEL
 collecting_table <- NULL
 collecting_table2019 <- NULL
 
 # PEL
  the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot0$met_desc <- factor(the_agg_plot0$met_desc, level=fleet_segments_ordered2) # reorder
  p0_barplot_pelfishing_pel_value <- ggplot(data=the_agg_plot0, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Value (KEuros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p0)

 
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot1$met_desc <- factor(the_agg_plot1$met_desc, level=fleet_segments_ordered2) # reorder
  p1_barplot_pelfishing_pel_land <- ggplot(data=the_agg_plot1, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot2$met_desc <- factor(the_agg_plot2$met_desc, level=fleet_segments_ordered2) # reorder
 p2_barplot_pelfishing_pel_fuel <- ggplot(data=the_agg_plot2, aes(x=met_desc, y=value/1e3, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="Fuel (thousands litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot3$met_desc <- factor(the_agg_plot3$met_desc, level=fleet_segments_ordered2) # reorder
 p3_barplot_pelfishing_pel_cpuf <- ggplot(data=the_agg_plot3, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2, position="left"))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot4$met_desc <- factor(the_agg_plot4$met_desc, level=fleet_segments_ordered2) # reorder
  p4_barplot_pelfishing_pel_vpuf <- ggplot(data=the_agg_plot4, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "VPUF (euro per litre)", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
       theme(axis.text.x=element_blank()) 
        #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot5$met_desc <- factor(the_agg_plot5$met_desc, level=fleet_segments_ordered2) # reorder
  p5_barplot_pelfishing_pel_fpuc <- ggplot(data=the_agg_plot5, aes(x=met_desc, y=value/a_unit, fill=Stockname)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_speciesnames[unique(the_agg_plot5$Stockname)], name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p5)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot6$met_desc <- factor(the_agg_plot6$met_desc, level=fleet_segments_ordered2) # reorder
  p6_barplot_pelfishing_pel_fpuv <- ggplot(data=the_agg_plot6, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p6)

 ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    the_agg_plot8[the_agg_plot8$ value>100,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot8$met_desc <- factor(the_agg_plot8$met_desc, level=fleet_segments_ordered2) # reorder
    p8_barplot_pelfishing_pel_price <- ggplot(data=the_agg_plot8, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
#print(p8)



# for paper:
  a_width <- 6000 ; a_height <- 5500
 namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_PEL_plot_land_and_FPUC_and_FPUV_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_barplot_pelfishing_pel_land, p2_barplot_pelfishing_pel_fuel, p4_barplot_pelfishing_pel_vpuf, p5_barplot_pelfishing_pel_fpuc, ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="right")
dev.off()


 # for paper:
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  "Euro_catch_per_per_kg.tiff"),   width = a_width, height = 3000,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p8_barplot_pelfishing_pel_price, ncol=1, heights=c(1),common.legend = TRUE, legend="bottom")
dev.off()




# export underlying data
  a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot0$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_mean), cv=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot1$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_mean), cv=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot2$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_mean), cv=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot3$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_mean), cv=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot4$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_mean), cv=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot5$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_mean), cv=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot6$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_mean), cv=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_cv))) 
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot8$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_mean), cv=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_cv))) 

  collecting_table[,4] <- round(collecting_table[,4],4)
  collecting_table[,5] <- round(collecting_table[,5],4)
# export underlying data
write.table(collecting_table,
            file=file.path(getwd(), "outputs2020_pel", "output_plots", 
            paste("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_PEL_plot_land_and_FPUC_and_FPUV.dat")),
             row.names=FALSE, quote=FALSE, sep=";")
            


  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot1[the_agg_plot0$year==2019,]$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0[the_agg_plot0$year==2019,]$Total, the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot0[the_agg_plot0$year==2019,]$Total, the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1[the_agg_plot1$year==2019,]$Total, the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot1[the_agg_plot1$year==2019,]$Total, the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2[the_agg_plot2$year==2019,]$Total, the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot2[the_agg_plot2$year==2019,]$Total, the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3[the_agg_plot3$year==2019,]$Total, the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot3[the_agg_plot3$year==2019,]$Total, the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4[the_agg_plot4$year==2019,]$Total, the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot4[the_agg_plot4$year==2019,]$Total, the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5[the_agg_plot5$year==2019,]$Total, the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot5[the_agg_plot5$year==2019,]$Total, the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6[the_agg_plot6$year==2019,]$Total, the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot6[the_agg_plot6$year==2019,]$Total, the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET, a_func_cv))) 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallMesh", seg=levels(the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8[the_agg_plot8$year==2019,]$Total, the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot8[the_agg_plot8$year==2019,]$Total, the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET, a_func_cv))) 

  collecting_table2019[,4] <- round(collecting_table2019[,4],4)
  collecting_table2019[,5] <- round(collecting_table2019[,5],4)
# export underlying data
write.table(collecting_table2019,
            file=file.path(getwd(), "outputs2020_pel", "output_plots", 
            paste("barplot_mean_fuel_efficiency_2019_PEL_plot_land_and_FPUC_and_FPUV.dat")),
             row.names=FALSE, quote=FALSE, sep=";")




 
















 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!##
 ## SPECIES ORIENTED - SECTORED PER FLEET-SEG
 ## PEL
### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......

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
#load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_EURO_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")


  count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
    dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
    dd$met_desc <- friendly_met_names(dd)
  
    dd <- cbind.data.frame(Year=y, dd)
   
   if(a_variable %in% c("KEUROallsp","KKGallsp","LE_KG_LITRE_FUEL")){
   
       marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 1, sum, na.rm=TRUE) # in row
       percent_over_stk <- sweep(dd[, paste0(prefixes[count], spp)] , 1,   marginal_sum_catches, FUN="/")*100
       a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_stk))
       # tapply(a_data$LE_KG_LITRE_FUEL, a_data$met_desc, sum, na.rm=TRUE)
       # reshape first
       library(data.table)
       a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
       a_long <- as.data.frame(a_long)
    
      }
   if(!a_variable %in% c("KEUROallsp","KKGallsp","LE_KG_LITRE_FUEL")){
   
       marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 2, sum, na.rm=TRUE) # in col
       percent_over_met <- sweep(dd[, paste0(prefixes[count], spp)] , 2,   marginal_sum_catches, FUN="/")*100
       a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_met))
       # tapply(a_data$LE_KG_LITRE_FUEL, a_data$met_desc, sum, na.rm=TRUE)
       # reshape first
       library(data.table)
       a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
       a_long <- as.data.frame(a_long)
 
   }  
    
     
    a_long$value <- a_long$value* a_long[, a_variable]/100 # dispatch with percentage

   
    a_long <- a_long[complete.cases(a_long),]

    a_long$Stock <- sapply(strsplit(as.character(a_long$Var), split="_"), function(x) x[3])
    
   # assign area to species as a proxy of stock
   a_long$Stock <- paste(a_long$Stock, sapply(strsplit(as.character(a_long$LE_MET), split="_"), function(x) x[2]))

   colnames(a_long)[colnames(a_long) %in% c(a_variable)] <- "Total"
   
  
   a_long$LE_MET <- paste(a_long$LE_MET, the_names[count])
   a_long <- as.data.frame(a_long)
   a_long$LE_MET <- factor(a_long$LE_MET)
   
     if(y==years[1]){
     agg <- cbind.data.frame(as.data.frame(a_long))
     }
      else{
      agg <- rbind.data.frame(as.data.frame(agg),
             cbind.data.frame(a_long))
     }
     
   }

     the_agg <- rbind.data.frame(as.data.frame(the_agg), as.data.frame(agg))
 }


   # filtering the ratios:
   # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
   #ss <- the_agg[the_agg$Var %in%  paste0("LE_KG_", spp) & as.numeric(as.character(the_agg$value))>2000000, ]  # < 2000tons?
   #the_agg <- the_agg[the_agg$Stock %in% unique(ss$Stock),]

   
   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]
    
  
    the_agg <- the_agg[!grepl("misc.", the_agg$met_desc, fixed=TRUE),]
 
##--------
library(ggplot2)
a_comment <- ""
a_unit <- 1

 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET, fixed=TRUE),])

 # a visual fix adding all combi--
 #dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), Year=levels(factor(the_agg_plot$Year)))
 #dd$met_desc <- friendly_met_names(dd)
 #dd$value <- 0
 #dd[,"Total"] <- 0
 #dd <- dd[,colnames(the_agg_plot)]
 #rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$Year)
 #rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 #dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 #the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

  the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)
 
    # caution filter out non-relevant species for these fleets
  the_agg_plot<-  the_agg_plot[ !(grepl("COD", the_agg_plot$Stock) | grepl("DAB", the_agg_plot$Stock)  | grepl("FLE", the_agg_plot$Stock)  | 
                    grepl("HOM", the_agg_plot$Stock)  | grepl("LEM", the_agg_plot$Stock)  | grepl("NEP", the_agg_plot$Stock)  | 
                    grepl("NOP 27.3", the_agg_plot$Stock) | grepl("PLE", the_agg_plot$Stock) | grepl("SOL", the_agg_plot$Stock)  | grepl("OTH", the_agg_plot$Stock) | 
                    grepl("TUR", the_agg_plot$Stock) | grepl("WIT", the_agg_plot$Stock) | grepl("POK", the_agg_plot$Stock) |   grepl("MUS", the_agg_plot$Stock)  |   grepl("NOP", the_agg_plot$Stock) |
                     grepl("PRA", the_agg_plot$Stock) | grepl("PRA", the_agg_plot$Stock) |  grepl("HKE", the_agg_plot$Stock) |  grepl("WHB 27.3", the_agg_plot$Stock) | grepl("HKE 27.3", the_agg_plot$Stock) |  grepl("CSH 27.4", the_agg_plot$Stock) | grepl("CSH 27.3", the_agg_plot$Stock) | grepl("MON", the_agg_plot$Stock) | grepl("ELE", the_agg_plot$Stock) ),]

    # find order of met
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$LE_MET), mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered <- as.character(dd[,1])


  
  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
  p1_area_pelfishing_pel_land_per_stk <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/1e3, group=met_desc)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
      geom_area(aes(fill=met_desc))  +     labs(y = "Landings (*1000 tons)", x = "Year")   +
      scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 
  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
   p3_area_pelfishing_pel_cpuf_per_stk <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=met_desc)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
      geom_area(aes(fill=met_desc))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
      scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 
  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  p4_area_pelfishing_pel_vpuf_per_stk <- ggplot(the_agg_plot4, aes(x=as.character(Year), y=value/a_unit, group=met_desc)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
      geom_area(aes(fill=met_desc))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
      scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 


 # paper
 a_width <- 6000 ; a_height <- 6500
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot_per_stk_land_and_CPUF_rev_per_species.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
library(ggpubr)
ggarrange(p1_area_pelfishing_pel_land_per_stk, p3_area_pelfishing_pel_cpuf_per_stk, p4_area_pelfishing_pel_vpuf_per_stk, ncol=3, common.legend = TRUE, legend="right")

dev.off()







##!!!!!!!!!!!!!!!!!!!!!!##
## A SUMMARY BARPLOT WITH AVERAGE OVER THE PERIOD 
##!!!!!!!!!!!!!!!!!!!!!!##

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
#load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]


### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_EURO_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")



  count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
    dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
    dd$met_desc <- friendly_met_names(dd)
  
    dd <- cbind.data.frame(Year=y, dd)
   
   if(a_variable %in% c("KEUROallsp","KKGallsp","LE_KG_LITRE_FUEL")){
   
       marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 1, sum, na.rm=TRUE) # in row
       percent_over_stk <- sweep(dd[, paste0(prefixes[count], spp)] , 1,   marginal_sum_catches, FUN="/")*100
       a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_stk))
       # tapply(a_data$LE_KG_LITRE_FUEL, a_data$met_desc, sum, na.rm=TRUE)
       # reshape first
       library(data.table)
       a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
       a_long <- as.data.frame(a_long)
    
      }
   if(!a_variable %in% c("KEUROallsp","KKGallsp","LE_KG_LITRE_FUEL")){
   
       marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 2, sum, na.rm=TRUE) # in col
       percent_over_met <- sweep(dd[, paste0(prefixes[count], spp)] , 2,   marginal_sum_catches, FUN="/")*100
       a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_met))
       # tapply(a_data$LE_KG_LITRE_FUEL, a_data$met_desc, sum, na.rm=TRUE)
       # reshape first
       library(data.table)
       a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
       a_long <- as.data.frame(a_long)
 
   }  
    
     
    a_long$value <- a_long$value* a_long[, a_variable]/100 # dispatch with percentage

   
    a_long <- a_long[complete.cases(a_long),]

    a_long$Stock <- sapply(strsplit(as.character(a_long$Var), split="_"), function(x) x[3])
    
   # assign area to species as a proxy of stock
   a_long$Stock <- paste(a_long$Stock, sapply(strsplit(as.character(a_long$LE_MET), split="_"), function(x) x[2]))

   colnames(a_long)[colnames(a_long) %in% c(a_variable)] <- "Total"
   
  
   a_long$LE_MET <- paste(a_long$LE_MET, the_names[count])
   a_long <- as.data.frame(a_long)
   a_long$LE_MET <- factor(a_long$LE_MET)
   
     if(y==years[1]){
     agg <- cbind.data.frame(as.data.frame(a_long))
     }
      else{
      agg <- rbind.data.frame(as.data.frame(agg),
             cbind.data.frame(a_long))
     }
     
   }

     the_agg <- rbind.data.frame(as.data.frame(the_agg), as.data.frame(agg))
 }


   # filtering the ratios:
   # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
   #ss <- the_agg[the_agg$Var %in%  paste0("LE_KG_", spp) & as.numeric(as.character(the_agg$value))>2000000, ]  # < 2000tons?
   #the_agg <- the_agg[the_agg$Stock %in% unique(ss$Stock),]

   
   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]
 

   
##----------
## PEL
 library(ggplot2)

# PEL
 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])
  
 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), Year=levels(factor(the_agg_plot$Year)))
   dd$met_desc <- friendly_met_names(dd)
 
  

    
 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---
 # PEL
 the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)

     # caution filter out non-relevant species for these fleets
  the_agg_plot<-  the_agg_plot[ !(grepl("COD", the_agg_plot$Stock) | grepl("DAB", the_agg_plot$Stock)  | grepl("FLE", the_agg_plot$Stock)  | 
                    grepl("HOM", the_agg_plot$Stock)  | grepl("LEM", the_agg_plot$Stock)  | grepl("NEP", the_agg_plot$Stock)  | 
                    grepl("NOP 27.3", the_agg_plot$Stock) | grepl("PLE", the_agg_plot$Stock) | grepl("SOL", the_agg_plot$Stock) | 
                    grepl("TUR", the_agg_plot$Stock) | grepl("WIT", the_agg_plot$Stock) | grepl("POK", the_agg_plot$Stock) |
                     grepl("PRA", the_agg_plot$Stock) | grepl("PRA", the_agg_plot$Stock) |  grepl("HKE", the_agg_plot$Stock) |  grepl("WHB", the_agg_plot$Stock) | grepl("HKE 27.3", the_agg_plot$Stock) |  grepl("CSH 27.4", the_agg_plot$Stock) | grepl("CSH 27.3", the_agg_plot$Stock) | grepl("MON", the_agg_plot$Stock) | grepl("ELE", the_agg_plot$Stock) ),]

 # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  ss <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE) & as.numeric(as.character(the_agg_plot$value))>500e3, ]  # 500tons
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% unique(ss$Stock),]

   # find order of stock
  dd <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),]
  dd <- aggregate(dd$value, by=list(dd$Stock), sum)
  dd <- orderBy(~ -x,dd)
  stock_ordered <- as.character(dd[,1])

    # order fleets in the legend
    the_agg_plot$met_desc <- factor(the_agg_plot$met_desc)
  the_agg_plot$met_desc <-  factor(as.character(the_agg_plot$met_desc), levels= levels(the_agg_plot$met_desc)[order(substr(levels(the_agg_plot$met_desc),4,9) )]  )  # reorder the fleet desc in alphabetical order

 #------------
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$Stock <- factor(the_agg_plot1$Stock, level=stock_ordered) # reorder
  p1_barplot_pelfishing_pel_land_per_stk <- ggplot(data=the_agg_plot1, aes(x=Stock, y=value/1000, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Landings (*1000 tons)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$Stock <- factor(the_agg_plot2$Stock, level=stock_ordered) # reorder
  p2_barplot_pelfishing_pel_fuel_per_stk <- ggplot(data=the_agg_plot2, aes(x=Stock, y=value/1e6, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="Fuel (millions litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$Stock <- factor(the_agg_plot3$Stock, level=stock_ordered) # reorder
  p3_barplot_pelfishing_pel_cpuf_per_stk <- ggplot(data=the_agg_plot3, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
   the_agg_plot4$Stock <- factor(the_agg_plot4$Stock, level=stock_ordered) # reorder
  p4_barplot_pelfishing_pel_vpuf_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "VPUF (euro per litre)", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p4)


  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
   the_agg_plot5$Stock <- factor(the_agg_plot5$Stock, level=stock_ordered) # reorder
  p5_barplot_pelfishing_pel_fpuc_per_stk <- ggplot(data=the_agg_plot5, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "litre per kg catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p4)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
   the_agg_plot6$Stock <- factor(the_agg_plot6$Stock, level=stock_ordered) # reorder
  p6_barplot_pelfishing_pel_fpuv_per_stk <- ggplot(data=the_agg_plot6, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "litre per euro catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p4)


  a_width <- 4200 ; a_height <- 6500
   namefile <- paste0("barplot_mean_fuel_efficiency_per_stock_", years[1], "-", years[length(years)],  a_comment, "_PEL_plot_land_and_CPUF_and_VPUF_rev_per_species.tif")
   tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_barplot_pelfishing_pel_land_per_stk, p2_barplot_pelfishing_pel_fuel_per_stk, p4_barplot_pelfishing_pel_vpuf_per_stk, p5_barplot_pelfishing_pel_fpuc_per_stk, 
     ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="right")
dev.off()


