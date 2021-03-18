
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


 years <- 2012:2019
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
 per_vessel_size <- TRUE
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

    #head(aggResult)
    #range(aggResult$effort_mins)
 

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

    dd <- apply (aggResult, 1, function (x) {
               idx_cols <- grepl("LE_VPUF_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_VPUF_", "", names(x[idx_cols])[idx])
               })
    aggResult$sp_with_max_vpuf <- dd          

    dd <- apply (aggResult, 1, function (x) {
               idx_cols <- grepl("LE_CPUE_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_CPUE_", "", names(x[idx_cols])[idx])
               })
    aggResult$sp_with_max_cpue <- dd          

    dd <- apply (aggResult, 1, function (x) {
               idx_cols <- grepl("LE_CPUF_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_CPUF_", "", names(x[idx_cols])[idx])
               })
    aggResult$sp_with_max_cpuf <- dd          
   
    dd <- apply (aggResult, 1, function (x) {
               idx_cols <- grepl("LE_VPUFSWA_", names(x))
               idx <- which.max(as.numeric(x[idx_cols]))
               gsub("LE_VPUFSWA_", "", names(x[idx_cols])[idx])
               })
    aggResult$sp_with_max_vpufswa <- dd          

    # capture an export for quickmap2020.r
    save(aggResult, file=file.path(getwd(), "outputs2020_pel", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_", y, ".RData", sep=""))) 


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
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort)
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
    idx.col.cpue     <- grep('LE_CPUE_', nm)
    idx.col.cpuf     <- grep('LE_CPUF_', nm)
    idx.col.vpuf     <- grep('LE_VPUF_', nm)
    idx.col.vpuswa     <- grep('LE_VPUFSWA_', nm)
    idx.col <- c(idx.col.1, idx.col.2, idx.col.3, idx.col.4, idx.col.cpue,idx.col.cpuf, idx.col.vpuf, idx.col.vpuswa)
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


   assign(paste0("aggResultPerMet_", y), aggResultPerMet)
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
  #save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep=""))) 


  load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosForPel.RData", sep="")))  # aggResultPerMetAlly
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
 
  
  # same but for litre fuel
  aggResultPerMetAlly$LE_LITRE_FUEL <- aggResultPerMetAlly$LE_KG_LITRE_FUEL  # a tip for ordering
  nm <- colnames(aggResultPerMetAlly)
  tot <- aggregate(aggResultPerMetAlly[, grepl("LE_LITRE_", nm)], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
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
  a_width <- 4000;  a_height <- 2000
  namefile <- paste0("barplot_fuel_efficiency_pelagics_per_species.tif")
  tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
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
    
   p <- ggplot(data=long[long$Species %in% c("HER", "MAC", "SPR", "WHB", "HOM", "OTH", "SAN", "NOP", "NOP", "WHB"),], aes(x=Species, y=value))  +
           geom_boxplot(outlier.size = -1, fill='#A4A4A4', color="black") +  scale_color_grey() + facet_wrap(~VarType, scales="free")   + labs(y = "", x = "Species") + 
           theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  print(p)
  
  dev.off()
  #write.table(a_summary, "clipboard", sep="\t", row.names=TRUE, col.names=TRUE)


     

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##

 long <- NULL
 agg <- NULL
 
 #a_variable <- "LE_KG_LITRE_FUEL"
 #prefix     <- "LE_KG_"

 #a_variable <- "CPUEallsp"
 #prefix    <- "LE_CPUE_"

 #a_variable <- "CPUFallsp"
 #prefix    <- "LE_CPUF_"
 
 #a_variable <- "VPUFallsp"
 #prefix    <- "LE_VPUF_"

 #a_variable <- "VPUFSWAallsp"
 #prefix    <- "LE_VPUFSWA_"
 
 variables <- c("LE_KG_LITRE_FUEL", "CPUEallsp", "CPUFallsp", "VPUFallsp", "VPUFSWAallsp")
 prefixes  <- c("LE_KG_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_",  "LE_VPUFSWA_")
 
 count <- 0
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     dd <- get(paste0("aggResultPerMet_", y))
     # get percent per stock for sectorisation


     PercentThisStk <- dd[paste0(prefixes[count],spp)] / apply(dd[paste0(prefixes[count],spp)], 1, sum, na.rm=TRUE)*100
     colnames(PercentThisStk)  <- paste0("Percent_",spp)
     dd <- cbind.data.frame (dd, PercentThisStk)
     VarThisStk <- sweep(dd[,colnames(PercentThisStk)]/100, 1, dd[,a_variable], FUN="*")
     colnames(VarThisStk)  <- spp
     dd <- cbind.data.frame (dd, VarThisStk)
     # reshape
     library(data.table)
     long <- melt(setDT(dd[,c("LE_MET",a_variable, colnames(VarThisStk))]), id.vars = c("LE_MET",a_variable), variable.name = "Stock")

     #as.data.frame(long)
     long <- long[complete.cases(long),]
     
     if(y==years[1]){
     agg <- cbind.data.frame(long, year=y)
     }
      else{
      agg <- rbind.data.frame(agg,
             cbind.data.frame(long, year=y))
     }
   }


 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 library(ggplot2)
 if(a_variable=="LE_KG_LITRE_FUEL") {a_ylab <- "Fuel use (litre)"; ylims=c(0,max(as.data.frame(agg)[,a_variable],100000))}
 if(a_variable=="CPUEallsp") {a_ylab <- "CPUE (kg per effort)";  ylims=c(0,max(as.data.frame(agg)[,a_variable],15))}
 if(a_variable=="CPUFallsp") {a_ylab <- "CPUF (kg per litre)";  ylims=c(0,max(as.data.frame(agg)[,a_variable],15))}
 if(a_variable=="VPUFallsp") {a_ylab <- "VPUF  (euro per litre)";  ylims=c(0,max(as.data.frame(agg)[,a_variable],15))}
 if(a_variable=="VPUFSWAallsp") {a_ylab <- "VPUFSWA  (euro per swept area)";  ylims=c(0,max(as.data.frame(agg)[,a_variable],100000))}

 a_width <- 9500 ; a_height <- 4200
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")

 # pel
 namefile <- paste0("barplot_fuel_efficiency_pel_", a_variable, "_", years[1], "-", years[length(years)], a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  the_agg <- agg[grep("SmallMesh",agg$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
  p <- ggplot(data=the_agg, aes(x=LE_MET, y=value, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat="identity")   + labs(y = a_ylab, x = "Fleet-segments")  + #ylim(ylims[1], ylims[2]) +
       scale_fill_manual(values=some_color_species) + facet_grid(. ~ year) + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##



 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 9500 ; a_height <- 4200
 library(ggplot2)


# pel
 namefile <- paste0("ts_fuel_efficiency_pel_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- agg[grep("SmallMesh",agg$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)  
 p <- ggplot(the_agg, aes(x=as.character(year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
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
 a_width <- 9500 ; a_height <- 4200
 library(ggplot2)
 
# pel
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- agg[grep("SmallMesh",agg$LE_MET),]
 
 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg$LE_MET)), Stock=levels(factor(the_agg$Stock)), year=levels(factor(the_agg$year)))
 dd$value <- 0
 dd[,a_variable] <- 0
 dd <- dd[,colnames(the_agg)]
 rownames(the_agg) <- paste0(the_agg$LE_MET,the_agg$Stock,the_agg$year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg),]
 the_agg <- rbind.data.frame(the_agg, dd)
 #---
 
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)  
 p <- ggplot(the_agg, aes(x=as.character(year), y=value, group=Stock)) + 
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


 variables <- c("LE_KG_LITRE_FUEL", "CPUEallseg", "CPUFallseg", "VPUFallseg")
 prefixes  <- c("LE_LITRE_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_")


 count <- 0
 for(a_variable in variables){
    count <- count+1

  a_long <- NULL
  long <- NULL
  for (y in years){
     dd <- get(paste0("aggResultPerMet_", y))
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

  a_width <- 9000 ; a_height <- 4000
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")

   some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")


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

 
# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
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
  
    # caution filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !(  grepl("CSH", the_agg$Stock) | grepl("MUS", the_agg$Stock) | grepl("SAN 27.2", the_agg$Stock) | grepl("SPR 27.2", the_agg$Stock)  | grepl("WHG 27.2", the_agg$Stock) |   grepl("MAC 27.3", the_agg$Stock) | grepl("NOP 27.2", the_agg$Stock) | grepl("COD", the_agg$Stock)  | grepl("HAD", the_agg$Stock) |  grepl("PRA", the_agg$Stock) |  grepl("HOM", the_agg$Stock) | grepl("HKE", the_agg$Stock) | grepl("DAB", the_agg$Stock)  | grepl("FLE", the_agg$Stock)  | grepl("HOM 27.3", the_agg$Stock)  | grepl("LEM", the_agg$Stock)  | grepl("NEP", the_agg$Stock)  | grepl("NOP 27.3", the_agg$Stock) | grepl("PLE", the_agg$Stock) | grepl("SOL", the_agg$Stock) | grepl("TUR", the_agg$Stock) | grepl("WIT", the_agg$Stock) | grepl("POK", the_agg$Stock) | grepl("WHB", the_agg$Stock) | grepl("CSH 27.3", the_agg$Stock) | grepl("MON", the_agg$Stock) | grepl("ELE", the_agg$Stock) ),]
  
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




