
 setwd (file.path("D:","FBA","BENTHIS_2020"))   # adapt to your need

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

some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
                         

 per_metier_level6 <- TRUE
 per_vessel_size <- FALSE
 per_region     <- TRUE
 
 # search in Baltic and North Sea
 fao_areas  <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 fao_areas  <- fao_areas[ grepl("27", fao_areas$F_AREA) & fao_areas$F_SUBAREA %in% c("27.3", "27.4", "27.2") & fao_areas$F_LEVEL!="MAJOR",] # caution with the MAJOR overidding the over()
 
     
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 #---
 # find metiers lvl6 to merge
 if(per_metier_level6 && !per_vessel_size){
   res <- NULL
   for (y in years){
      load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult for towed gears (trawl and dredge) & seines 
      aggResult1 <- aggResult
      load(file.path(getwd(), "outputs2020_gns", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult for GNS
      aggResult2 <- aggResult
      aggResult <- rbind.data.frame(aggResult1, aggResult2)
    
      aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
      levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
      levels(aggResult$LE_MET_init) <- gsub("OTB_CRU_90-119_0_0", "OTB_DEF_90-119_0_0", levels(aggResult$LE_MET_init)) # immediate correction to avoid an artifical split
    
     
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
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156", "120-219", ">=220")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
    
      dd <- tapply(aggResult$effort_mins, paste0(aggResult$target, "_", aggResult$F_SUBAREA, "_", aggResult$LE_MET_init), sum)
      res <- rbind.data.frame(res, cbind.data.frame(names(dd), dd))
      }
      
   pel <- res[grep("SmallMesh",res[,1]),]
   pel <- aggregate(pel$dd, list(pel[,1]), sum)
   pel <- orderBy(~ -x, pel)
   oth_mets_pel <- as.character(pel[cumsum(pel[,2])/sum(pel[,2])>.9,1]) # 75% in effort in pel
  
   dem <- res[grep("LargeMesh",res[,1]),]
   dem <- aggregate(dem$dd, list(dem[,1]), sum)
   dem <- orderBy(~ -x, dem)
   oth_mets_dem <- as.character(dem[cumsum(dem[,2])/sum(dem[,2])>.95,1]) # 75% in effort in dem
 
   # but keep two exeption anyway to get some representation for SSC and GNS 
   #LargeMesh_27.3_SSC_DEF_>=120_0_0 
   oth_mets_dem <- oth_mets_dem[!oth_mets_dem %in% c('LargeMesh_27.3_SSC_DEF_>=120_0_0')]
 
 # met to keep
 oth_mets <- c(oth_mets_dem, oth_mets_pel, "27.3_No_Matrix6_[12,18)","27.4_No_Matrix6", "27.3_No_Matrix6","27.4_No_Matrix6", paste0("NA","_") )
 oth_mets_27_3 <- oth_mets[grepl("27.3", oth_mets)]
 oth_mets_27_4 <- oth_mets[grepl("27.4", oth_mets)]
 }
 #----
 
  #---
 # find metiers lvl6-Vsize to merge
 if(per_metier_level6 && per_vessel_size){
   res <- NULL
   for (y in years){
       load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsize_",y,".RData") ))  # aggResult for towed gears (trawl and dredge) & seines 
      aggResult1 <- aggResult
      load(file.path(getwd(), "outputs2020_gns", paste0("AggregatedSweptAreaPlusMet6AndVsize_",y,".RData") ))  # aggResult for GNS
      aggResult2 <- aggResult
      aggResult <- rbind.data.frame(aggResult1, aggResult2)
    
      aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
      levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
      levels(aggResult$LE_MET_init) <- gsub("OTB_CRU_90-119_0_0", "OTB_DEF_90-119_0_0", levels(aggResult$LE_MET_init)) # immediate correction to avoid an artifical split

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
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156", "120-219", ">=220")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
    
      dd <- tapply(aggResult$effort_mins, paste0(aggResult$target, "_", aggResult$F_SUBAREA, "_", aggResult$LE_MET_init, "_", aggResult$VesselSize), sum)
      res <- rbind.data.frame(res, cbind.data.frame(names(dd), dd))
      }
      
   pel <- res[grep("SmallMesh",res[,1]),]
   pel <- aggregate(pel$dd, list(pel[,1]), sum)
   pel <- orderBy(~ -x, pel)
   oth_mets_pel <- as.character(pel[cumsum(pel[,2])/sum(pel[,2])>.75,1]) # 75% in effort in pel
  
   dem <- res[grep("LargeMesh",res[,1]),]
   dem <- aggregate(dem$dd, list(dem[,1]), sum)
   dem <- orderBy(~ -x, dem)
   oth_mets_dem <- as.character(dem[cumsum(dem[,2])/sum(dem[,2])>.75,1]) # 75% in effort in dem
 
  
   # but keep two exeption anyway to get some representation for SSC and GNS 
   #LargeMesh_27.3_SSC_DEF_>=120_0_0 
   oth_mets_dem <- oth_mets_dem[!oth_mets_dem %in% c('LargeMesh_27.4_SSC_DEF_>=120_0_0_[24,40)', 'LargeMesh_27.4_SSC_DEF_>=120_0_0_[18,24)', 'LargeMesh_27.3_PTB_DEF_>=105_1_110_[12,18)')]

 
 # met to keep
 oth_mets <- c(oth_mets_dem, oth_mets_pel, "27.3_No_Matrix6_[12,18)","27.4_No_Matrix6_[12,18)", "27.3_No_Matrix6_[18,24)","27.4_No_Matrix6_[18,24)", paste0("NA","_",levels(aggResult$VesselSize)) )
 oth_mets_27_3 <- oth_mets[grepl("27.3", oth_mets)]
 oth_mets_27_4 <- oth_mets[grepl("27.4", oth_mets)]
 }
 #----
 
 

 
      
 # aggregation per metier this year
 for (y in years){
    if(!per_metier_level6 && !per_vessel_size){
        load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlus_",y,".RData") ))  # aggResult
        aggResult$LE_MET_init <- factor(aggResult$LE_MET)
        levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming of MCD->CRU
  
              
        #correct some met:
        levels(aggResult$LE_MET)[levels(aggResult$LE_MET)=="OT_CRU"] <- "OT_MIX_NEP"
        levels(aggResult$LE_MET)[levels(aggResult$LE_MET)=="OT_MIX_DMF_PEL"] <- "OT_DMF_PEL"
        levels(aggResult$LE_MET)[levels(aggResult$LE_MET)=="OT_DEM_PEL"] <- "OT_DMF_PEL"
        levels(aggResult$LE_MET)[levels(aggResult$LE_MET)=="OT_DMF_PEL"] <- "OT_DMF"
        aggResult <- aggResult[!is.na(aggResult$LE_MET),]
    }
    if(per_metier_level6 && !per_vessel_size) {
       load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult
       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
       
       # dd <- tapply(aggResult$effort_mins, aggResult$LE_MET_init, sum)
       # dd <- dd[order(-dd)]
       # names(dd[cumsum(dd)/sum(dd)>.99])
       colnames(aggResult)[ colnames(aggResult) =="LE_MET_init"] <- "LE_MET"
       aggResult$LE_MET <- factor(aggResult$LE_MET)
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% oth_mets_27_3] <- "27.3_OTHER_0_0"
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% oth_mets_27_4] <- "27.4_OTHER_0_0"
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "No_matrix6"] <- "OTHER_0_0"
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "NA"] <- "OTHER_0_0"
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "DRB_MOL_>=0_0_0"] <- "DRB_MOL_>0_0_0"

      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156", "120-219", ">=220")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
      aggResult$LE_MET <- paste0(aggResult$target,"_",aggResult$LE_MET)

    }
   
     if(per_metier_level6 && !per_vessel_size) {
       load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult
       aggResult1 <- aggResult
       load(file.path(getwd(), "outputs2020_gns", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult for GNS
       aggResult2 <- aggResult
       aggResult <- rbind.data.frame(aggResult1, aggResult2)
 
       
       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
       levels(aggResult$LE_MET_init) <- gsub("OTB_CRU_90-119_0_0", "OTB_DEF_90-119_0_0", levels(aggResult$LE_MET_init)) # immediate correction to avoid an artifical split

       # dd <- tapply(aggResult$effort_mins, aggResult$LE_MET_init, sum)
       # dd <- dd[order(-dd)]
       # names(dd[cumsum(dd)/sum(dd)>.99])
       colnames(aggResult)[ colnames(aggResult) =="LE_MET_init"] <- "LE_MET"
       aggResult$LE_MET <- factor(aggResult$LE_MET)
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "DRB_MOL_>=0_0_0"] <- "DRB_MOL_>0_0_0"
       
   
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
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156", "120-219", ">=220")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
  
      aggResult$LE_MET <- factor(paste0(aggResult$target, "_", aggResult$F_SUBAREA,"_", aggResult$LE_MET))
      
      # manage the OTHER mets
      smallmesh_oth_mets_27_3 <- oth_mets_27_3[grepl("SmallMesh",oth_mets_27_3)]
      smallmesh_oth_mets_27_4 <- oth_mets_27_4[grepl("SmallMesh",oth_mets_27_4)]
      largemesh_oth_mets_27_3 <- oth_mets_27_3[grepl("LargeMesh",oth_mets_27_3)]
      largemesh_oth_mets_27_4 <- oth_mets_27_4[grepl("LargeMesh",oth_mets_27_4)]
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% smallmesh_oth_mets_27_3] <- "SmallMesh_27.3_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% smallmesh_oth_mets_27_4] <- "SmallMesh_27.4_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% largemesh_oth_mets_27_3] <- "LargeMesh_27.3_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% largemesh_oth_mets_27_4] <- "LargeMesh_27.4_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% oth_mets[!oth_mets %in% c(smallmesh_oth_mets_27_3, smallmesh_oth_mets_27_4, largemesh_oth_mets_27_3, largemesh_oth_mets_27_4)]] <- "LargeMesh_OTHER_0_0_0"
      levels(aggResult$LE_MET)[grepl("NA", levels(aggResult$LE_MET)) ] <- "NA_OTHER_0_0_0"
      
      # caution - a small fix for an annoying renaming of segment
      aggResult$LE_MET <- factor(aggResult$LE_MET)
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "LargeMesh_27.4_OTB_CRU_>=120_0_0"] <- "LargeMesh_27.4_OTB_DEF_>=120_0_0"
      aggResult$LE_MET <- factor(aggResult$LE_MET)


  }

     if(per_metier_level6 && per_vessel_size) {
       load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsize_",y,".RData") ))  # aggResult for towed gears (trawl and dredge) & seines 
       aggResult1 <- aggResult
       load(file.path(getwd(), "outputs2020_gns", paste0("AggregatedSweptAreaPlusMet6AndVsize_",y,".RData") ))  # aggResult for GNS
       aggResult2 <- aggResult
       aggResult <- rbind.data.frame(aggResult1, aggResult2)
      
       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
       levels(aggResult$LE_MET_init) <- gsub("OTB_CRU_90-119_0_0", "OTB_DEF_90-119_0_0", levels(aggResult$LE_MET_init)) # immediate correction to avoid an artifical split

       # dd <- tapply(aggResult$effort_mins, aggResult$LE_MET_init, sum)
       # dd <- dd[order(-dd)]
       # names(dd[cumsum(dd)/sum(dd)>.99])
       colnames(aggResult)[ colnames(aggResult) =="LE_MET_init"] <- "LE_MET"
       aggResult$LE_MET <- factor(aggResult$LE_MET)
       levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "DRB_MOL_>=0_0_0"] <- "DRB_MOL_>0_0_0"
       
   
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
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"
  
      aggResult$LE_MET <- factor(paste0(aggResult$target, "_", aggResult$F_SUBAREA,"_", aggResult$LE_MET, "_", aggResult$VesselSize))
      
      # manage the OTHER mets
      smallmesh_oth_mets_27_3 <- oth_mets_27_3[grepl("SmallMesh",oth_mets_27_3)]
      smallmesh_oth_mets_27_4 <- oth_mets_27_4[grepl("SmallMesh",oth_mets_27_4)]
      largemesh_oth_mets_27_3 <- oth_mets_27_3[grepl("LargeMesh",oth_mets_27_3)]
      largemesh_oth_mets_27_4 <- oth_mets_27_4[grepl("LargeMesh",oth_mets_27_4)]
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% smallmesh_oth_mets_27_3] <- "SmallMesh_27.3_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% smallmesh_oth_mets_27_4] <- "SmallMesh_27.4_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% largemesh_oth_mets_27_3] <- "LargeMesh_27.3_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% largemesh_oth_mets_27_4] <- "LargeMesh_27.4_OTHER_0_0"
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% oth_mets[!oth_mets %in% c(smallmesh_oth_mets_27_3, smallmesh_oth_mets_27_4, largemesh_oth_mets_27_3, largemesh_oth_mets_27_4)]] <- "LargeMesh_OTHER_0_0_0"
      levels(aggResult$LE_MET)[grepl("NA", levels(aggResult$LE_MET)) ] <- "NA_OTHER_0_0_0"
      
      # caution - a small fix for an annoying renaming of segment
      aggResult$LE_MET <- factor(aggResult$LE_MET)
      levels(aggResult$LE_MET)[levels(aggResult$LE_MET) %in% "LargeMesh_27.4_OTB_CRU_>=120_0_0_[24,40)"] <- "LargeMesh_27.4_OTB_DEF_>=120_0_0_[24,40)"
      aggResult$LE_MET <- factor(aggResult$LE_MET)


  }

    #head(aggResult)
    #range(aggResult$effort_mins)
          
          
    # debug divide by 0 or almost 0
    aggResult <- aggResult[aggResult$LE_KG_LITRE >0,]
    aggResult[, paste0('LE_KG_', spp)]  [aggResult[, paste0('LE_KG_', spp)] <0.1] <- 0 
    aggResult[, paste0('LE_EURO_', spp)]  [aggResult[, paste0('LE_EURO_', spp)] <1] <- 0 

    aggResult$KKGallsp <- apply (aggResult[, paste0('LE_KG_', spp)], 1, sum, na.rm=TRUE) /1e3 # in tons
    aggResult$KEUROallsp <- apply (aggResult[, paste0('LE_EURO_', spp)], 1, sum, na.rm=TRUE) / 1e3 # in thousands euros

    # compute some ratios
    #dd <- sweep(aggResult[, paste0('LE_KG_', spp)], 1,  aggResult$KKGallsp*1e3, FUN="/")
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

    # check
    #aggResult[aggResult$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0",]
    #bb <- which(aggResult[aggResult$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0","LE_MPRICE_PLE"]>10)
    # vv <- aggResult[aggResult$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0",]
    #vv[bb,]


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

 
    # capture an export for quickmap2020.r
     if(per_metier_level6 && per_vessel_size) {
       save(aggResult, file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_", y, ".RData", sep=""))) 
     }
     if(per_metier_level6 && !per_vessel_size) {
       save(aggResult, file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndRatiosForBottContact_", y, ".RData", sep=""))) 
     }
   
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
   print(y)
     dd <- get(paste0("aggResultPerMet_", y))
     dd <- cbind.data.frame(Year=y, dd)
     aggResultPerMetAlly <- rbind.data.frame(aggResultPerMetAlly, dd)
  }   
  #save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContact.RData", sep=""))) 
  #save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep=""))) 


  #-----------------
  #-----------------
  load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
  
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
  namefile <- paste0("ts_LPUE_", years[1], "-", years[length(years)],  "_DEM_PEL_rev.tif")
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(a_lpue_plot, ncol=1, common.legend = TRUE, legend="right")

  dev.off()



  
  #-----------------
  #-----------------
  load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
  spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
 
  unique(aggResultPerMetAlly$LE_MET) 
 
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
  tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
  tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
  tot[,-1] <- round(tot[,-1]/1e6,2) # millions litres or thousand tons or euros
  head(tot, 5)

  # same but for euros
  aggResultPerMetAlly$LE_EURO_LITRE_FUEL <- aggResultPerMetAlly$LE_KG_LITRE_FUEL  # a tip for ordering
  nm <- colnames(aggResultPerMetAlly)
  tot <- aggregate(aggResultPerMetAlly[, grepl("LE_EURO_", nm)], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
  tot <- orderBy(~ - LE_EURO_LITRE_FUEL, tot)
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
  
  
  # dem
  library(ggplot2)
  a_width <- 2000;  a_height <- 5500
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
   p <- ggplot(data=long[!long$Species %in% c( "MAC", "HOM", "WHB", "ELE"),], aes(x=Species, y=value))  +
           geom_boxplot(outlier.size = -1, fill='#A4A4A4', color="black") +  scale_color_grey() + 
            facet_wrap(~VarType, scales="free", ncol=1, labeller=as_labeller(var_names),  strip.position = "left") +  
            labs(y = "", x = "Species") + 
           theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8), strip.background = element_blank(),strip.placement = "outside")


  namefile <- paste0("barplot_fuel_efficiency_bottcontact_per_species-2005-2019_rev.tif")
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
    print(p)
  
  dev.off()
  #write.table(a_summary, "clipboard", sep="\t", row.names=TRUE, col.names=TRUE)


 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## a barplot---------------
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
 
 variables <- c("LE_KG_LITRE_FUEL", "CPUEallsp", "CPUFallsp", "VPUFallsp", "VPUFSWAallsp", "KKGallsp", "KEUROallsp")
 prefixes  <- c("LE_KG_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_",  "LE_VPUFSWA_", "LE_KG_", "LE_EURO_")
 
 count <- 0
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- as.data.frame(aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ])
 
     # get percent per stock for sectorisation

     # debug SAN
     dd[grepl("LargeMesh",dd$LE_MET) & !grepl("OTHER",dd$LE_MET), paste0(prefixes[count], "SAN")] <- 0 # because SAN creates misleading distorsion on plots...

     PercentThisStk <- sweep(dd[paste0(prefixes[count],spp)],1, apply(dd[paste0(prefixes[count],spp)], 1, sum, na.rm=TRUE), FUN="/")*100
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
 if(a_variable=="KKGallsp") {a_ylab <- "Landings (tons)";  ylims=c(0,max(as.data.frame(agg)[,a_variable],100000))}
 if(a_variable=="KEUROallsp") {a_ylab <- "Landings  (keuros)";  ylims=c(0,max(as.data.frame(agg)[,a_variable],100000))}

  a_width <- 9500 ; a_height <- 4000
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")

 # dem
 namefile <- paste0("barplot_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)], a_comment, "_DEM_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  the_agg <- agg[grep("LargeMesh",agg$LE_MET),]
  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
  p <- ggplot(data=the_agg, aes(x=LE_MET, y=value, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat="identity")   + labs(y = a_ylab, x = "Fleet-segments")  + #ylim(ylims[1], ylims[2]) +
       scale_fill_manual(values=some_color_species) + facet_grid(. ~ year) + theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  print(p)
dev.off()

 # pel
 namefile <- paste0("barplot_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)], a_comment, "_PEL_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
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
 a_width <- 9500 ; a_height <- 4000
 library(ggplot2)

 # dem
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- agg[grep("LargeMesh",agg$LE_MET),]
  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=Stock), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=Stock), size=3)   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1)) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
 dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- agg[grep("SmallMesh",agg$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)  
 p <- ggplot(the_agg, aes(x=as.character(year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=Stock), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=Stock), size=3) +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1)) +
     xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##



 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 a_width <- 9500 ; a_height <- 4000
 library(ggplot2)

 # dem
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 
 the_agg <- as.data.frame(agg[grep("LargeMesh",agg$LE_MET),])
 
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

  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes( fill=Stock))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1)) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- as.data.frame(agg[grep("SmallMesh",agg$LE_MET),])
 
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
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1)) +
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


 variables <- c("LE_KG_LITRE_FUEL", "CPUEallseg", "CPUFallseg", "VPUFallseg", "KKGallseg", "KEUROallseg")
 prefixes  <- c("LE_LITRE_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_")


 count <- 0
 for(a_variable in variables){
    count <- count+1

  a_long <- NULL
  long <- NULL
  for (y in years){
    # dd <- get(paste0("aggResultPerMet_", y))
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

  # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  nm <- colnames(aggResultPerMetAlly)
  tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
  tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
  tot[,-1] <- round(tot[,-1]) # kg
  head(tot, 5)
  colnames(tot)[1] <- "LE_MET" 
  a_long_for_filter <- melt(setDT(tot[,c("LE_MET", paste0("LE_KG_", spp))]), id.vars = c("LE_MET"), variable.name = "Var2", value.name="value2")
  a_long_for_filter$Var2 <- gsub("LE_KG_", prefixes[count], a_long_for_filter$Var2)
  long <- merge(long, a_long_for_filter, by.x=c("LE_MET", "Var"), by.y=c("LE_MET", "Var2"))
  long <- long[long$value2>5000,] # here the actual filtering....i.e. keep only seg and value when total catch kg is > threshold in kg
  long <- as.data.frame(long)
  long <- long[,c("LE_MET","Var", "Year","Stock", "value", a_variable)]
  
  
  
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

  a_width <- 9500 ; a_height <- 4000
   a_comment <- "" ; if(per_metier_level6) a_comment <- "_met6";  if(per_vessel_size) a_comment <- paste0(a_comment,"_vsize") ; if(per_region) a_comment <- paste0(a_comment,"_region")

   some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

 # dem
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  the_agg <- long[grep("LargeMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)
  
  # caution filter out non-relevant species for these fleets
  #the_agg<-  the_agg[! (grepl("OTHER", the_agg$Stock)  | grepl("NOP", the_agg$Stock) | grepl("HER", the_agg$Stock)  | grepl("WHB", the_agg$Stock) | grepl("SPR", the_agg$Stock) | grepl("HOM", the_agg$Stock) | grepl("CSH", the_agg$Stock) | grepl("PRA", the_agg$Stock) | grepl("MAC", the_agg$Stock) | grepl("SAN", the_agg$Stock) | grepl("ELE", the_agg$Stock)| grepl("MUS", the_agg$Stock) | grepl("WHG", the_agg$Stock)   ),]
  the_agg<-  the_agg[! (grepl("NOP", the_agg$Stock) | grepl("HER", the_agg$Stock)  | grepl("WHB", the_agg$Stock) | grepl("SPR", the_agg$Stock) | grepl("HOM", the_agg$Stock) | grepl("CSH", the_agg$Stock) | grepl("PRA", the_agg$Stock) | grepl("MAC", the_agg$Stock) | grepl("SAN", the_agg$Stock) | grepl("ELE", the_agg$Stock)| grepl("MUS", the_agg$Stock) | grepl("WHG", the_agg$Stock)   ),]
  
 library(ggplot2) 
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_line(aes(color=LE_MET), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=LE_MET), size=3)   +
    scale_color_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1)) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)

   # caution - filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !( grepl("COD", the_agg$Stock) | grepl("DAB", the_agg$Stock)  | grepl("FLE", the_agg$Stock)  | grepl("HOM 27.3", the_agg$Stock)  | grepl("LEM", the_agg$Stock)  | grepl("NEP", the_agg$Stock)  | grepl("NOP 27.3", the_agg$Stock) | grepl("PLE", the_agg$Stock) | grepl("SOL", the_agg$Stock) | grepl("TUR", the_agg$Stock) | grepl("WIT", the_agg$Stock) | grepl("POK", the_agg$Stock) | grepl("WHB", the_agg$Stock) | grepl("CSH 27.3", the_agg$Stock) | grepl("MON", the_agg$Stock) | grepl("ELE", the_agg$Stock) ),]


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
 a_width <- 9500 ; a_height <- 4000
 library(ggplot2)

 # dem
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

 the_agg <- long[grep("LargeMesh",long$LE_MET),]

  # caution - filter out non-relevant species for these fleets
  the_agg<-  the_agg[! (grepl("OTHER", the_agg$Stock)  | grepl("NOP", the_agg$Stock) | grepl("HER", the_agg$Stock) | grepl("SPR", the_agg$Stock)  | grepl("WHB", the_agg$Stock) | grepl("HOM", the_agg$Stock) | grepl("CSH", the_agg$Stock) | grepl("PRA", the_agg$Stock) | grepl("MAC", the_agg$Stock) | grepl("SAN", the_agg$Stock) | grepl("ELE", the_agg$Stock)| grepl("MUS", the_agg$Stock) | grepl("WHG", the_agg$Stock)   ),]

  
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

  the_agg$LE_MET <- gsub("LargeMesh_", "", the_agg$LE_MET)


 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   labs(y = a_ylab) +
  geom_area(aes( fill=LE_MET))  +     labs(y = a_ylab, x = "Year")   +
      scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1)) +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]

   # caution filter out non-relevant species for these fleets
  the_agg<-  the_agg[ !(grepl("COD", the_agg$Stock) | grepl("DAB", the_agg$Stock)  | grepl("FLE", the_agg$Stock)  | grepl("HOM 27.3", the_agg$Stock)  | grepl("LEM", the_agg$Stock)  | grepl("NEP", the_agg$Stock)  | grepl("NOP 27.3", the_agg$Stock) | grepl("PLE", the_agg$Stock) | grepl("SOL", the_agg$Stock) | grepl("TUR", the_agg$Stock) | grepl("WIT", the_agg$Stock) | grepl("POK", the_agg$Stock) | grepl("WHB", the_agg$Stock) | grepl("CSH 27.3", the_agg$Stock) | grepl("MON", the_agg$Stock) | grepl("ELE", the_agg$Stock) ),]

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
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
 
 head(aggResultPerMetAlly[aggResultPerMetAlly$LE_MET=="SmallMesh_27.3_OTB_CRU_32-69_0_0",])

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
     dd[grepl("70-99_0",dd$LE_MET), "met_desc3"] <- "fish (70-119mm)"   
     dd[grepl("90-119_0_0",dd$LE_MET), "met_desc3"] <- "fish (90-119mm)"   
     dd[grepl("100-119_0_0",dd$LE_MET), "met_desc3"] <- "fish (100-119mm)"   
     dd[grepl("110-119_0_0",dd$LE_MET), "met_desc3"] <- "fish (110-119mm)"   
     dd[grepl("120-219_0",dd$LE_MET), "met_desc3"] <- "fish (120-219mm)"   
     dd[grepl("<16_0_0",dd$LE_MET), "met_desc3"] <- "forage fish (<16mm)"   
     dd[grepl("SPF_16-31_0_0",dd$LE_MET), "met_desc3"] <- "pelagics (16-31mm)"   
     dd[grepl("_PS_SPF_>0_0_0",dd$LE_MET), "met_desc3"] <- "pelagics"   
     dd[grepl("SPF_32-69_0_0",dd$LE_MET), "met_desc3"] <- "pelagics (32-69mm)"   
     dd[grepl("DEF_16-31_0_0",dd$LE_MET), "met_desc3"] <- "d/pelagics (16-31mm)"   
     dd[grepl("CRU",dd$LE_MET), "met_desc3"] <- "crustaceans" 
     dd[grepl("MOL",dd$LE_MET), "met_desc3"] <- "molluscs" 
     dd$met_desc2[is.na(dd$met_desc2)] <- ""  
     dd$met_desc3[is.na(dd$met_desc3)] <- ""  
     
    return(paste(dd$met_desc1, dd$met_desc2, dd$met_desc3))
    } 
                                   
                                                                     
 library(ggplot2)
 
 # compare prices e.g seine vs. trawls for cod
 #aggResultPerMetAlly[aggResultPerMetAlly$LE_MET=="LargeMesh_27.3_SDN_DEF_>=120_0_0","LE_MPRICE_COD"]
 #[1] 2.766031 3.310001 3.993804 3.396713 2.590793 3.018366 3.013149 3.092763 3.211551 2.927512 3.304162 3.328078 3.332026 3.047696 3.332285
 # aggResultPerMetAlly[aggResultPerMetAlly$LE_MET=="LargeMesh_27.3_OTB_DEF_>=105_1_120","LE_MPRICE_COD"]
 #[1] 1.156881 1.237244 1.215207 1.128402 1.081045 1.105845 1.171007 1.296540 1.363090 1.592291


 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_KG_", "LE_VPUF_", "LE_MPRICE_")
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
 p1_area_bottomfishing_dem_land <- ggplot(the_agg_plot1, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_area(aes(fill=Stock))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p1_bottomfishing_dem_land)

  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_area_bottomfishing_dem_cpuf <- ggplot(the_agg_plot3, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + 
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p3)
 
  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_dem_vpuf <- ggplot(the_agg_plot4, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + 
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p4)

 
  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
 p5_area_bottomfishing_dem_fpuc <- ggplot(the_agg_plot5, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)

 
  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p6_area_bottomfishing_dem_fpuv <- ggplot(the_agg_plot6, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p6)
 

 ### ADD-ON for bottom fishing: revenue per swept area
  the_agg_plot7 <- as.data.frame(the_agg_plot[grep("(g)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot7$LE_MET <- gsub("\\(g)","", the_agg_plot7$LE_MET)
 p7_area_bottomfishing_dem_vswpa <- ggplot(the_agg_plot7, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Euro catch per km-sq", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p7)

 
 ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p8_area_bottomfishing_dem_price <- ggplot(the_agg_plot8, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p8)

 # for paper:
 namefile <- paste0("ts_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot_land_and_FPUE_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_area_bottomfishing_dem_land, p4_area_bottomfishing_dem_vpuf, p5_area_bottomfishing_dem_fpuc, ncol=3, common.legend = TRUE, legend="right")

dev.off()



 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##FORAGE FISH or SMALL MESHES

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
 p1_area_bottomfishing_pel_land <- ggplot(the_agg_plot1, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
 #print(p1)

  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_area_bottomfishing_dem_cpuf <- ggplot(the_agg_plot3, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
 #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_pel_vpuf <- ggplot(the_agg_plot4, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
 #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
 p5_area_bottomfishing_pel_fpuc <- ggplot(the_agg_plot5, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
 #print(p5)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p6_area_bottomfishing_pel_fpuv <- ggplot(the_agg_plot6, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
 #print(p6)
 
  ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p8_area_bottomfishing_pel_price <- ggplot(the_agg_plot8, aes(x=as.character(year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
    xlab("")
 #print(p8)



# PEL: for paper:
 namefile <- paste0("ts_fuel_efficiency_", "mean_fuel_efficiency", "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot_land_and_FPUE_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_area_bottomfishing_pel_land, p4_area_bottomfishing_pel_vpuf, p5_area_bottomfishing_pel_fpuc, ncol=3, common.legend = TRUE, legend="right")

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
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly

## DEM
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

 a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}
 
# find order of LE_MET
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$LE_MET), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered <- as.character(dd[,1])

 # find order of met_desc
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$met_desc), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered2 <- as.character(dd[,1])

 #------------
collecting_table <- NULL
 collecting_table2019 <- NULL
 
 the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot0$met_desc <- factor(the_agg_plot0$met_desc, level=fleet_segments_ordered2) # reorder)
  p0_barplot_bottomfishing_dem_value <- ggplot(data=the_agg_plot0, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Value (KEuros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p0)

the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot1$met_desc <- factor(the_agg_plot1$met_desc, level=fleet_segments_ordered2) # reorder)
  p1_barplot_bottomfishing_dem_land <- ggplot(data=the_agg_plot1, aes(x=met_desc, y=value/1000, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (*1000 tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot2$met_desc <- factor(the_agg_plot2$met_desc, level=fleet_segments_ordered2) # reorder)
 p2_barplot_bottomfishing_dem_fuel <- ggplot(data=the_agg_plot2, aes(x=met_desc, y=value/1e6, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="Fuel (millions litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot3$met_desc <- factor(the_agg_plot3$met_desc, level=fleet_segments_ordered2) # reorder)
 p3_barplot_bottomfishing_dem_cpuf <- ggplot(data=the_agg_plot3, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
    labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot4 <- the_agg_plot4[the_agg_plot4$value<20,]  # debug for other
  the_agg_plot4$met_desc <- factor(the_agg_plot4$met_desc, level=fleet_segments_ordered2) # reorder)
  p4_barplot_bottomfishing_dem_vpuf <- ggplot(data=the_agg_plot4, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
  labs(y = "VPUF (euro per litre)", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
         theme(axis.text.x=element_blank()) 
        # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p4)

 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot5 <- the_agg_plot5[the_agg_plot5$value<1,]  # debug for other
  the_agg_plot5$met_desc <- factor(the_agg_plot5$met_desc, level=fleet_segments_ordered2) # reorder)
  p5_barplot_bottomfishing_dem_fpuc <- ggplot(data=the_agg_plot5, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=10))

  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot6$met_desc <- factor(the_agg_plot6$met_desc, level=fleet_segments_ordered2) # reorder)
  p6_barplot_bottomfishing_dem_fpuv <- ggplot(data=the_agg_plot6, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=8))


          ## ADD-ON revenue per swept area for bottom fishing
  the_agg_plot7 <- as.data.frame(the_agg_plot[grep("(g)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot7$LE_MET <- gsub("\\(g)","", the_agg_plot7$LE_MET)
  the_agg_plot7 <- the_agg_plot7[!the_agg_plot7$LE_MET=="OTHER_0_0_0 ",] # remove outlier
  the_agg_plot7$LE_MET <- factor(the_agg_plot7$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot7$met_desc <- factor(the_agg_plot7$met_desc, level=fleet_segments_ordered2) # reorder)
  p7_barplot_bottomfishing_dem_vswpa <- ggplot(data=the_agg_plot7, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Euro catch per km-sq", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
         theme(axis.text.x=element_blank()) 
        # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
print(p7)

 ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    the_agg_plot8[the_agg_plot8$ value>100,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot8$met_desc <- factor(the_agg_plot8$met_desc, level=fleet_segments_ordered2) # reorder)
  p8_barplot_bottomfishing_dem_price <- ggplot(data=the_agg_plot8, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=0.5, size=12))
print(p8)

 # for paper:
 a_width <- 6000 ; a_height <- 5500
 namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_FPUC_and_FPUV_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p1_barplot_bottomfishing_dem_land, p2_barplot_bottomfishing_dem_fuel, p4_barplot_bottomfishing_dem_vpuf, p5_barplot_bottomfishing_dem_fpuc, ncol=1,
                            heights=c(1,1,1,1.5),common.legend = TRUE, legend="right")
dev.off()




# export underlying data
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot0$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_mean), cv=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot1$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_mean), cv=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot2$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_mean), cv=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot3$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_mean), cv=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot4$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_mean), cv=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot5$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_mean), cv=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot6$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_mean), cv=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_cv))) 
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot7$LE_MET), var="Euro catch per km-sq", average=tapply(the_agg_plot7$Total, the_agg_plot7$LE_MET, a_func_mean), cv=tapply(the_agg_plot7$Total, the_agg_plot7$LE_MET, a_func_cv))) 
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot8$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_mean), cv=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_cv))) 

  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0[the_agg_plot0$year==2019,]$Total, the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot0[the_agg_plot0$year==2019,]$Total, the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1[the_agg_plot1$year==2019,]$Total, the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot1[the_agg_plot1$year==2019,]$Total, the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2[the_agg_plot2$year==2019,]$Total, the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot2[the_agg_plot2$year==2019,]$Total, the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3[the_agg_plot3$year==2019,]$Total, the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot3[the_agg_plot3$year==2019,]$Total, the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4[the_agg_plot4$year==2019,]$Total, the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot4[the_agg_plot4$year==2019,]$Total, the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5[the_agg_plot5$year==2019,]$Total, the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot5[the_agg_plot5$year==2019,]$Total, the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6[the_agg_plot6$year==2019,]$Total, the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot6[the_agg_plot6$year==2019,]$Total, the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET, a_func_cv))) 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot7[the_agg_plot7$year==2019,]$LE_MET), var="Euro catch per km-sq", average=tapply(the_agg_plot7[the_agg_plot7$year==2019,]$Total, the_agg_plot7[the_agg_plot7$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot7[the_agg_plot7$year==2019,]$Total, the_agg_plot7[the_agg_plot7$year==2019,]$LE_MET, a_func_cv))) 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_LargeMesh", seg=levels(the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8[the_agg_plot8$year==2019,]$Total, the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot8[the_agg_plot8$year==2019,]$Total, the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET, a_func_cv))) 



 # for paper:
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  "Euro_catch_per_km-sq_or_per_kg_rev.tiff"),   width = a_width, height = 5000,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p7_barplot_bottomfishing_dem_vswpa, p8_barplot_bottomfishing_dem_price, ncol=1, heights=c(1),common.legend = TRUE, legend="bottom")
dev.off()


##!!!!!!!!!!!!!!!!!!!!!!##
## PEL 

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

  # find order of LE_MET
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$LE_MET), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered <- as.character(dd[,1])

  
 # find order of met_desc
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$met_desc), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered2 <- as.character(dd[,1])

  
# PEL
  the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot0$met_desc <- factor(the_agg_plot0$met_desc, level=fleet_segments_ordered2) # reorder)
  p0_barplot_bottomfishing_pel_value <- ggplot(data=the_agg_plot0, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Value (KEuros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p0)
 

  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot1$met_desc <- factor(the_agg_plot1$met_desc, level=fleet_segments_ordered2) # reorder)
  p1_barplot_bottomfishing_pel_land <- ggplot(data=the_agg_plot1, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p1)
 
 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot2$met_desc <- factor(the_agg_plot2$met_desc, level=fleet_segments_ordered2) # reorder)
  p2_barplot_bottomfishing_pel_fuel <- ggplot(data=the_agg_plot2, aes(x=met_desc, y=value/1e3, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="Fuel (thousands litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p2)
 
 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot3$met_desc <- factor(the_agg_plot3$met_desc, level=fleet_segments_ordered2) # reorder)
  p3_barplot_bottomfishing_pel_cpuf <- ggplot(data=the_agg_plot3, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
    labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p3)
 
  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot4$met_desc <- factor(the_agg_plot4$met_desc, level=fleet_segments_ordered2) # reorder)
  p4_barplot_bottomfishing_pel_vpuf <- ggplot(data=the_agg_plot4, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
  labs(y = "VPUF (euro per litre)", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
         theme(axis.text.x=element_blank()) 
        # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p4)
 
 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot5$met_desc <- factor(the_agg_plot5$met_desc, level=fleet_segments_ordered2) # reorder)
  p5_barplot_bottomfishing_pel_fpuc <- ggplot(data=the_agg_plot5, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
 
  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot6$met_desc <- factor(the_agg_plot6$met_desc, level=fleet_segments_ordered2) # reorder)
  p6_barplot_bottomfishing_pel_fpuv <- ggplot(data=the_agg_plot6, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=12))
 
 
 ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    idx <- the_agg_plot8[the_agg_plot8$LE_MET=="27.4_OTB_DEF_<16_0_0 " & the_agg_plot8$Stock=="SAN" & the_agg_plot8$value>5, "value"] <- 0   # remove outlier
    the_agg_plot8[the_agg_plot8$LE_MET=="27.4_OTB_DEF_<16_0_0 " & the_agg_plot8$Stock=="SAN","value"]
    the_agg_plot8[the_agg_plot8$value>25,"value"] <- 0 # remove outlier
    the_agg_plot8[the_agg_plot8$Total>25,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot8$met_desc <- factor(the_agg_plot8$met_desc, level=fleet_segments_ordered2) # reorder)
  p8_barplot_bottomfishing_pel_price <- ggplot(data=the_agg_plot8, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  print(p8)


 
 # for paper:
a_width <- 6000 ; a_height <- 5500
  namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_PEL_plot_land_and_FPUC_and_FPUV_rev.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  ggarrange(p1_barplot_bottomfishing_pel_land, p2_barplot_bottomfishing_pel_fuel, p4_barplot_bottomfishing_pel_vpuf, p5_barplot_bottomfishing_pel_fpuc, ncol=1,
                            heights=c(1,1,1,2),common.legend = TRUE, legend="right")

dev.off()


 # for paper:
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  "Euro_catch_per_kg_PEL_rev.tiff"),   width = a_width, height = 3000,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
   ggarrange(p8_barplot_bottomfishing_dem_price, ncol=1, heights=c(1),common.legend = TRUE, legend="bottom")

dev.off()


# export underlying data
  a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot0$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_mean), cv=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot1$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_mean), cv=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot2$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_mean), cv=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot3$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_mean), cv=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot4$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_mean), cv=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot5$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_mean), cv=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot6$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_mean), cv=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_cv))) 
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot8$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_mean), cv=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_cv))) 

  collecting_table[,4] <- round(collecting_table[,4],4)
  collecting_table[,5] <- round(collecting_table[,5],4)
# export underlying data
write.table(collecting_table,
            file=file.path(getwd(), "outputs2020", "output_plots", 
            paste("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_DEM_and_PEL_plot_land_and_FPUC_and_FPUV.dat")),
             row.names=FALSE, quote=FALSE, sep=";")
            


  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0[the_agg_plot0$year==2019,]$Total, the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot0[the_agg_plot0$year==2019,]$Total, the_agg_plot0[the_agg_plot0$year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1[the_agg_plot1$year==2019,]$Total, the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot1[the_agg_plot1$year==2019,]$Total, the_agg_plot1[the_agg_plot1$year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2[the_agg_plot2$year==2019,]$Total, the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot2[the_agg_plot2$year==2019,]$Total, the_agg_plot2[the_agg_plot2$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3[the_agg_plot3$year==2019,]$Total, the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot3[the_agg_plot3$year==2019,]$Total, the_agg_plot3[the_agg_plot3$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4[the_agg_plot4$year==2019,]$Total, the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot4[the_agg_plot4$year==2019,]$Total, the_agg_plot4[the_agg_plot4$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5[the_agg_plot5$year==2019,]$Total, the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot5[the_agg_plot5$year==2019,]$Total, the_agg_plot5[the_agg_plot5$year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6[the_agg_plot6$year==2019,]$Total, the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot6[the_agg_plot6$year==2019,]$Total, the_agg_plot6[the_agg_plot6$year==2019,]$LE_MET, a_func_cv))) 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="BottomFishing_SmallOrNoMesh", seg=levels(the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8[the_agg_plot8$year==2019,]$Total, the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot8[the_agg_plot8$year==2019,]$Total, the_agg_plot8[the_agg_plot8$year==2019,]$LE_MET, a_func_cv))) 

  collecting_table2019[,4] <- round(collecting_table2019[,4],4)
  collecting_table2019[,5] <- round(collecting_table2019[,5],4)
# export underlying data
write.table(collecting_table2019,
            file=file.path(getwd(), "outputs2020", "output_plots", 
            paste("barplot_mean_fuel_efficiency_2019_DEM_and_PEL_plot_land_and_FPUC_and_FPUV.dat")),
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
 ## DEM
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
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
 

 
 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_KG_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")


 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
    dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
    dd$met_desc <- friendly_met_names(dd)
  
    dd <- cbind.data.frame(Year=y, dd)
   
    marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 2, sum, na.rm=TRUE)
    percent_over_met <- sweep(dd[, paste0(prefixes[count], spp)] , 2,   marginal_sum_catches, FUN="/")*100
    a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_met))
   
       
    # reshape first
    library(data.table)
    a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
    a_long <- as.data.frame(a_long)
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
   ss <- the_agg[the_agg$Var %in%  paste0("LE_KG_", spp) & as.numeric(as.character(the_agg$value))>2000000, ]  # < 2000tons?
   the_agg <- the_agg[the_agg$Stock %in% unique(ss$Stock),]

   
   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]
 
   # select some species
  the_agg <- the_agg[the_agg$Stock %in% c("COD 27.3", "COD 27.4", "HAD 27.4", "HKE 27.4", "NEP 27.3", "PLE 27.3", "PLE 27.4", "POK 27.4", "SOL 27.3"),]
  
##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
   some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

 library(ggplot2)
 a_comment <- ""
 a_unit <- 1

# DEM
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

 # caution filter out non-relevant species for these fleets
 the_agg_plot<-  the_agg_plot[! (grepl("NOP", the_agg_plot$Stock) | grepl("OTH", the_agg_plot$Stock) | grepl("HER", the_agg_plot$Stock)  | grepl("WHB", the_agg_plot$Stock) | grepl("SPR", the_agg_plot$Stock) | grepl("HOM", the_agg_plot$Stock) | grepl("CSH", the_agg_plot$Stock) | grepl("PRA", the_agg_plot$Stock) | grepl("MAC", the_agg_plot$Stock) | grepl("SAN", the_agg_plot$Stock) | grepl("ELE", the_agg_plot$Stock)| grepl("MUS", the_agg_plot$Stock) | grepl("WHG", the_agg_plot$Stock)   ),]

 

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

  the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)
  
  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  p1_area_bottomfishing_dem_land_per_stk <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/1e6, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  geom_area(aes(fill=met_desc))  +     labs(y = "Landings (*1000 tons)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 
 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  p3_area_bottomfishing_dem_cpuf_per_stk <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
  geom_area(aes(fill=met_desc))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p2)
 
 the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_dem_vpuf_per_stk <- ggplot(the_agg_plot4, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p3)

 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  p5_area_bottomfishing_dem_fpuc_per_stk <- ggplot(the_agg_plot5, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)

 the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  p6_area_bottomfishing_dem_fpuv_per_stk <- ggplot(the_agg_plot6, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p6)



 a_width <- 6200 ; a_height <- 6500
  namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot_per_stk_land_and_FPUC_and_FPUV_per_species.tif")
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_area_bottomfishing_dem_land_per_stk, p4_area_bottomfishing_dem_vpuf_per_stk, p5_area_bottomfishing_dem_fpuc_per_stk, ncol=3, common.legend = TRUE, legend="right")
 dev.off()




#!!!!!!!!!!!!
#!!!!!!!!!!!!
#!!!!!!!!!!!!
# PEL

 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_KG_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")


 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
    dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
    dd$met_desc <- friendly_met_names(dd)
  
    dd <- cbind.data.frame(Year=y, dd)
   
    marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 2, sum, na.rm=TRUE)
    percent_over_met <- sweep(dd[, paste0(prefixes[count], spp)] , 2,   marginal_sum_catches, FUN="/")*100
    a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_met))
   
       
    # reshape first
    library(data.table)
    a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
    a_long <- as.data.frame(a_long)
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
   ss <- the_agg[the_agg$Var %in%  paste0("LE_KG_", spp) & as.numeric(as.character(the_agg$value))>2000e3, ]  # < 5000tons?
   the_agg <- the_agg[the_agg$Stock %in% unique(ss$Stock),]

   
   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]
 
   # select some species
  the_agg <- the_agg[the_agg$Stock %in% c("HOM 27.4","MAC 27.4","NOP 27.4","PRA 27.3", "SAN 27.4","WHG 27.4"),]



##--------
library(ggplot2)
a_comment <- ""
a_unit <- 1

 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

   # caution filter out non-relevant species for these fleets
  the_agg_plot<-  the_agg_plot[ !(grepl("COD", the_agg_plot$Stock) | grepl("DAB", the_agg_plot$Stock)  | grepl("FLE", the_agg_plot$Stock)  | grepl("HOM", the_agg_plot$Stock)  | grepl("LEM", the_agg_plot$Stock)  | grepl("NEP", the_agg_plot$Stock)  | grepl("NOP 27.3", the_agg_plot$Stock) | grepl("PLE", the_agg_plot$Stock) | grepl("SOL", the_agg_plot$Stock) | grepl("TUR", the_agg_plot$Stock) | grepl("WIT", the_agg_plot$Stock) | grepl("POK", the_agg_plot$Stock) | grepl("WHB", the_agg_plot$Stock) | grepl("CSH 27.3", the_agg_plot$Stock) | grepl("MON", the_agg_plot$Stock) | grepl("ELE", the_agg_plot$Stock) ),]

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

  the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)
  
  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  p1_area_bottomfishing_pel_land_per_stk <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/1e3, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
      geom_area(aes(fill=met_desc))  +     labs(y = "Landings (tons)", x = "Year")   +
      scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 
  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)  
  p3_area_bottomfishing_pel_cpuf_per_stk <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +  
      geom_area(aes(fill=met_desc))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
      scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 

 the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_pel_vpuf_per_stk <- ggplot(the_agg_plot4, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p3)

 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  p5_area_bottomfishing_pel_fpuc_per_stk <- ggplot(the_agg_plot5, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)

 the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  p6_area_bottomfishing_pel_fpuv_per_stk <- ggplot(the_agg_plot6, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p6)
 

a_width <- 6200 ; a_height <- 6500
namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot_per_stk_land_and_FPUC_and_FPUV_per_species.tif")
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_area_bottomfishing_pel_land_per_stk, p4_area_bottomfishing_pel_vpuf_per_stk, p5_area_bottomfishing_pel_fpuc_per_stk, ncol=3, common.legend = TRUE, legend="right")

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
 #load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosBottContact.RData", sep="")))  # aggResultPerMetAlly
 load(file=file.path(getwd(), "outputs2020", paste("aggResultPerMetAllyMet6AndRatiosBottContactAndGNS.RData", sep="")))  # aggResultPerMetAlly
 

 
 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "VPUFSWAallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_KG_", "LE_VPUF_", "LE_MPRICE_")
 the_names <- c("(z)", "(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")
 
 


 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
    dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
    dd$met_desc <- friendly_met_names(dd)
  
    dd <- cbind.data.frame(Year=y, dd)
   
    marginal_sum_catches <- apply( dd[,  paste0(prefixes[count], spp)] , 2, sum, na.rm=TRUE)
    percent_over_met <- sweep(dd[, paste0(prefixes[count], spp)] , 2,   marginal_sum_catches, FUN="/")*100
    a_data <- cbind.data.frame(dd[,c("Year","met_desc", "LE_MET", a_variable)], as.data.frame(percent_over_met))
   
       
    # reshape first
    library(data.table)
    a_long <- melt(setDT(a_data[,c("met_desc", "LE_MET", "Year", a_variable, paste0(prefixes[count], spp))]), id.vars = c("met_desc", "LE_MET", "Year", a_variable), variable.name = "Var")
    a_long <- as.data.frame(a_long)
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
   ss <- the_agg[the_agg$Var %in%  paste0("LE_KG_", spp) & as.numeric(as.character(the_agg$value))>2000e3, ]  # < 5000tons?
   the_agg <- the_agg[the_agg$Stock %in% unique(ss$Stock),]

   
   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]
 
  
 
   
##----------
## DEM
 library(ggplot2)

# DEM
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

   # select some species
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% c("COD 27.3", "COD 27.4", "HAD 27.4", "HKE 27.4", "NEP 27.3", "PLE 27.3", "PLE 27.4", "POK 27.4", "SOL 27.3"),]
 
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
 # DEM
 the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)

 
 # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  ss <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE) & as.numeric(as.character(the_agg_plot$value))>500e3, ]  # 500tons
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% unique(ss$Stock),]

  the_agg_plot <- the_agg_plot[!the_agg_plot$Stock %in% c("SAN 27.4",  "SPR 27.4"),]

 
    # find order of met
  dd <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),]
  dd <- aggregate(dd$value, by=list(dd$Stock), sum)
  dd <- orderBy(~ -x,dd)
  stock_ordered <- as.character(dd[,1])

    
 
 #------------
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  the_agg_plot1$Stock <- factor(the_agg_plot1$Stock, level=stock_ordered) # reorder
  p1_barplot_bottomfishing_dem_land_per_stk <- ggplot(data=the_agg_plot1, aes(x=Stock, y=value/1000, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Landings (*1000 tons)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))
  print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
  the_agg_plot2$Stock <- factor(the_agg_plot2$Stock, level=stock_ordered) # reorder
 p2_barplot_bottomfishing_dem_fuel_per_stk <- ggplot(data=the_agg_plot2, aes(x=Stock, y=value/1e6, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="Fuel (millions litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$Stock <- factor(the_agg_plot3$Stock, level=stock_ordered) # reorder
  p3_barplot_bottomfishing_dem_cpuf_per_stk <- ggplot(data=the_agg_plot3, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
   the_agg_plot4$Stock <- factor(the_agg_plot4$Stock, level=stock_ordered) # reorder
  p4_barplot_bottomfishing_dem_vpuf_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "VPUF (euro per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_blank())
  #print(p4)

the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
   the_agg_plot5$Stock <- factor(the_agg_plot5$Stock, level=stock_ordered) # reorder
 p5_barplot_bottomfishing_dem_fpuc_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
    geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per kg catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=12))
 
  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$Stock <- factor(the_agg_plot6$Stock, level=stock_ordered) # reorder
  p6_barplot_bottomfishing_dem_fpuv_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per euro catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  

  
 a_width <- 4200 ; a_height <- 6500
 namefile <- paste0("barplot_mean_fuel_efficiency_per_stock_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_CPUF_and_VPUF_per_species.tif")
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_barplot_bottomfishing_dem_land_per_stk, p2_barplot_bottomfishing_dem_fuel_per_stk, p4_barplot_bottomfishing_dem_vpuf_per_stk, p5_barplot_bottomfishing_dem_fpuc_per_stk, 
            ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="right")
 dev.off()






   
##----------
## PEL
 library(ggplot2)

# PEL
 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])
  
  # select some species
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% c("HOM 27.4","MAC 27.4","NOP 27.4","PRA 27.3", "SAN 27.4","WHG 27.4"),]



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

 
 # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  ss <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE) & as.numeric(as.character(the_agg_plot$value))>500e3, ]  # 500tons
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% unique(ss$Stock),]

  # find order of met
  dd <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),]
  dd <- aggregate(dd$value, by=list(dd$Stock), sum)
  dd <- orderBy(~ -x,dd)
  stock_ordered <- as.character(dd[,1])

  
 # find order of met_desc
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$met_desc), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered2 <- as.character(dd[,1])
 
  #------------
  #------------
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  the_agg_plot1$Stock <- factor(the_agg_plot1$Stock, level=stock_ordered) # reorder
  p1_barplot_bottomfishing_pel_land_per_stk <- ggplot(data=the_agg_plot1, aes(x=Stock, y=value/1000, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Landings (*1000 tons)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))
  print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
  the_agg_plot2$Stock <- factor(the_agg_plot2$Stock, level=stock_ordered) # reorder
 p2_barplot_bottomfishing_pel_fuel_per_stk <- ggplot(data=the_agg_plot2, aes(x=Stock, y=value/1e6, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="Fuel (millions litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$Stock <- factor(the_agg_plot3$Stock, level=stock_ordered) # reorder
  p3_barplot_bottomfishing_pel_cpuf_per_stk <- ggplot(data=the_agg_plot3, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=1, position="right"))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
   the_agg_plot4$Stock <- factor(the_agg_plot4$Stock, level=stock_ordered) # reorder
  p4_barplot_bottomfishing_pel_vpuf_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "VPUF (euro per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_blank())
  #print(p4)

the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
   the_agg_plot5$Stock <- factor(the_agg_plot5$Stock, level=stock_ordered) # reorder
 p5_barplot_bottomfishing_pel_fpuc_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
    geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per kg catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=12))
 
  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$Stock <- factor(the_agg_plot6$Stock, level=stock_ordered) # reorder
  p6_barplot_bottomfishing_pel_fpuv_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=LE_MET)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per euro catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=1, position="right"))  + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  

 
 a_width <- 4200 ; a_height <- 6500
 namefile <- paste0("barplot_mean_fuel_efficiency_per_stock_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_plot_land_and_CPUF_and_VPUF_per_species.tif")
  tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_barplot_bottomfishing_pel_land_per_stk, p2_barplot_bottomfishing_pel_fuel_per_stk, p4_barplot_bottomfishing_pel_vpuf_per_stk, p5_barplot_bottomfishing_pel_fpuc_per_stk, 
            ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="right")
 dev.off()


