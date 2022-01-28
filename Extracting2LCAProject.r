
 
 
 
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
 # search in Baltic and North Sea
  library(rgdal)
  library(raster)
  fao_areas  <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
  fao_areas  <- fao_areas[ grepl("27", fao_areas$F_AREA) & fao_areas$F_SUBAREA %in% c("27.3", "27.4", "27.2") & fao_areas$F_LEVEL!="MAJOR",] # caution with the MAJOR overidding the over()
  fao_areas <- fao_areas[fao_areas$F_SUBDIVIS  %in% c('27.3.a.20', '27.3.a.21','27.3.c.22',  '27.3.b.23', '27.3.d.24'),]
 
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


 
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ### DEMERSAL FISHERIES WITH LARGE VESSELS 
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   for (y in years){
   print(y)
   load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult
       aggResult1 <- aggResult
       load(file.path(getwd(), "outputs2020_gns", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult for GNS
       aggResult2 <- aggResult
       aggResult <- rbind.data.frame(aggResult1, aggResult2)


       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       levels(aggResult$LE_MET_init) <- gsub("MCD", "CRU", levels(aggResult$LE_MET_init)) # immediate correction to avoid useless historical renaming MCD->CRU
       levels(aggResult$LE_MET_init) <- gsub("OTB_CRU_90-119_0_0", "OTB_DEF_90-119_0_0", levels(aggResult$LE_MET_init)) # immediate correction to avoid an artifical split

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
      aggResult$F_SUBDIVIS <- idx[,"F_SUBDIVIS"]


      ## SELECTED AREAS: WESTERN BALTIC AND KATT-SKAGERRAK
      #We want to keep fao_areas$F_SUBDIVIS %in% c('27.3.a.20', '27.3.a.21','27.3.c.22',  '27.3.b.23', '27.3.d.24')
      aggResult <- aggResult[aggResult$F_SUBDIVIS  %in% c('27.3.a.20', '27.3.a.21','27.3.c.22',  '27.3.b.23', '27.3.d.24'),]


      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156", "120-219", ">=220")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"

      aggResult$LE_MET <- factor(paste0(aggResult$target, "_", aggResult$F_SUBDIVIS,"_", aggResult$LE_MET))


    # debug
    aggResult <- aggResult[aggResult$LE_KG_LITRE >0,]

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
  # capture an export for later doing some quick table
  aggResultPerMetAlly <- NULL
  for (y in years){
   print(y)
     dd <- get(paste0("aggResultPerMet_", y))
     dd <- cbind.data.frame(Year=y, dd)
     aggResultPerMetAlly <- rbind.data.frame(aggResultPerMetAlly, dd)
  }

   save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndRatiosForBottContact_2LCAProject.RData", sep="")))





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


 load(file=file.path(getwd(), "outputs2020", paste("AggregatedSweptAreaPlusMet6AndRatiosForBottContact_2LCAProject.RData", sep="")))  # aggResultPerMetAlly


 library(ggplot2)


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

     # KEEP ONLY MAIN COD AND PLAICE AND HERRING FISHERIES IN THE SELECTED AREAS
     dd <- dd[(dd$LE_KG_COD>50000 & (grepl("27.3.c.22", dd$LE_MET) | grepl("27.3.c.24", dd$LE_MET))) |
                   (dd$LE_KG_PLE>100000 & (grepl("27.3.a.20", dd$LE_MET) | grepl("27.3.a.21", dd$LE_MET))) |
                     (dd$LE_KG_HER>100000 & (grepl("27.3.a.20", dd$LE_MET) | grepl("27.3.a.21", dd$LE_MET))) ,]

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
     long <- melt(setDT(dd[,c("LE_MET",a_variable, colnames(VarThisStk))]), id.vars = c("LE_MET",a_variable), variable.name = "Stock")
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



   save(the_agg, file=file.path(getwd(), "outputs2020", paste("WestBalticKattegatAggForBottContact_2LCAProject.RData", sep="")))
  write.table(the_agg, file=file.path(getwd(), "outputs2020", paste("WestBalticKattegatAggForBottContact_2LCAProject_rev.csv", sep="")), col.names=TRUE, row.names=FALSE, quote=FALSE, sep=";")




## DEM
 a_width <- 3200 ; a_height <- 6500
 library(ggplot2)
 a_comment <- ""
 a_unit <- 1

# DEM
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), year=levels(factor(the_agg_plot$year)))
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

# find order of met
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$LE_MET), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered <- as.character(dd[,1])


 #------------
 namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_FPUC_and_FPUV_2_LCA_project.tif")
 #namefile <- paste0("barplot_mean_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_CPUF_and_VPUF.tif")

 collecting_table <- NULL
 collecting_table2019 <- NULL

 the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
  p0 <- ggplot(data=the_agg_plot0, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Value (KEuros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))
  #print(p0)

the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
  p1 <- ggplot(data=the_agg_plot1, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot2[the_agg_plot2$LE_MET=="27.3_OTB_CRU_32-69_0_0 ",]
  p2 <- ggplot(data=the_agg_plot2, aes(x=LE_MET, y=value/1e3, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="Fuel (thousands litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
  p3 <- ggplot(data=the_agg_plot3, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
    labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot4 <- the_agg_plot4[the_agg_plot4$value<20,]  # debug for other
  p4 <- ggplot(data=the_agg_plot4, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
  labs(y = "VPUF (euro per litre)", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  +
         theme(axis.text.x=element_blank())
        # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p4)

 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot5 <- the_agg_plot5[the_agg_plot5$value<1,]  # debug for other
  p5 <- ggplot(data=the_agg_plot5, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
   labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  +
        # theme(axis.text.x=element_blank())
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))

  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  p6 <- ggplot(data=the_agg_plot6, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
   labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  +
        # theme(axis.text.x=element_blank())
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))


 ## ADD-ON revenue per swept area for bottom fishing
   the_agg_plot7 <- as.data.frame(the_agg_plot[grep("(g)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot7$LE_MET <- gsub("\\(g)","", the_agg_plot7$LE_MET)
    the_agg_plot7 <- the_agg_plot7[!the_agg_plot7$LE_MET=="OTHER_0_0_0 ",] # remove outlier
  the_agg_plot7$LE_MET <- factor(the_agg_plot7$LE_MET, level=fleet_segments_ordered) # reorder
  p7 <- ggplot(data=the_agg_plot7, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
   labs(y = "Euro catch per km-sq", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  +
         theme(axis.text.x=element_blank())
        # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
print(p7)

 ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    the_agg_plot8[the_agg_plot8$ value>100,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  p8 <- ggplot(data=the_agg_plot8, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  +
        # theme(axis.text.x=element_blank())
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
print(p8)


 # for a plot
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p1, p2, p4, p5, ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="bottom")
dev.off()


 # for a plot
 tiff(filename=file.path(getwd(), "outputs2020", "output_plots",  "Euro_catch_per_km-sq_or_per_kg_2_LCAproject.tiff"),   width = a_width, height = 5000,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p7, p8, ncol=1, heights=c(1),common.legend = TRUE, legend="bottom")
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


# export underlying data
write.table(collecting_table,
            file=file.path(getwd(), "outputs2020", "output_plots",
            paste("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_DEM__plot_land_and_FPUC_and_FPUV_2_LCA_project.dat")),
             row.names=FALSE, quote=FALSE, sep=";")













###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ### SMALL VESSELS
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(vmstools)
library(maps)
library(mapdata)

if(.Platform$OS.type == "windows") {
 codePath  <- "D:/FBA/BENTHIS_2020/"
 dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
 outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
 polPath   <- "D:/FBA/BENTHIS/BalanceMaps"
 }


 #years <- 2012:2019
years <- 2005:2019


  load(file=file.path(dataPath ,paste("eflalo_ally_",years[1],"-",years[length(years)],".RData", sep='')))
  eflalo <- eflalo

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
  per_vessel_size <- FALSE
  per_region     <- TRUE

  # search in Baltic and North Sea
  library(rgdal)
  fao_areas  <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
  fao_areas  <- fao_areas[ grepl("27", fao_areas$F_AREA) & fao_areas$F_SUBAREA %in% c("27.3", "27.4", "27.2") & fao_areas$F_LEVEL!="MAJOR",] # caution with the MAJOR overidding the over()

  
  eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=FALSE) # ideally, should have been right=TRUE...


  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


   eflalo <- eflalo[eflalo$VesselSize=="[0,12)",]
  
    eflalo <- eflalo[!is.na(eflalo$VesselSize),]
   eflalo$LE_MET <- factor(eflalo$LE_MET)
   
   
 if(TRUE){
  load(file=file.path(getwd(), "AIS_data", "ais_profile_small_vessels.RData"))  # ais_profile
 
  table.fuelcons.per.engine       <-  read.table(file= file.path(getwd(),"EflaloAndTacsat", "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
  linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
  eflalo$max_consumed_per_h        <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(eflalo$VE_KW))))
  fuel_per_h                      <- function (a,x) a*(x^3)  # cubic law, x is vessel speed

  eflalo$a <- NA
  eflalo$fuelcons_per_h <- NA
  eflalo$LE_MET <- as.character(eflalo$LE_MET)
  eflalo$max_consumed_per_h <-   as.numeric(as.character(eflalo$max_consumed_per_h ))
  ais_profile$max_vessel_speed <- as.numeric(as.character(ais_profile$max_vessel_speed))
  ais_profile$Speed <- as.numeric(as.character(ais_profile$Speed))
  ais_profile$Prop <- as.numeric(as.character(ais_profile$Prop))
  ais_profile$Level6 <- as.character(ais_profile$Level6)
  ais_profile$Weight <- ais_profile[, "Speed"] * ais_profile[, "Prop"]
 
  library(data.table)
  ais_profile_dt <- ais_profile[,c('Level6',"Speed", "Prop")]
  wide_prop <- data.table::dcast(setDT(ais_profile_dt), Level6 ~ Speed, value.var="Prop")
  wide_prop <- as.data.frame(wide_prop)
  rownames(wide_prop) <- wide_prop$Level6
  wide_prop[is.na(wide_prop)] <-0 
  eflalo <- cbind.data.frame(eflalo, wide_prop[eflalo$LE_MET,])

  maxspeed_per_metier <- ais_profile[!duplicated(ais_profile$Level6),c("Level6", "max_vessel_speed")]
  rownames(maxspeed_per_metier) <- maxspeed_per_metier$Level6
  eflalo <- cbind.data.frame(eflalo, max_vessel_speed=maxspeed_per_metier[eflalo$LE_MET,"max_vessel_speed"])
 
  eflalo$a                <- eflalo$max_consumed_per_h /  (eflalo$max_vessel_speed^3)
  cubic_speed             <- matrix(as.numeric(colnames(wide_prop[,-1]))^3,  ncol=length(colnames(wide_prop[,-1])), nrow=nrow(eflalo), byrow=TRUE)
  prop_per_speed_bin      <- eflalo[,c(colnames(wide_prop[,-1]))]
  
  fuelcons_max_per_speed  <- sweep(cubic_speed, 1, eflalo$a, FUN="*")
  colnames(fuelcons_max_per_speed)    <- colnames(wide_prop[,-1])
  
  # Assume max vessel consumption when towing for towed gears (i.e. within a Speed interval assuming fishing) because of the dragging resistance of the towed net
  towedGears  <- c("OTB", "PTB", "DRB", "OTM", "PTM", "SSC", "SDN")
  eflalo$Gear <- substr(eflalo$Level6, 1,3)
  idx <- eflalo$Gear %in% c(towedGears)
  fuelcons_max_per_speed [, c("1.5", "2.5", "3.5")] <- eflalo [,"max_consumed_per_h"] *0.9  #assume 90% full load when a towed net is fishing
  
  eflalo$fuelcons_per_h   <- apply(fuelcons_max_per_speed*prop_per_speed_bin, 1, sum)
  # => doing a weighted average of fuel comsumption with standardized vessel speed profile as weight (then lower than assuming vessel always at max_speed)
  
 
  # back up if some missing metiers in the sampled AIS vessels....
  load(file=file.path(getwd(), "AIS_data", "fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m.RData"))    # get fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m (but a rough average)
  eflalo$LE_MET5 <- substr(as.character(eflalo$LE_MET), 1,7) 
  fuel_cons_in_trip_per_level5_per_hour_for_vessels_under_12m <-   cbind.data.frame(fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m, LE_MET5= substr(names(fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m), 1,7)  )
  fuel_cons_in_trip_per_level5_per_hour_for_vessels_under_12m <- tapply(fuel_cons_in_trip_per_level5_per_hour_for_vessels_under_12m[,1], fuel_cons_in_trip_per_level5_per_hour_for_vessels_under_12m$LE_MET5, mean, na.rm=TRUE)
  eflalo$fuel_cons_in_trip_level_5 <-   fuel_cons_in_trip_per_level5_per_hour_for_vessels_under_12m[eflalo$LE_MET5 ]

  eflalo$LE_MET4 <- substr(as.character(eflalo$LE_MET), 1,3) 
  fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m <-   cbind.data.frame(fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m, LE_MET4= substr(names(fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m), 1,3)  )
  fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m <- tapply(fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m[,1], fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m$LE_MET4, mean, na.rm=TRUE)
  fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m <- c(fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m, LHP=as.numeric(fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m["LLD"]), GND=as.numeric(fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m["GNS"])) # a small fix to avoid loosing vessels
  eflalo$fuel_cons_in_trip_level_4 <-   fuel_cons_in_trip_per_level4_per_hour_for_vessels_under_12m[eflalo$LE_MET4 ]
 
  # take the best estimate first, then, if NAs, fill the gaps with lower levels
  eflalo$fuel_cons_in_trip_level <-   eflalo$fuel_cons_in_trip_level_6
  eflalo[is.na(eflalo$fuel_cons_in_trip_level),"fuel_cons_in_trip_level"] <-   eflalo[is.na(eflalo$fuel_cons_in_trip_level),"fuel_cons_in_trip_level_5"]
  eflalo[is.na(eflalo$fuel_cons_in_trip_level),"fuel_cons_in_trip_level"] <-   eflalo[is.na(eflalo$fuel_cons_in_trip_level),"fuel_cons_in_trip_level_4"]
    
  # check
  unique(eflalo[is.na(eflalo$fuel_cons_in_trip_level_4) ,"VE_REF"])     # ideally, we should lost 0 vessels

  eflalo$LE_KG_LITRE_FUEL   <- as.numeric(as.character(eflalo$fuel_cons_in_trip_level)) * eflalo$LE_EFF
 

}
 
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

    eflalo <- eflalo[!is.na(eflalo$VesselSize),]
    eflalo$VesselSize <- factor(eflalo$VesselSize ) 
    eflalo <- eflalo[!is.na(eflalo$LE_KG_LITRE_FUEL),] # loss of a bunch of few vessels


     ## PLOT TIME SERIES OF TRIP EFFORT AND NB OF VESSELS
  
     # marginal sum of euros
     eflalo$toteuros <- apply(eflalo[,grep("EURO", names(eflalo))], 1, sum, na.rm=TRUE)
    
     dd <-  eflalo[,c("VE_REF", "VesselSize", "LE_MET", "LE_EFF", "toteuros", "LE_KG_LITRE_FUEL", "Year")]
     dd <- aggregate(dd[,c("LE_EFF", "toteuros", "LE_KG_LITRE_FUEL")], list(dd$VE_REF, dd$VesselSize, dd$LE_MET, dd$Year), sum, na.rm=TRUE)
     colnames(dd) <- c("VE_REF", "VesselSize", "LE_MET", "Year", "trip_effort_hours", "toteuros", "litre_fuel")


    # a trick to combine both info on the same plot i.e. use a secondary y axis
    library(ggplot2)
    #some_color_vessel_size <- c("(0,12]"="#999999", "(12,18]"="#FFDB6D",  "(18,24]"="#FC4E07",  "(24,40]"="#52854C",  "(40,100]"="#293352")
   # some_color_vessel_size2 <- c("(0,12]"="#999999", "(12,18]"="#ffc207",  "(18,24]"="#c93e05",  "(24,40]"="#416a3c",  "(40,100]"="#293d52")
    some_color_vessel_size <- c("[0,12)"="#999999", "[12,18)"="#FFDB6D",  "[18,24)"="#c93e05",  "[24,40)"="#52854C",  "[40,100)"="#293352")
    some_color_vessel_size2 <- c("[0,12)"="#999999", "[12,18)"="#ffc207",  "[18,24)"="#FC4E07",  "[24,40)"="#416a3c",  "[40,100)"="#293d52")
      dd <- dd[!duplicated(data.frame(dd$VE_REF, dd$Year)),]
      dd$nbvessel <- 1e3 
    p3 <-   ggplot() + geom_bar(data=eflalo, aes(x=as.character(Year), y=LE_EFF, group=VesselSize, fill=VesselSize), size=1.5, position="stack",  stat = "summary", fun = "sum") +
       geom_line(data=dd, aes(x=as.character(Year), y=nbvessel, group=VesselSize, color=VesselSize),size=1.5, stat = "summary", fun = "sum") +   
       geom_line(data=dd, aes(x=as.character(Year), y=litre_fuel, group=1),size=1.2, color=1, linetype = "dashed", stat = "summary", fun = "sum") +   
       geom_line(data=dd, aes(x=as.character(Year), y=toteuros/100, group=1),size=1.2,  color=5, linetype = "dashed", stat = "summary", fun = "sum") +   
       scale_y_continuous(name = "Trip effort hours; fuel use (litre); euros/100", sec.axis = sec_axis(~./1e3, name = "Nb Vessels") )+
       theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  + 
       labs(x = "Year")     + 
       scale_color_manual(values=some_color_vessel_size, name="VesselSize") +  
       scale_fill_manual(values=some_color_vessel_size2) +
       guides(fill =guide_legend(ncol=1)) 
    print(p3)




  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  library(vmstools)
  eflalo <- cbind.data.frame(eflalo,
                  vmstools::ICESrectangle2LonLat(statsq=eflalo$LE_RECT, midpoint=TRUE)
                  )
  eflalo <- eflalo[!is.na(eflalo$SI_LATI),]  # some leaks of data there. i.e. total effort is not complete, so do not use it


  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 if(per_metier_level6 && !per_vessel_size){

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

   #
   eflalo$LE_MET <- factor(eflalo$LE_MET)
   levels(eflalo$LE_MET) <- gsub("MCD_90-119_0_0", "DEF_90-119_0_0", levels(eflalo$LE_MET)) # immediate correction to avoid an artifical split
   levels(eflalo$LE_MET) <- gsub("_MCD_>=120", "_DEF_>=120", levels(eflalo$LE_MET)) # immediate correction to avoid an artifical split


   # code small vs large mesh
   eflalo$target <- factor(eflalo$LE_MET) # init
   code <- sapply(strsplit(levels(eflalo$target), split="_"), function(x) x[3])
   levels(eflalo$target) <- code
   levels(eflalo$target)[levels(eflalo$target) %in% c(">=105","100-119","90-119",">=120","90-104", ">=220", "70-89", ">=157", ">=156", "110-156", "120-219", "90-99")] <- "LargeMesh"
   levels(eflalo$target)[!levels(eflalo$target) %in% "LargeMesh"] <- "SmallMesh"

   dd <- tapply(eflalo$LE_EFF, paste0(eflalo$target, "_", eflalo$F_SUBAREA, "_", eflalo$LE_MET), sum)


   pel <- dd[grep("SmallMesh",names(dd))]
   pel <- pel[order(pel, decreasing=TRUE)]
   oth_mets_pel <-  names(pel)[cumsum(pel)/sum(pel)>.70]
   oth_mets_pel <- c(oth_mets_pel, "NA_27.3_No_Matrix6", "NA_27.4_No_Matrix6")

   dem <- dd[grep("LargeMesh",names(dd))]
   dem <- dem[order(dem, decreasing=TRUE)]
   oth_mets_dem <-  names(dem)[cumsum(dem)/sum(dem)>.75]
   oth_mets_dem <- c(oth_mets_dem, "NA_27.3_No_Matrix6", "NA_27.4_No_Matrix6")

  eflalo$LE_MET_init <- eflalo$LE_MET
  eflalo$LE_MET <-  factor(paste0(eflalo$target, "_", eflalo$F_SUBAREA, "_", eflalo$LE_MET))
  levels(eflalo$LE_MET)[levels(eflalo$LE_MET) %in% oth_mets_pel] <- "SmallMesh_OTHER_0_0_0"
  levels(eflalo$LE_MET)[levels(eflalo$LE_MET) %in% oth_mets_dem] <- "LargeMesh_OTHER_0_0_0"

 }




  
 


  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # OTH species

   spp <- gsub("LE_KG_", "", colnames(eflalo)[grepl("LE_KG", colnames(eflalo))])
   spp <- spp[!spp %in% c("SPECS", "LITRE_FUEL")]


   dd <- apply(eflalo[,paste0("LE_EURO_",spp)], 2, sum, na.rm=TRUE)
   dd <- dd[order(dd, decreasing=TRUE)]
   main_species <-gsub("LE_EURO_", "", names(dd[cumsum(dd)/sum(dd)<0.93]) )
   oth_species <- gsub("LE_EURO_", "", names(dd[cumsum(dd)/sum(dd)>=0.93]) )
   eflalo$LE_EURO_OTH <- apply(eflalo[,paste0("LE_EURO_",oth_species)], 1, sum, na.rm=TRUE)
   eflalo$LE_KG_OTH <- apply(eflalo[,paste0("LE_KG_",oth_species)], 1, sum, na.rm=TRUE)

   spp <- c(main_species, "OTH")
   eflalo <- eflalo[,!colnames(eflalo) %in% paste0("LE_EURO_",oth_species)]
   eflalo <- eflalo[,!colnames(eflalo) %in% paste0("LE_KG_",oth_species)]
   

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # compute some ratios (to plot across spp)
  
  eflalo$KKGallsp <- apply (eflalo[, paste0('LE_KG_', spp)], 1, sum, na.rm=TRUE) /1e3 # in tons
  eflalo$KEUROallsp <- apply (eflalo[, paste0('LE_EURO_', spp)], 1, sum, na.rm=TRUE) / 1e3 # in thousands euros
  
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

  dd <- eflalo[, paste0('LE_EURO_', spp)]/ eflalo[, paste0('LE_KG_', spp)]
  colnames(dd) <- paste0('LE_MPRICE_', spp)
  dd <- do.call(data.frame,lapply(dd, function(x) replace(x, is.infinite(x) | is.nan(x) , NA)))
  eflalo <- cbind.data.frame (eflalo, dd)
  eflalo$mpriceallsp <- apply (eflalo[, paste0('LE_MPRICE_', spp)], 1, mean, na.rm=TRUE)

    idx_cols <- grepl("LE_VPUF_", names(eflalo))    
    dd <- apply (eflalo[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x))[1]
               })
    eflalo$sp_with_max_vpuf <-   gsub("LE_VPUF_", "", names(eflalo[,idx_cols])[dd])          

    idx_cols <- grepl("LE_CPUE_", names(eflalo))    
    dd <- apply (eflalo[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x))[1]
               })
    eflalo$sp_with_max_cpue <-   gsub("LE_CPUE_", "", names(eflalo[,idx_cols])[dd])          

   idx_cols <- grepl("LE_CPUF_", names(eflalo))    
   dd <- apply (eflalo[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x)) [1]
               })
   eflalo$sp_with_max_cpuf <-   gsub("LE_CPUF_", "", names(eflalo[,idx_cols])[dd])          

    
   idx_cols <- grepl("LE_VPUFSWA_", names(eflalo))    
   dd <- apply (eflalo[,idx_cols], 1, function (x) {
               idx <- which.max(as.numeric(x))[1]
               })
   eflalo$sp_with_max_vpufswa <-   gsub("LE_VPUFSWA_", "", names(eflalo[,idx_cols])[dd])          



   
   # just for info and a rough approximation
    total_kg1 <- tapply(eflalo$LE_KG_SPECS, list(eflalo$LE_MET), sum, na.rm=TRUE)
    total_kg2 <- tapply(eflalo$LE_KG_SPECS, list(eflalo$LE_MET, as.character(eflalo$sp_with_max_cpue)), sum, na.rm=TRUE)
    an_order <- total_kg1 [order(total_kg1, decreasing=TRUE)]
    xx<- round(total_kg2[names(an_order),])   # cumul over period


   # HERE a SUBSET FOR SARA H.
   eflalo <- eflalo[eflalo$LE_DIV %in% c("3AN", "3AS", "3B"),]
   

   # agg
    agg_by <- c("LE_MET", "Year")

   # aggregate ("sum" if absolute value, "mean" if ratio)
    nm <- names(eflalo)
    library(data.table)
    # for sum
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('LE_EFF', nm)
    idx.col.kkg       <- grep('KKGallsp', nm, fixed=TRUE)
    idx.col.keuro     <- grep('KEUROallsp', nm, fixed=TRUE)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort, idx.col.kkg, idx.col.keuro)
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
    idx.col.5     <- grep('mpriceallsp', nm, fixed=TRUE)
    idx.col.cpue     <- grep('LE_CPUE_', nm)
    idx.col.cpuf     <- grep('LE_CPUF_', nm)
    idx.col.vpuf     <- grep('LE_VPUF_', nm)
    idx.col.mprice     <- grep('LE_MPRICE_', nm)
    idx.col <- c(idx.col.1, idx.col.2, idx.col.3, idx.col.5, idx.col.cpue,idx.col.cpuf, idx.col.vpuf, idx.col.mprice)
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

   
    # then do some estimates, caution, after the aggregation for those ones 
    # litre per kilo catch
    aggResultPerMet$FPUCallsp <- aggResultPerMet$LE_KG_LITRE_FUEL/(aggResultPerMet$KKGallsp*1000) 
    aggResultPerMet$FPUCallsp [is.infinite(aggResultPerMet$FPUCallsp)] <- 0

    # litre per euro catch
    aggResultPerMet$FPUVallsp <-  aggResultPerMet$LE_KG_LITRE_FUEL/(aggResultPerMet$KEUROallsp*1000)
    aggResultPerMet$FPUVallsp [is.infinite(aggResultPerMet$FPUVallsp)] <- 0

    # debug outlier  e.g. SAL?
    aggResultPerMet[aggResultPerMet$FPUCallsp>50, c("FPUCallsp", "FPUVallsp")] <- 50
    
    
    aggResultPerMet2LCA <- aggResultPerMet[,c("LE_MET", "Year", "LE_KG_LITRE_FUEL",  "LE_KG_COD",  "LE_KG_HER", "LE_KG_PLE", "KKGallsp", "KEUROallsp", "FPUCallsp")]
    colnames (aggResultPerMet2LCA)[colnames(aggResultPerMet2LCA) %in% "LE_KG_LITRE_FUEL" ] <- "LITRE_FUEL"
 
 
  save(aggResultPerMet2LCA, file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],"_2LCA.RData", sep=""))) 
  write.table(aggResultPerMet2LCA, file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],"_2LCA.csv", sep="")),
    col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)

 
 
 


 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ### PELAGIC FISHERIES WITH LARGE VESSELS 
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
 
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   for (y in years){
     load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6_",y,".RData") ))  # aggResult
         
       aggResult$LE_MET_init <- factor(aggResult$LE_MET_init)
       colnames(aggResult)[ colnames(aggResult) =="LE_MET_init"] <- "LE_MET"
        
       ##!#!#!#!caution#!#!#!#!
       ##!! remove the difference among OTM and PTM
       aggResult[grepl("PTM",aggResult$LE_MET), "LE_KG_LITRE_FUEL"] <-  aggResult[grepl("PTM",aggResult$LE_MET), "LE_KG_LITRE_FUEL"] *2 # because pair trawling
       levels(aggResult$LE_MET) <- gsub("OTM", "TM", levels(aggResult$LE_MET))
       levels(aggResult$LE_MET) <- gsub("PTM", "TM", levels(aggResult$LE_MET))
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
      aggResult$F_SUBDIVIS <- idx[,"F_SUBDIVIS"]


      ## SELECTED AREAS: WESTERN BALTIC AND KATT-SKAGERRAK
      #We want to keep fao_areas$F_SUBDIVIS %in% c('27.3.a.20', '27.3.a.21','27.3.c.22',  '27.3.b.23', '27.3.d.24')
      aggResult <- aggResult[aggResult$F_SUBDIVIS  %in% c('27.3.a.20', '27.3.a.21','27.3.c.22',  '27.3.b.23', '27.3.d.24'),]


      # code small vs large mesh
      aggResult$target <- aggResult$LE_MET # init
      code <- sapply(strsplit(levels(aggResult$target), split="_"), function(x) x[3])
      levels(aggResult$target) <- code
      levels(aggResult$target)[levels(aggResult$target) %in% c(">=105","100-119","90-119",">=120","90-104", "110-156", "120-219", ">=220")] <- "LargeMesh"
      levels(aggResult$target)[!levels(aggResult$target) %in% "LargeMesh"] <- "SmallMesh"

         # remove
       aggResult <- aggResult[!aggResult$target %in% "LargeMesh",] # we don´t want to see the LargeMesh in this routine.
       aggResult <- aggResult[!grepl("OTB", aggResult$LE_MET),] # we don´t want to see the LargeMesh in this routine.
    
      
      aggResult$LE_MET <- factor(paste0(aggResult$target, "_", aggResult$F_SUBDIVIS,"_", aggResult$LE_MET))


    # debug
    aggResult <- aggResult[aggResult$LE_KG_LITRE >0,]

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
  # capture an export for later doing some quick table
  aggResultPerMetAlly <- NULL
  for (y in years){
   print(y)
     dd <- get(paste0("aggResultPerMet_", y))
     dd <- cbind.data.frame(Year=y, dd)
     aggResultPerMetAlly <- rbind.data.frame(aggResultPerMetAlly, dd)
  }

   save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel_2LCAProject.RData", sep=""))) 




 
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
load(file=file.path(getwd(), "outputs2020_pel", paste("aggResultPerMetAllyMet6AndRatiosForPel_2LCAProject.RData", sep="")))  # aggResultPerMetAlly
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_KG_",  "LE_MPRICE_")
 the_names <- c("(z)","(a)","(b)", "(c)", "(d)", "(e)", "(f)",  "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]

     # get percent per stock for sectorisation

      # KEEP ONLY MAIN COD AND PLAICE FISHERIES IN THE SELECTED AREAS
     dd <- dd[dd$LE_KG_HER>50000 & (grepl("27.3.c.22", dd$LE_MET) | grepl("27.3.c.24", dd$LE_MET) | (grepl("27.3.a.20", dd$LE_MET)) | grepl("27.3.a.21", dd$LE_MET)) ,]

  
     PercentThisStk <- dd[paste0(prefixes[count],spp)] / apply(dd[paste0(prefixes[count],spp)], 1, sum, na.rm=TRUE)*100
     if(prefixes[count]=="LE_MPRICE_")   PercentThisStk <- (dd[paste0(prefixes[count],spp)]*dd[paste0("LE_KG_",spp)]) / apply(dd[paste0(prefixes[count],spp)]*dd[paste0("LE_KG_",spp)], 1, sum, na.rm=TRUE)*100
  
     colnames(PercentThisStk)  <- paste0("Percent_",spp)
     dd <- cbind.data.frame (dd, PercentThisStk)
     VarThisStk <- sweep(dd[,colnames(PercentThisStk)]/100, 1, dd[,a_variable], FUN="*")
     colnames(VarThisStk)  <- spp
     dd <- cbind.data.frame (dd, VarThisStk)
     # reshape
     library(data.table)
     long <- melt(setDT(dd[,c("LE_MET",a_variable, colnames(VarThisStk))]), id.vars = c("LE_MET",a_variable), variable.name = "Stock")
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



  save(the_agg, file=file.path(getwd(), "outputs2020", paste("WestBalticKattegatAggForPelagics_2LCAProject.RData", sep="")))
  write.table(the_agg, file=file.path(getwd(), "outputs2020", paste("WestBalticKattegatAggForPelagics_2LCAProject.csv", sep="")), col.names=TRUE, row.names=FALSE, quote=FALSE, sep=";")



 ##!!!!!!!!!!!!!!!!!!!!!!##
## PEL 
 a_width <- 3200 ; a_height <- 6500
 library(ggplot2)

 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), year=levels(factor(the_agg_plot$year)))
 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)

   a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}

    # find order of met
  dd <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  dd$LE_MET <- gsub("\\(a)","", dd$LE_MET)
  dd <- aggregate(dd$Total, by=list(dd$LE_MET), a_func_mean)
  dd <- orderBy(~ -x,dd)
  fleet_segments_ordered <- as.character(dd[,1])

 


# PEL
 collecting_table <- NULL
 collecting_table2019 <- NULL
 
 # PEL
  the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
  p0 <- ggplot(data=the_agg_plot0, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Value (KEuros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p0)

 
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
  p1 <- ggplot(data=the_agg_plot1, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
 p2 <- ggplot(data=the_agg_plot2, aes(x=LE_MET, y=value/1e3, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="Fuel (thousands litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
 p3 <- ggplot(data=the_agg_plot3, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  p4 <- ggplot(data=the_agg_plot4, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "VPUF (euro per litre)", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
       theme(axis.text.x=element_blank()) 
        #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  p5 <- ggplot(data=the_agg_plot5, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        # theme(axis.text.x=element_blank()) 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p5)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  p6 <- ggplot(data=the_agg_plot6, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
  #print(p6)

 ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    the_agg_plot8[the_agg_plot8$ value>100,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  p8 <- ggplot(data=the_agg_plot8, aes(x=LE_MET, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))
print(p8)



# for paper:
 #namefile <- paste0("barplot_mean_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_plot_land_and_CPUF_and_VPUF.tif")
 namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_PEL_plot_land_and_FPUC_and_FPUV_2_LCA_project.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
ggarrange(p1, p2, p4, p5, ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="bottom")

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
            paste("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_PEL_plot_land_and_FPUC_and_FPUV_2_LCA_project.dat")),
             row.names=FALSE, quote=FALSE, sep=";")
            






