
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!COUPLE INTERPOLATED VMS WITH CATCH LANDED!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Add-on to the BENTHIS WP2 workflow. Therefore possible repetation of some steps

cat("start couplingInterpolatedVmsToLandings2020.r\n")  
rm(list=ls())
library(vmstools)
library(maps)
library(mapdata)
library(doBy)

if(.Platform$OS.type == "unix") {
 codePath  <- file.path("/zhome","fe","8","43283","BENTHIS")
 dataPath  <- file.path("/zhome","fe","8","43283","BENTHIS","EflaloAndTacsat")
 #outPath   <- file.path("~","BENTHIS", "outputs")
 outPath   <- file.path("/zhome","fe","8","43283","BENTHIS", "outputs2020_gns")
 polPath   <- file.path("/zhome","fe","8","43283","BENTHIS", "BalanceMaps") 

 ##First read in the arguments listed at the command line
 args=(commandArgs(TRUE))

 ##args is now a list of character vectors
 ## First check to see if arguments are passed.
 ## Then cycle through each element of the list and evaluate the expressions.
  if(length(args)==0){
    print("No arguments supplied.")
    ##supply default values
    year1 <- 2012
    year2 <- 2019
    years <- year1:year2
  }else{
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
  years <- year1:year2
  }
 }
 
 
 if(.Platform$OS.type == "windows") {
    codePath  <- "D:/FBA/BENTHIS_2020/"
    dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
    outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_gns")
    polPath   <- "D:/FBA/BENTHIS/BalanceMaps"
    years <- 2012:2019
 }
 
 
  overwrite <- TRUE
 
  
 if(FALSE){  # do not re-run...this takes ages!

  ##-----------------------------------
  ## SPLIT CATCH AMONG THE INTERPOLATED PINGS
  ##-----------------------------------
  #- PER YEAR
  for (a_year in years){
    cat(paste("Split among interpolated pings", "\n"))

    dir.create(file.path(outPath,a_year,"interpolated", "plus"))

     # per year, load eflalo data,
     load(file.path(dataPath,paste("eflalo_", a_year,".RData", sep=''))); # get the eflalo object
    if(a_year>=2016){
       eflalo <- formatEflalo(get(paste0("eflalo_", a_year))) # format each of the columns to the specified class
    } else{
       eflalo <- formatEflalo(get(paste0("eflalo"))) # format each of the columns to the specified class
    }
     ctry   <- "DNK"
     eflalo <- eflalo[ grep(ctry, as.character(eflalo$VE_REF)),]  # keep the national vessels only.

     eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
     eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
     eflalo                <- orderBy(~VE_REF+FT_DDATIM+FT_REF, data=eflalo)
     eflalo$ID             <- paste(eflalo$VE_REF,eflalo$FT_REF,sep="")
     eflalo$LE_EFF         <- an(difftime(eflalo$FT_LDATIM, eflalo$FT_DDATIM, units="hours"))
     eflalo$dummy          <- 1
     eflalo$LE_EFF         <- eflalo$LE_EFF / merge(eflalo,aggregate(eflalo$dummy,by=list(eflalo$ID),FUN=sum),by.x="ID",by.y="Group.1",all.x=T)$x
     
     # deprecated. Better to inform fuel cons from the VMS  
     #table.fuelcons.per.engine       <-  read.table(file= file.path(dataPath, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
     #linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
     #eflalo$LE_KG_LITRE_FUEL         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(eflalo$VE_KW)))) * eflalo$LE_EFF # Liter per hour * effort this trip in hour


     # Gear codes to keep (with assumed severe bottom impact)
     gears2keep            <- c("GNS")
     eflalo                <- eflalo[which(eflalo$LE_GEAR %in% gears2keep),]

     fls <- dir(file.path(outPath,a_year,"interpolated"))
     fls <- fls[fls!="plus"]

     lst <- list(); count <- 0
     for(iFile in fls){
         cat(paste(a_year, "\n"))
         cat(paste(iFile, "\n"))
         count <- count+1
         load(file.path(outPath,a_year,"interpolated",iFile))  # get  tacsatIntGearVEREF

         a_vid   <-  tacsatIntGearVEREF$VE_REF [1]
         a_gear  <-  tacsatIntGearVEREF$LE_GEAR[1]

         cnm <- colnames(tacsatIntGearVEREF)
         cnm <- cnm[!cnm%in%c("LE_KG_LITRE_FUEL")]
         if(length(grep("LE_KG", cnm))>1 ) cat('this tacsat object has already been merged!! likely to fail.\n')
      
         # avoid redoing if the outcome file already there for this vessel-gear combination
         do_it <-TRUE
         if(overwrite==FALSE) if(length(fls)!=0 && 
            length(grep(paste("tacsatSweptAreaPlus_",a_vid, "_", a_gear, ".RData", sep=""),fls)!=0)) do_it <- FALSE

         
         if(do_it){
           tacsatIntGearVEREF <- tacsatIntGearVEREF[,!colnames(tacsatIntGearVEREF)%in%"LITRE_FUEL"]
           colnames(tacsatIntGearVEREF)[colnames(tacsatIntGearVEREF)=="LE_KG_LITRE_FUEL"] <- "LITRE_FUEL" # force renaming to avoid splitAmongPings() to fail
           tacsatIntGearVEREF      <- splitAmongPings(tacsat=subset(tacsatIntGearVEREF, LE_GEAR == a_gear & VE_REF == a_vid),
                                              eflalo=subset(eflalo, LE_GEAR == a_gear & VE_REF == a_vid),
                                              variable="all",level="day",conserve=T)
                                          # note that we can safely ignore the warning as it corresponds to the 0 catch
                                          # this is because sometimes the declaration of rectangle (in eflalo) does not match the rectangle from VMS points
           colnames(tacsatIntGearVEREF)[colnames(tacsatIntGearVEREF)=="LITRE_FUEL"] <- "LE_KG_LITRE_FUEL" # force renaming for back compatibility
       
           # check e.g. for cod
           #library(raster)
           #plotTools(tacsatIntGearVEREF,level="gridcell", xlim=c(-56,25),ylim=c(45,75),zlim=NULL,log=F, gridcell=c(0.1,0.05), color=NULL, control.tacsat=list(clm="LE_KG_COD"))
           #savePlot(file.path(outPath, a_year, "interpolated", "plus", paste("tacsatSweptAreaPlus_",a_vid, "_", a_gear, "_COD.jpeg", sep="")), type="jpeg")
           #plotTools(subset(eflalo, LE_GEAR == a_gear & VE_REF == a_vid), level="ICESrectangle",xlim=c(-56,25),ylim=c(45,65), zlim=NULL,log=F,color=NULL,control.eflalo=list(clm="LE_KG_COD"))
           #savePlot(file.path(outPath, a_year, "interpolated", "plus", paste("tacsatSweptAreaPlus_",a_vid, "_", a_gear, "_COD_EFLALO.jpeg", sep="")), type="jpeg")


           save(tacsatIntGearVEREF, file=file.path(outPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_",a_vid, "_", a_gear, ".RData", sep="")),compress=T)

           }
         }  # end a_vessel

   }   # end a_year
   
   
  }# end FALSE

  if(FALSE){
 
  ##-----------------------------------
  ## COMPUTE SWEPT AREA
  ##-----------------------------------

  # not computed for GNS...
  
  # Gear codes to keep (with assumed severe bottom impact)
  gears2keep            <- c("GNS")
  netGears            <- c("GNS")
  VMS_ping_rate_in_hour <- 1 # e.g. 1 hour for Denmark (rev(sort(table(intervalTacsat(sortTacsat(tacsat),level="vessel")$INTV))[1])
  

   spp <- c('LITRE_FUEL', 'COD', 'CSH', 'DAB', 'ELE', 'FLE', 'HAD', 'HER', 'HKE', 'HOM',
         'LEM', 'MAC', 'MON', 'MUS', 'NEP', 'NOP', 'PLE', 'POK', 'PRA', 'SAN',
           'SOL', 'SPR', 'TUR', 'WHB', 'WIT', 'WHG',
            'OTH')

   cols2keep <- c("VE_REF", "VE_LEN", "VE_KW", "SI_LATI","SI_LONG","SI_DATE","LE_GEAR","LE_MET","LE_MET_init","SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER","SWEPT_AREA_KM2_UPPER", "GEAR_WIDTH", "SI_DATIM", "SI_FT", "FT_REF") 

   for (a_year in years){
    cat(paste(a_year, "\n"))
    cat(paste("Compute swept area", "\n"))

    fls <- dir(file.path(outPath,a_year,"interpolated", "plus"))
    fls <- fls[grep('.RData', fls)]
    fls <- fls[!fls %in% paste0("tacsatSweptAreaPlus_",a_year,".RData")]

    load(file.path(outPath,a_year,"interpolated", "plus", fls[2])) # get one as an example for the right columns
  
   
    colkg <- colnames(tacsatIntGearVEREF) [ grep('KG', colnames(tacsatIntGearVEREF)) ]
    coleuro <- colnames(tacsatIntGearVEREF) [grep('EURO', colnames(tacsatIntGearVEREF))]
  
    
    #colums_to_keep  <- colnames(tacsatIntGearVEREF) [ ! c(1:ncol(tacsatIntGearVEREF))  %in%  c(colkg, coleuro)  ]
    cols2keep
    colkg_to_keep   <- c(paste('LE_KG_', spp, sep=''))
    coleuro_to_keep <- c(paste('LE_EURO_', spp, sep=''))
    coleuro_to_keep <- coleuro_to_keep[!coleuro_to_keep %in% "LE_EURO_LITRE_FUEL"] # remove a useless naming

    colkg_to_sum    <-  colkg[!colkg %in%  colkg_to_keep]
    coleuro_to_sum  <-  coleuro[!coleuro %in%  coleuro_to_keep]


    lst <- list(); count <- 0  ;vid_with_errors <- NULL
    for(iFile in fls){
     cat(paste(iFile, "\n"))
     count <- count+1
     load(file.path(outPath,a_year,"interpolated", "plus", iFile))

       #- Make selection for gears where you already have gear width and which not
      # compute the swept area
      tacsatIntGearVEREF <- cbind.data.frame(tacsatIntGearVEREF,  data.frame('GEAR_WIDTH'=0, 'SWEPT_AREA_KM2'=0, 'SWEPT_AREA_KM2_LOWER'=0, 'SWEPT_AREA_KM2_UPPER'=0)) # no width for passive gears



     tacsatIntGearVEREF$LE_KG_OTH <- apply(tacsatIntGearVEREF[,colkg_to_sum], 1, sum, na.rm=TRUE)
     tacsatIntGearVEREF$LE_EURO_OTH <- apply(tacsatIntGearVEREF[,coleuro_to_sum], 1, sum, na.rm=TRUE)

      
     lst[[count]] <- tacsatIntGearVEREF[, c(cols2keep, colkg_to_keep, coleuro_to_keep)]

     print(ncol( lst[[count]]))

     }
   cat(paste("saving....", "\n"))
   # caution: the job can get killed silently here if the memory allocated hitting the ceiling...
   tacsatSweptArea   <- do.call(rbind, lst)
   save(tacsatSweptArea, file=file.path(outPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_", a_year, ".RData", sep="")),compress=T)
   cat(paste("saving....ok", "\n"))
   } # end year


    } # end FALSE





  ##-----------------------------------
  ## GRIDDING
  ##-----------------------------------
   #---------------------------------------------------
   # TO DO  from the 'tacsatSweptAreaPlus_ objects':
   # 1. from the catches, figure out what has been fished for, close to each benthic stations....
   # 2. from catches, compute an efficiency indicator: catch per swept area => a way to identify the effective fisheries and priorities areas...
   #                                                                         i.e. what we aim for is high catches with low total swept area.
   # Gear codes to keep (with assumed severe bottom impact)
   gears2keep            <- c("GNS")
   netGears            <- c("GNS")

   for (a_year in years){
    cat(paste(a_year, "\n"))
    cat(paste("Gridding", "\n"))

    load(file=file.path(outPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_", a_year, ".RData", sep="")))
 
     # compute effective effort in minutes
     tacsatSweptArea$effort_mins <- c(0,as.numeric(diff(tacsatSweptArea$SI_DATIM), units='mins'))
     idx <- which( tacsatSweptArea$effort_mins>75 & tacsatSweptArea$LE_GEAR %in% netGears) # if interval > 75 min 
     tacsatSweptArea[ idx, "effort_mins"] <- NA  # exclude change of haul
     idx <- which( tacsatSweptArea$effort_mins <0) #   
     tacsatSweptArea[ idx, "effort_mins"] <- NA  # exclude change of vessel id
     totkg <- apply(tacsatSweptArea[,grep("LE_KG_", colnames(tacsatSweptArea))[-1]], 1, sum)
     idx <- which( totkg ==0) 
     tacsatSweptArea[ idx, "effort_mins"] <- NA   # exclude steaming time for passive gears
     
  
     # retrieve the harbour dep from FT_REF
     load(file=file.path(outPath, a_year, "cleanEflalo.RData"))  # get tacsatp
     tacsatSweptArea$VE_REF_FT_REF <- paste0(tacsatSweptArea$VE_REF,"_",tacsatSweptArea$FT_REF)
     eflalo$VE_REF_FT_REF <- paste0(eflalo$VE_REF,"_",eflalo$FT_REF) 
     dd <- eflalo [!duplicated(eflalo$VE_REF_FT_REF),]
     dd <- dd[,c("VE_REF_FT_REF","FT_DHAR")]
     rownames(dd) <- dd$VE_REF_FT_REF
     tacsatSweptArea$FT_DHAR <- dd[tacsatSweptArea$VE_REF_FT_REF, "FT_DHAR"]
     
     # check
     #levels(factor(tacsatSweptArea$FT_DHAR)) %in% levels(vss$Port) 
   
     # vessel size
     #12-18, 18-24, 24-40, o40
     tacsatSweptArea$VesselSize <- cut(tacsatSweptArea$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=FALSE)
     
    library(vmstools)
    xrange  <- c(-30,50) # ALL
    yrange  <- c(30,81) # ALL
 
    #- Set grid
    resx    <- 1/60 #1 minute
    resy    <- 1/60 #1 minute
    grd     <- createGrid(xrange,yrange,resx=1/60,resy=1/60,type="SpatialGrid",exactBorder=T)

    #- Grid all tacsatSweptArea data
    # Convert all tacsat poins first to SpatialPoints
    coords <- SpatialPoints(cbind(SI_LONG=tacsatSweptArea$SI_LONG,SI_LATI=tacsatSweptArea$SI_LATI))
    idx <- over(coords,grd)
    tacsatSweptArea$grID <- idx

    #- Remove records that are not in the study area
    tacsatSweptArea <- subset(tacsatSweptArea,is.na(grID)==F)

    #-1 Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
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
 
 
 
    # aggregate per VE_REF
    library(data.table)
    nm <- names(tacsatSweptArea)
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('effort_mins', nm)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort)
    DT  <- data.table(tacsatSweptArea) # library data.table for fast grouping replacing aggregate()
    # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    tacsatSweptArea.agg <- DT[,eval(eq1),by=list( VE_REF, FT_DHAR, LE_MET, VE_LEN, VE_KW)]
    tacsatSweptArea.agg <- data.frame( tacsatSweptArea.agg)
    colnames(tacsatSweptArea.agg) <- c("VE_REF", "FT_DHAR", "LE_MET", "VE_LEN", "VE_KW", nm[idx.col.euro], nm[idx.col.kg], nm[idx.col.swpt], nm[idx.col.effectiveeffort])
 
      aggResult<- tacsatSweptArea.agg
   
  
     save(aggResult,file=file.path(outPath,  paste("AggregatedSweptAreaPlusPerVidPerMet6PerHarb_", a_year, ".RData", sep=""))) 

 if(FALSE){
   
    # aggregate per LE_MET
    library(data.table)
    nm <- names(tacsatSweptArea)
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('effort_mins', nm)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort)
    DT  <- data.table(tacsatSweptArea) # library data.table for fast grouping replacing aggregate()
    # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    tacsatSweptArea.agg <- DT[,eval(eq1),by=list(grID, LE_MET)]
    tacsatSweptArea.agg <- data.frame( tacsatSweptArea.agg)
    colnames(tacsatSweptArea.agg) <- c("grID", "LE_MET",  nm[idx.col.euro], nm[idx.col.kg], nm[idx.col.swpt], nm[idx.col.effectiveeffort])

    
     #- Add midpoint of gridcell to dataset
     aggResult <- cbind(tacsatSweptArea.agg,CELL_LONG=coordinates(grd)[tacsatSweptArea.agg$grID,1],
                        CELL_LATI=coordinates(grd)[tacsatSweptArea.agg$grID,2])

     #- Remove records that are not in the study area 
     aggResult       <- subset(aggResult,is.na(grID)==F)

     save(aggResult,file=file.path(outPath,  paste("AggregatedSweptAreaPlus_", a_year, ".RData", sep=""))) 


    # aggregate per LE_MET_init
    library(data.table)
    nm <- names(tacsatSweptArea)
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('effort_mins', nm)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort)
    DT  <- data.table(tacsatSweptArea) # library data.table for fast grouping replacing aggregate()
    # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    tacsatSweptArea.agg <- DT[,eval(eq1),by=list(grID, LE_MET_init)]
    tacsatSweptArea.agg <- data.frame( tacsatSweptArea.agg)
    colnames(tacsatSweptArea.agg) <- c("grID", "LE_MET_init",  nm[idx.col.euro], nm[idx.col.kg], nm[idx.col.swpt], nm[idx.col.effectiveeffort])

    
     #- Add midpoint of gridcell to dataset
     aggResult <- cbind(tacsatSweptArea.agg,CELL_LONG=coordinates(grd)[tacsatSweptArea.agg$grID,1],
                        CELL_LATI=coordinates(grd)[tacsatSweptArea.agg$grID,2])

     #- Remove records that are not in the study area 
     aggResult       <- subset(aggResult,is.na(grID)==F)

     save(aggResult,file=file.path(outPath,  paste("AggregatedSweptAreaPlusMet6_", a_year, ".RData", sep=""))) 
  # DO the plot ordering cell from large revenue to lower revenue  and plot cumsum
  
  
     # aggregate per LE_MET_init & Vessel size
    library(data.table)
    nm <- names(tacsatSweptArea)
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col.swpt     <- grep('SWEPT_AREA_KM2', nm)
    idx.col.effectiveeffort     <- grep('effort_mins', nm)
    idx.col <- c(idx.col.euro, idx.col.kg, idx.col.swpt, idx.col.effectiveeffort)
    DT  <- data.table(tacsatSweptArea) # library data.table for fast grouping replacing aggregate()
    # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    tacsatSweptArea.agg <- DT[,eval(eq1),by=list(grID, LE_MET_init, VesselSize)]
    tacsatSweptArea.agg <- data.frame( tacsatSweptArea.agg)
    colnames(tacsatSweptArea.agg) <- c("grID", "LE_MET_init", "VesselSize", nm[idx.col.euro], nm[idx.col.kg], nm[idx.col.swpt], nm[idx.col.effectiveeffort])

    
     #- Add midpoint of gridcell to dataset
     aggResult <- cbind(tacsatSweptArea.agg,CELL_LONG=coordinates(grd)[tacsatSweptArea.agg$grID,1],
                        CELL_LATI=coordinates(grd)[tacsatSweptArea.agg$grID,2])

     #- Remove records that are not in the study area 
     aggResult       <- subset(aggResult,is.na(grID)==F)

     save(aggResult,file=file.path(outPath,  paste("AggregatedSweptAreaPlusMet6AndVsize_", a_year, ".RData", sep=""))) 
  # DO the plot ordering cell from large revenue to lower revenue  and plot cumsum

 } # end FALSE 
    
   } # end year
    
    

  ##-----------------------------------
  ## GET SOME EFFORT TIME SERIES
  ##-----------------------------------

   # Gear codes to keep (with assumed severe bottom impact)
    gears2keep            <- c("GNS")
   netGears            <- c("GNS")

 
 aggEffortAndFuelAlly <- NULL
 for (a_year in years){
    cat(paste(a_year, "\n"))
    cat(paste("Effort", "\n"))

    rm(tacsatSweptArea)  ; gc()
    load(file=file.path(outPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_", a_year, ".RData", sep="")))
 
     # compute effort in nmin
     tacsatSweptArea$effort_mins <- c(0,as.numeric(diff(tacsatSweptArea$SI_DATIM), units='mins'))
     idx <- which( tacsatSweptArea$effort_mins & tacsatSweptArea$LE_GEAR %in% towedGears > 15) # if interval > 15 min 
     tacsatSweptArea[ idx, "effort_mins"] <- NA  # exclude change of haul
     idx <- which( tacsatSweptArea$effort_mins & tacsatSweptArea$LE_GEAR %in% seineGears > 75) # if interval > 75 min 
     tacsatSweptArea[ idx, "effort_mins"] <- NA  # exclude change of haul
     idx <- which( tacsatSweptArea$effort_mins <0) #   
     tacsatSweptArea[ idx, "effort_mins"] <- NA  # exclude change of vessel id
     
     # vessel size
     #15-18, 18-24, 24-40, o40
     tacsatSweptArea$VesselSize <- cut(tacsatSweptArea$VE_LEN, breaks=c(0,14.99,17.99,23.99,39.99,100), right=FALSE)

     # marginal sum of euros
     tacsatSweptArea$toteuros <- apply(tacsatSweptArea[,grep("EURO", names(tacsatSweptArea))], 1, sum)
    
     dd <-  tacsatSweptArea[,c("VE_REF", "VesselSize", "LE_MET_init", "effort_mins", "toteuros", "LE_KG_LITRE_FUEL")]
     dd <- aggregate(dd[,c("effort_mins", "toteuros", "LE_KG_LITRE_FUEL")], list(dd$VE_REF, dd$VesselSize, dd$LE_MET_init), sum, na.rm=TRUE)
     colnames(dd) <- c("VE_REF", "VesselSize", "LE_MET", "effective_effort_mins", "toteuros", "litre_fuel")

     aggEffortAndFuelAlly <- rbind.data.frame(aggEffortAndFuelAlly, cbind.data.frame(dd, Year=a_year))
     }
     
     aggEffortAndFuelAlly0_15m <- aggEffortAndFuelAlly[aggEffortAndFuelAlly$VesselSize=="[0,15)",] # clean up
     aggEffortAndFuelAlly <- aggEffortAndFuelAlly[aggEffortAndFuelAlly$VesselSize!="[0,15)",] # clean up
    
     save(aggEffortAndFuelAlly0_15m,file=file.path(outPath,  paste("AggregatedEffortAndFuelAlly_Gillnetting0_15m.RData", sep=""))) 
     save(aggEffortAndFuelAlly,file=file.path(outPath,  paste("AggregatedEffortAndFuelAlly_Gillnetting.RData", sep=""))) 
    
   
   
   
   
     ###----------------------
     ## do a ggplot 
     load(file=file.path(outPath,  paste("AggregatedEffortAndFuelAlly_Gillnetting.RData"))) # aggEffortAlly
    
    
    library(ggplot2)
    some_color_vessel_size <- c("[15,18)"="#FFDB6D",  "[18,24)"="#FC4E07",  "[24,40)"="#52854C",  "[40,100)"="#293352")

     p <-  ggplot() + geom_bar(data=aggEffortAndFuelAlly, aes(x=as.character(Year), y=effective_effort_mins/60, group=VesselSize, fill=VesselSize), size=1.5, position="stack",  stat = "summary", fun = "sum") +
       #facet_wrap(. ~ LE_MET, scales = "free_y")  + 
       theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + 
       labs(y = "", x = "Year")     + 
       #   geom_point(aes(color=VesselSize), size=3)   + 
       scale_fill_manual(values=some_color_vessel_size) +
       guides(fill =guide_legend(ncol=1)) 
     print(p)

    library(ggplot2)
    some_color_vessel_size <- c("[15,18)"="#FFDB6D",  "[18,24)"="#FC4E07",  "[24,40)"="#52854C",  "[40,100)"="#293352")
    dd <- aggEffortAndFuelAlly[!duplicated(data.frame(aggEffortAndFuelAlly$VE_REF, aggEffortAndFuelAlly$Year)),]
    dd$nbvessel <- 1 
    a_ylab <- "Nb Vessels"
    p2 <- ggplot() +
     geom_line(data=dd, aes(x=as.character(Year), y=nbvessel, group=VesselSize, color=VesselSize),size=1.5, stat = "summary", fun = "sum") +   
       #facet_wrap(. ~ LE_MET, scales = "free_y")  + 
       theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + 
       labs(y = a_ylab, x = "Year")     + 
       #   geom_point(aes(color=VesselSize), size=3)   + 
       scale_color_manual(values=some_color_vessel_size, name="VesselSize") +  
       guides(fill =guide_legend(ncol=1)) 
 print(p2)

 
    library(ggplot2)
    some_color_vessel_size <- c("[15,18)"="#FFDB6D",  "[18,24)"="#FC4E07",  "[24,40)"="#52854C",  "[40,100)"="#293352")
    dd <- aggEffortAndFuelAlly[!duplicated(data.frame(aggEffortAndFuelAlly$VE_REF, aggEffortAndFuelAlly$Year)),]
    dd$nbvessel <- 1 
    a_ylab <- "Fuel use"
    p3 <- ggplot() +
     geom_line(data=dd, aes(x=as.character(Year), y=litre_fuel/1e6, group=VesselSize, color=VesselSize),size=1.5, stat = "summary", fun = "sum") +   
       #facet_wrap(. ~ LE_MET, scales = "free_y")  + 
       theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + 
       labs(y = a_ylab, x = "Year")     + 
       #   geom_point(aes(color=VesselSize), size=3)   + 
       scale_color_manual(values=some_color_vessel_size, name="VesselSize") +  
       guides(fill =guide_legend(ncol=1)) 
 print(p3)

 
    library(ggplot2)
    some_color_vessel_size <- c("[15,18)"="#FFDB6D",  "[18,24)"="#FC4E07",  "[24,40)"="#52854C",  "[40,100)"="#293352")
    dd <- aggEffortAndFuelAlly[!duplicated(data.frame(aggEffortAndFuelAlly$VE_REF, aggEffortAndFuelAlly$Year)),]
    dd$nbvessel <- 1 
    a_ylab <- "Income from landings (euros)"
    p3 <- ggplot() +
     geom_line(data=dd, aes(x=as.character(Year), y=toteuros/1e6, group=VesselSize, color=VesselSize),size=1.5, stat = "summary", fun = "sum") +   
       #facet_wrap(. ~ LE_MET, scales = "free_y")  + 
       theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + 
       labs(y = a_ylab, x = "Year")     + 
       #   geom_point(aes(color=VesselSize), size=3)   + 
       scale_color_manual(values=some_color_vessel_size, name="VesselSize") +  
       guides(fill =guide_legend(ncol=1)) 
 print(p3)



 # a trick to combine both info on the same plot i.e. use a secondary y axis
    some_color_vessel_size <- c("[15,18)"="#FFDB6D",  "[18,24)"="#FC4E07",  "[24,40)"="#52854C",  "[40,100)"="#293352")
    some_color_vessel_size2 <- c("[15,18)"="#ffc207",  "[18,24)"="#FC4E07",  "[24,40)"="#52854C",  "[40,100)"="#293352")
      dd <- aggEffortAndFuelAlly[!duplicated(data.frame(aggEffortAndFuelAlly$VE_REF, aggEffortAndFuelAlly$Year)),]
      dd$nbvessel <- 2e4 
    p4 <-   ggplot() + geom_bar(data=aggEffortAndFuelAlly, aes(x=as.character(Year), y=effective_effort_mins/60, group=VesselSize, fill=VesselSize), size=1.5, position="stack",  stat = "summary", fun = "sum") +
       geom_line(data=dd, aes(x=as.character(Year), y=nbvessel, group=VesselSize, color=VesselSize),size=1.5, stat = "summary", fun = "sum") +   
       geom_line(data=dd, aes(x=as.character(Year), y=litre_fuel/5, group=1),size=1,  color=1, linetype = "dashed", stat = "summary", fun = "sum") +   
       geom_line(data=dd, aes(x=as.character(Year), y=toteuros/100, group=1),size=1,  color=2, linetype = "dashed", stat = "summary", fun = "sum") +   
        scale_y_continuous(name = "Effective effort hours; or fuel use (litre/5); or keuros", sec.axis = sec_axis(~./2e4, name = "Nb Vessels") )+
       theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + 
       labs(x = "Year")     + 
       scale_color_manual(values=some_color_vessel_size, name="VesselSize") +  
       scale_fill_manual(values=some_color_vessel_size2) +
       guides(fill =guide_legend(ncol=1)) 
    print(p4)


# dem GNS
#a_width <- 3000; a_height <- 2300
a_width <- 5500; a_height <- 2500   
 namefile <- paste0("barplot_and_ts_effort_nb_vessels_", years[1], "-", years[length(years)], "_DEM.tif")
 tiff(filename=file.path(getwd(), "outputs2020_gns", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
print(p4)
dev.off()

       