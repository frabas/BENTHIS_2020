
#-------------------------------------------------------------------------------
#
# Initially named "the Benthis WP2 workflow"
# Designed by: Francois Bastardie, Niels Hintzen
#
# Updated by F. Bastardie to feed the EMFF Danish sandbank project
# Runs with: R version 3.6.3
# and VMStools version: 0.71
#
#-------------------------------------------------------------------------------

rm(list=ls())
library(vmstools)
library(maps)
library(mapdata)

if(.Platform$OS.type == "unix") {
 codePath  <- file.path("/zhome","fe","8","43283","BENTHIS")
 dataPath  <- file.path("/zhome","fe","8","43283","BENTHIS","EflaloAndTacsat")
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
    a_year <- 2019
  }else{
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
  }
 }
 
if(.Platform$OS.type == "windows") {
 codePath  <- "D:/FBA/BENTHIS_2020/"
 dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
 outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_gns")
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

for (a_year in c(2005:2019))    {  # on WINDOWS system...


overwrite    <- TRUE
dir.create(file.path(outPath))
dir.create(file.path(outPath, a_year))

library(vmstools)
  

#if(TRUE){

  # DO IT ONCE FOR ALL IN ORDER TO SAVE TIME LOADING BIG DATA:
  #Caution: Because unfortunately the RData format has been impacted on 
  # the Unix side after 3.5.0 we need to save the RData with version=2 to ensure back compatibility
  #assign (paste0("tacsat_", a_year),
  #        read.table(file.path(dataPath,paste("tacsat2_", a_year,".csv", sep='')), sep=",", header=TRUE)) # get the tacsat object
  #save(list=paste0("tacsat_", a_year), file=file.path(dataPath,paste("tacsat_", a_year,".RData", sep='')), version=2)
  #assign (paste0("eflalo_", a_year),
  #       read.table(file.path(dataPath,paste("eflalo4_", a_year,".csv", sep='')), sep=",", header=TRUE)) # get the eflalo object
  #save(list=paste0("eflalo_", a_year), file=file.path(dataPath,paste("eflalo_", a_year,".RData", sep='')), version=2)
  
  print(file.path(dataPath,paste("eflalo_", a_year,".RData", sep='')))

  load(file.path(dataPath,paste("eflalo_", a_year,".RData", sep=''))); # get the eflalo object
  load(file.path(dataPath,paste("tacsat_", a_year,".RData", sep=''))); # get the tacsat object
  if(a_year>=2016){
    tacsat <- formatTacsat(get(paste0("tacsat_", a_year))) # format each of the columns to the specified class
    eflalo <- formatEflalo(get(paste0("eflalo_", a_year))) # format each of the columns to the specified class
    } else{
    tacsat <- formatTacsat(get(paste0("tacsat"))) # format each of the columns to the specified class
    eflalo <- formatEflalo(get(paste0("eflalo"))) # format each of the columns to the specified class
    }

  # drop the species catch and euro composition
  idx                   <- kgeur(colnames(eflalo))
  eflalo$LE_KG_SPECS    <- rowSums(eflalo[,grep("LE_KG_",colnames(eflalo))],na.rm=T)
  eflalo$LE_EURO_SPECS  <- rowSums(eflalo[,grep("LE_EURO_",colnames(eflalo))],na.rm=T)
  eflalo                <- eflalo[,-idx]

  # country-specific
  ctry   <- "DNK"
  eflalo <- eflalo[ grep(ctry, as.character(eflalo$VE_REF)),]  # keep the national vessels only.
  #VMS_ping_rate_in_hour <- 115/60 # Dutch data (rev(sort(table(intervalTacsat(sortTacsat(tacsat),level="vessel")$INTV))[1])
  VMS_ping_rate_in_hour <- 1 # e.g. 1 hour ping for Denmark (rev(sort(table(intervalTacsat(sortTacsat(tacsat),level="vessel")$INTV))[1])
  
  # Gear codes to keep ()
  gears2keep            <- c("GNS")
  netGears            <- c("GNS")

  if(.Platform$OS.type == "windows")
    data(euharbours)
  if(.Platform$OS.type == "unix")
    data(harbours)
  data(ICESareas)
  data(europa)

   

 # check if the files are not already existing. If yes, avoid rebuilding them because too time consuming
 do_it <- TRUE
 fls <- dir(file.path(outPath, a_year))
 fls <- fls[grep("tacsatMerged.RData", fls)]
 if(length(fls)!=0  && overwrite==FALSE) do_it <- FALSE


if(do_it){
  
  if(TRUE){
  #-----------------------------------------------------------------------------
  # Cleaning tacsat (keep track of removed records)
  #-----------------------------------------------------------------------------
  remrecsTacsat      <- matrix(NA,nrow=6,ncol=2,dimnames= list(c("total","duplicates","notPossible",
                                                                 "pseudoDuplicates","harbour","land"),
                                                               c("rows","percentage")))
  remrecsTacsat["total",] <- c(nrow(tacsat),"100%")

  # Remove duplicate records
  tacsat$SI_DATIM     <- as.POSIXct(paste(tacsat$SI_DATE,  tacsat$SI_TIME,   sep=" "),
                                   tz="GMT", format="%d/%m/%Y  %H:%M")
  uniqueTacsat        <- paste(tacsat$VE_REF,tacsat$SI_LATI,tacsat$SI_LONG,tacsat$SI_DATIM)
  tacsat              <- tacsat[!duplicated(uniqueTacsat),]
  remrecsTacsat["duplicates",] <- c(nrow(tacsat),100+round((nrow(tacsat) -
                                   an(remrecsTacsat["total",1]))/an(remrecsTacsat["total",1])*100,2))

  # Remove points that cannot be possible
  spThres             <- 20   #Maximum speed threshold in analyses in nm
  idx                 <- which(abs(tacsat$SI_LATI) > 90 | abs(tacsat$SI_LONG) > 180)
  idx                 <- unique(c(idx,which(tacsat$SI_HE < 0 | tacsat$SI_HE > 360)))
  idx                 <- unique(c(idx,which(tacsat$SI_SP > spThres)))
  if(length(idx)>0)
    tacsat            <- tacsat[-idx,]
  remrecsTacsat["notPossible",] <- c(nrow(tacsat),100+round((nrow(tacsat) -
                                     an(remrecsTacsat["total",1]))/an(remrecsTacsat["total",1])*100,2))

  # Remove points which are pseudo duplicates as they have an interval rate < x minutes
  intThres            <- 5    # Minimum difference in time interval in minutes to prevent pseudo duplicates
  tacsat              <- sortTacsat(tacsat)
  tacsatp             <- intervalTacsat(tacsat,level="vessel",fill.na=T)
  tacsat              <- tacsatp[which(tacsatp$INTV > intThres | is.na(tacsatp$INTV)==T),-grep("INTV",colnames(tacsatp))]
  remrecsTacsat["pseudoDuplicates",] <- c(nrow(tacsat),100+round((nrow(tacsat) -
                                          an(remrecsTacsat["total",1]))/an(remrecsTacsat["total",1])*100,2))

  # Remove points in harbour 
  idx             <- pointInHarbour(tacsat$SI_LONG,tacsat$SI_LATI,harbours)
  pih             <- tacsat[which(idx == 1),]
  save(pih,file=paste(outPath, a_year, "pointInHarbour.RData",sep=""))
  tacsat          <- tacsat[which(idx == 0),]
  remrecsTacsat["harbour",] <- c(nrow(tacsat),100+round((nrow(tacsat) -
                  an(remrecsTacsat["total",1]))/an(remrecsTacsat["total",1])*100,2))

  # Remove points on land
  pols                <- lonLat2SpatialPolygons(lst=lapply(as.list(sort(unique(europa$SID))),
                                                function(x){data.frame(SI_LONG=subset(europa,SID==x)$X,
                                                                       SI_LATI=subset(europa,SID==x)$Y)}))
  idx                 <- pointOnLand(tacsat,pols)
  pol                 <- tacsat[which(idx == 1),]
  save(pol,file=file.path(outPath,a_year,"pointOnLand.RData"))
  tacsat              <- tacsat[which(idx == 0),]
  remrecsTacsat["land",] <- c(nrow(tacsat),100+round((nrow(tacsat) -
                an(remrecsTacsat["total",1]))/an(remrecsTacsat["total",1])*100,2))

  # Save the remrecsTacsat file
  save(remrecsTacsat,file=file.path(outPath,a_year,"remrecsTacsat.RData"))

  # remove (otherwise we will see interpolation from 0s!)
  tacsat <- tacsat[tacsat$SI_LATI!=0,] 

  save(tacsat,file=file.path(outPath,a_year,"cleanTacsat.RData"))
  }
  load(file=file.path(outPath,a_year,"cleanTacsat.RData"))
  
  #-----------------------------------------------------------------------------
  # Cleaning Eflalo
  #-----------------------------------------------------------------------------

  # Keep track of removed points
  remrecsEflalo     <- matrix(NA,nrow=5,ncol=2,dimnames=list(c("total","duplicated","impossible time",
                                                               "before 1st Jan","departArrival"),
                                                             c("rows","percentage")))
  remrecsEflalo["total",] <- c(nrow(eflalo),"100%")

  # Remove non-unique trip numbers
  eflalo            <- eflalo[!duplicated(paste(eflalo$LE_ID,eflalo$LE_CDAT,sep="-")),]
  remrecsEflalo["duplicated",] <- c(nrow(eflalo),100+round((nrow(eflalo) -
                                    an(remrecsEflalo["total",1]))/an(remrecsEflalo["total",1])*100,2))

  # Remove impossible time stamp records
  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")

  eflalo            <- eflalo[!(is.na(eflalo$FT_DDATIM) |is.na(eflalo$FT_LDATIM)),]
  remrecsEflalo["impossible time",] <- c(nrow(eflalo),100+round((nrow(eflalo) -
                  an(remrecsEflalo["total",1]))/an(remrecsEflalo["total",1])*100,2))

  # Remove trip starting before 1st Jan
  # year              <- min(year(eflalo$FT_DDATIM))  # deprecated?
  eflalo            <- eflalo[eflalo$FT_DDATIM>=strptime(paste(a_year,"-01-01 00:00:00",sep=''),
                                                             "%Y-%m-%d %H:%M"),]
  remrecsEflalo["before 1st Jan",] <- c(nrow(eflalo),100+round((nrow(eflalo) -
                  an(remrecsEflalo["total",1]))/an(remrecsEflalo["total",1])*100,2))

  # Remove records with arrival date before departure date
  eflalop           <- eflalo
  eflalop$FT_DDATIM <- as.POSIXct(paste(eflalo$FT_DDAT,  eflalo$FT_DTIME,   sep=" "),
                                tz="GMT", format="%d/%m/%Y  %H:%M")
  eflalop$FT_LDATIM <- as.POSIXct(paste(eflalo$FT_LDAT,  eflalo$FT_LTIME,   sep=" "),
                                tz="GMT", format="%d/%m/%Y  %H:%M")
  idx               <- which(eflalop$FT_LDATIM >= eflalop$FT_DDATIM)
  eflalo            <- eflalo[idx,]
  remrecsEflalo["departArrival",] <- c(nrow(eflalo),100+round((nrow(eflalo) -
                  an(remrecsEflalo["total",1]))/an(remrecsEflalo["total",1])*100,2))

  # Save the remrecsEflalo file
  save(remrecsEflalo,file=file.path(outPath,a_year,"remrecsEflalo.RData"))

  # Save the cleaned eflalo file
  save(eflalo,file=file.path(outPath,a_year,"cleanEflalo.RData"))

  #-----------------------------------------------------------------------------
  # Make gear code selection and calculate effort for each gear
  #-----------------------------------------------------------------------------
  load(file=file.path(outPath,a_year,"cleanEflalo.RData"))
  
 
  # effort < 15m vs >15m
  eflalo$length_class   <- cut(as.numeric(as.character(eflalo$VE_LEN)), breaks=c(0,15,100))   # DCF but VMS!

  # compute effort
  eflalo                <- subset(eflalo,FT_REF != 0)
  eflalo                <- orderBy(~VE_REF+FT_DDATIM+FT_REF, data=eflalo)
  eflalo$ID             <- paste(eflalo$VE_REF,eflalo$FT_REF,sep="")
  eflalo$LE_EFF         <- an(difftime(eflalo$FT_LDATIM, eflalo$FT_DDATIM, units="hours"))
  eflalo$dummy          <- 1
  eflalo$LE_EFF         <- eflalo$LE_EFF / merge(eflalo,aggregate(eflalo$dummy,by=list(eflalo$ID),FUN=sum),by.x="ID",by.y="Group.1",all.x=T)$x
  eflalo                <- eflalo[which(eflalo$LE_GEAR %in% gears2keep),] 
  aggregate(eflalo$LE_EFF, list(eflalo$length_class), sum,na.rm=T)

  gc(reset=TRUE)



  #-----------------------------------------------------------------------------
  # Merge eflalo and tacsat
  #-----------------------------------------------------------------------------

  tacsatp               <- mergeEflalo2Tacsat(eflalo,tacsat)

  tacsatp$LE_GEAR       <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$VE_LEN        <- eflalo$VE_LEN[ match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$LE_MET        <- eflalo$LE_MET[ match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$VE_KW         <- eflalo$VE_KW[ match(tacsatp$FT_REF,eflalo$FT_REF)]
  if("LE_WIDTH" %in% colnames(eflalo))
    tacsatp$LE_WIDTH    <- eflalo$LE_WIDTH[ match(tacsatp$FT_REF,eflalo$FT_REF)]
  save(tacsatp,file=file.path(outPath,a_year,"tacsatMerged.RData"))

  # Save not merged tacsat data
  tacsatpmin            <- subset(tacsatp,FT_REF == 0)
  save(tacsatpmin, file=file.path(outPath,a_year,"tacsatNotMerged.RData"))

  
} # end do_it

load(file=file.path(outPath,a_year,"tacsatMerged.RData"))
  
  

  #-----------------------------------------------------------------------------
  # transform into WP2 BENTHIS metier - HANDMADE WORK NEEDED: ADAPT TO YOUR OWN METIER LIST!!!
  #-----------------------------------------------------------------------------
  tacsatp               <- subset(tacsatp,FT_REF != 0)
   ctry <- "DNK"
   tacsatp$LE_MET_init    <- tacsatp$LE_MET
   tacsatp$LE_MET         <- factor(tacsatp$LE_MET)            
   print(levels(tacsatp$LE_MET))
  

  
  #-----------------------------------------------------------------------------
  # Define activity
  #-----------------------------------------------------------------------------
  save(tacsatp,file=file.path(outPath,a_year,"tacsatMergedAndMet.RData"))
  load(file=file.path(outPath,a_year,"tacsatMergedAndMet.RData"))
  
  
  idx               <- which(is.na(tacsatp$VE_REF) == T   | is.na(tacsatp$SI_LONG) == T | is.na(tacsatp$SI_LATI) == T |
                             is.na(tacsatp$SI_DATIM) == T |  is.na(tacsatp$SI_SP) == T)
  if(length(idx)>0) tacsatp         <- tacsatp[-idx,]

  if(.Platform$OS.type == "windows" && FALSE) {
    storeScheme       <- activityTacsatAnalyse(tacsatp, units = "year", analyse.by = "LE_GEAR",identify="means")
    storeScheme       <- storeScheme[which(is.na(storeScheme$analyse.by)==F),]

    storeScheme$years <- as.numeric(as.character(storeScheme$years))
    storeScheme       <- storeScheme[storeScheme$years==a_year,]
    save(storeScheme, file=file.path(outPath,a_year,"storeScheme.RData"))
  }  else{  # actually, storeScheme copied/pasted from a year to the next because assumed constant. So do the copy/paste before running those lines
    load(file.path(outPath,a_year,"storeScheme.RData"))
    storeScheme$years <- a_year  
  }

  tacsatp$year      <- format(tacsatp$SI_DATIM, "%Y")
  require(mixtools)
  activity          <- activityTacsat(tacsatp,units="year",analyse.by="LE_GEAR", storeScheme,
                              plot=FALSE, level="all")
  tacsatp$SI_STATE  <- NA
  tacsatp$SI_STATE  <- activity

  #- Plot the result
  if(FALSE){
    result          <- table(tacsatp$SI_STATE,tacsatp$SI_SP,tacsatp$LE_GEAR)
    par(mfrow=rep(ceiling(sqrt(length(unique(tacsatp$LE_GEAR)))),2))
    for(i in 1:dim(result)[3])
      barplot(result[,,i],col=1:3)
  }

  # General speed rules for remaining records
  idx               <- which(is.na(tacsatp$SI_STATE))
  if(length(idx)>0){
    tacsatp$SI_STATE[idx[which(tacsatp$SI_SP[idx] >= 1.5 &
                               tacsatp$SI_SP[idx] <= 7.5)]] <- 'f'
    tacsatp$SI_STATE[idx[which(tacsatp$SI_SP[idx] <  1.5)]] <- 'h'
    tacsatp$SI_STATE[idx[which(tacsatp$SI_SP[idx] >  7.5)]] <- 's'
  }
  save(tacsatp,     file=file.path(outPath,a_year,"tacsatActivity.RData"))

  # Labelling each haul (caution: to do before discarding the steaming points...)
  tacsatp   <- labellingHauls(tacsatp)

  # fuel use
  table.fuelcons.per.engine       <-  read.table(file= file.path(dataPath, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
  linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
  max_vessel_speed                <- tapply(as.numeric(as.character(tacsatp$SI_SP)), tacsatp$VE_REF, quantile, 0.95)
  tacsatp$max_vessel_speed        <- max_vessel_speed[tacsatp$VE_REF]
  tacsatp$max_consumed            <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(tacsatp$VE_KW))))
  fuel_per_h                      <- function (a,x) a*(x^3)  # cubic law
  tacsatp$a                       <- tacsatp$max_consumed/ (tacsatp$max_vessel_speed^3) # scaling factor
  tacsatp$LITRE_FUEL              <- fuel_per_h(tacsatp$a, tacsatp$SI_SP)* round(VMS_ping_rate_in_hour*1.0)
  # get info to later dispatch steaming cons on fishing points since we will be removing steaming when interpolating
  tacsatps <- tacsatp[tacsatp$SI_STATE=="s",] 
  steaming_cons_per_VE_REF_FT_REF       <-     tapply(tacsatps$LITRE_FUEL, paste0(tacsatps$VE_REF,"_",tacsatps$FT_REF), sum)


  #-----------------------------------------------------------------------------
  # Interpolation (of fishing sequences only)
  #-----------------------------------------------------------------------------
  dir.create(file.path(outPath,a_year,"interpolated"))
  tacsatp           <- orderBy(~VE_REF+SI_DATIM,data=tacsatp)

   tacsatp$SI_STATE  <- "f"
  

   fls <- dir(file.path(outPath, a_year,"interpolated"))
   fls <- fls[grep("tacsatSweptArea_", fls)]


   # NO INTERPOLATION FOR PASSIVE GEARS....

  # per gear per vessel
  for(iGr in netGears){ # no interpolation
    tacsatpGear        <- tacsatp[!is.na(tacsatp$LE_GEAR) & tacsatp$LE_GEAR==iGr,]
  
    for(iVE_REF in sort(unique(tacsatpGear$VE_REF))){
      cat(paste(iGr, " ", iVE_REF, "\n"))
      tacsatpGearVEREF <- tacsatpGear[tacsatpGear$VE_REF %in% iVE_REF,]
      tacsatpGearVEREF <- tacsatpGearVEREF[tacsatpGearVEREF$SI_STATE=='f',] # keep fishing pings only
  
      # fuel use
      tacsatpGearVEREF$LITRE_FUEL_FISHING      <- fuel_per_h(as.numeric(as.character(tacsatpGearVEREF$a)), as.numeric(as.character(tacsatpGearVEREF$SI_SP)))* VMS_ping_rate_in_hour*1.0
      tacsatpGearVEREF$VE_REF_FT_REF           <-  paste0(tacsatpGearVEREF$VE_REF,"_",tacsatpGearVEREF$FT_REF) 
      nb_fishing_pts_per_VE_REF_FT_REF           <-  table(tacsatpGearVEREF$VE_REF_FT_REF) # for dispatching evenly on fishing pts
      tacsatpGearVEREF$FUEL_LITRE_STEAMING     <- steaming_cons_per_VE_REF_FT_REF[tacsatpGearVEREF$VE_REF_FT_REF]  /  table(tacsatpGearVEREF$VE_REF_FT_REF)[tacsatpGearVEREF$VE_REF_FT_REF]  
      tacsatpGearVEREF$LE_KG_LITRE_FUEL        <- tacsatpGearVEREF$LITRE_FUEL_FISHING + tacsatpGearVEREF$FUEL_LITRE_STEAMING 
      tacsatpGearVEREF <- tacsatpGearVEREF[, !colnames(tacsatpGearVEREF) %in% c("VE_REF_FT_REF", "max_vessel_speed", "max_consumed", "a", "FUEL_LITRE_STEAMING")] # cleaning
   
   
      tacsatIntGearVEREF <- tacsatpGearVEREF
  
      save(tacsatIntGearVEREF, file=file.path(outPath,a_year,"interpolated",
                                              paste("tacsatSweptArea_",iVE_REF, "_", iGr, ".RData", sep="")))
    }
  }

#} # end TRUE/FALSE

cat(paste("All interpolations done", "\n"))
 

 #-----------------------------------------------------------------------------
 # compute (discrete point) effort_days and effort_KWdays
 #-----------------------------------------------------------------------------

  if(FALSE){
  library(doBy)
  tacsatp                  <- orderBy(~VE_REF+SI_DATIM+FT_REF,data=tacsatp)
  tacsatp$effort_days      <- as.numeric(as.character(difftime(c(tacsatp$SI_DATIM[-1],0),tacsatp$SI_DATIM,units="days")))
  tacsatp$effort_KWdays    <- tacsatp$effort_days *  as.numeric(as.character(tacsatp$VE_KW))
  tacsatp$effort_days[tacsatp$effort_days>0.014] <- 0  # correct (i.e. set at 0 if >3hours as a sign for a change of trip)
  }


#-----------------------------------------------------------------------------
# Create one swept area dataset
#-----------------------------------------------------------------------------

#for(a_year in c(2005:2013)) {
#print(a_year)

fls <- dir(file.path(outPath, a_year,"interpolated"))
fls <- fls[grep("tacsatSweptArea_", fls)]

lst <- list(); count <- 0
vid_with_errors <- NA
cols2keep <- c("SI_LATI","SI_LONG","SI_DATE","LE_GEAR","LE_MET","SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER","SWEPT_AREA_KM2_UPPER", "GEAR_WIDTH", "SI_DATIM", "SI_FT", "LE_KG_LITRE_FUEL" )  
for(iFile in fls){
  cat(paste(iFile, "\n"))
  count <- count+1
  load(file.path(outPath,a_year,"interpolated",iFile))


  
  #- Make selection for gears where you already have gear width and which not
  # compute the swept area
  tacsatIntGearVEREF <- cbind.data.frame(tacsatIntGearVEREF,  data.frame('GEAR_WIDTH'=0, 'SWEPT_AREA_KM2'=0, 'SWEPT_AREA_KM2_LOWER'=0, 'SWEPT_AREA_KM2_UPPER'=0)) # no width for passive gears
  
  lst[[count]] <- tacsatIntGearVEREF[,cols2keep]

}
tacsatSweptArea   <- do.call(rbind,lst)

# check NAs (approx. 2% of the records)
nrow(tacsatSweptArea[is.na(tacsatSweptArea$SWEPT_AREA_KM2),])

# save
save(tacsatSweptArea, file=file.path(outPath,a_year, paste("tacsatSweptArea.RData", sep="")),compress=T)

} # end a year

