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


 #years <- 2012:2019
years <- 2005:2019


if(FALSE){


eflalo_res <- NULL


for (a_year in years)    {  # on WINDOWS system...

dir.create(file.path(outPath))
dir.create(file.path(outPath, a_year))

cat(paste("Year", a_year, "\n"))

library(vmstools)


  load(file.path(dataPath,paste("eflalo_", a_year,".RData", sep=''))); # get the eflalo object
  if(a_year>=2016){
    eflalo <- formatEflalo(get(paste0("eflalo_", a_year))) # format each of the columns to the specified class
    } else{
    eflalo <- formatEflalo(get(paste0("eflalo"))) # format each of the columns to the specified class
    }


  


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

  if (a_year==years[1]) {  eflalo_res <- eflalo }

  cols <- unique(colnames(eflalo)[colnames(eflalo) %in% colnames(eflalo_res)],
     colnames(eflalo_res)[ colnames(eflalo_res)  %in% colnames(eflalo)])

 if (a_year>years[1]) {
  eflalo_res <- rbind.data.frame(eflalo_res[,cols], eflalo[,cols])
  }
 } # end y
 
 # keep all vessels with < 12m in length (i.e. most of them without VMS)
  eflalo <- eflalo_res 
  eflalo_small_vids <- eflalo_res[as.numeric(as.character(eflalo$VE_LEN)) <= 11.99,]

 save(eflalo, file=file.path(dataPath ,paste("eflalo_ally_",years[1],"-",years[length(years)],".RData", sep='')))
 save(eflalo_small_vids, file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))

} # end FALSE

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  #load(file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))
  #eflalo <- eflalo_small_vids
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
   
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # fuel comsumption
  
   if(FALSE){
   # ad hoc
  # from there decrease the maximal fuel cons by a conversion factor assuming the engine is not switch on all the time:
  #1.	All trawling and Flyshooting (OTB, OTM, TBB, DRB, SSC, etc.): Engine load/conversion factor of 90% for the duration of the trip (Assumed as an average across shorter or longer periods of steaming, trawling, setting or hauling) 
  #2.	All passive gears (GNS, Pots, lines, etc.): Engine load/conversion factor of 50% for the duration of the trip (Assumed as an average across shorter or longer periods of steaming, setting or hauling)
  #3.	Danish seiners (SDN): Engine load/conversion factor of 67% for the duration of the trip (Assumed as an average across shorter or longer periods of steaming, setting or hauling)
  # TO DO:
  eflalo$convfactor <- 1
  convfactor <- c("OTB"=0.9,"GN"=0.5,"SSC"=0.67,"GTR"=0.5,"PTM"=0.9,"DRB"=0.9,"MIS"=0.5,"GNS"=0.5,"FPN"=0.5,"LL"=0.5,"UNK"=0.5,"SDN"=0.67,"LHP"=0.5,"LLS"=0.5,
                      "GND"=0.5,"PTB"=0.9,"OTM"=0.9,"LLD"=0.5,"FYK"=0.5,"FPO"=0.5,"LH"=0.5,"OTT"=0.9,"TBB"=0.9,"FIX"=0.5,"LX"=0.5,"DRH"=0.5,"OFG"=0.5,"ZZZ"=0.5,
                      "GTN"=0.5,"DRC"=0.5,"LTL"=0.5,"TBN"=0.9,"AHB"=0.5,"BMS"=0.5,"DRO"=0.5,"KLM"=0.5,"BRJ"=0.5)
  eflalo$convfactor <- convfactor[as.character(eflalo$LE_GEAR)]       
        
  
  table.fuelcons.per.engine       <-  read.table(file= file.path(dataPath, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
  linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
  eflalo$LE_KG_LITRE_FUEL         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(eflalo$VE_KW)))) * eflalo$LE_EFF * eflalo$convfactor # Liter per hour * effort this trip in hour
  } # end FALSE
  

  if(FALSE){
  # use AIS data to deduce fuel cons from a typical speed profile (per DCF level6) as we don´t have speed info for small vessels in logbooks
  # (we also dont have AIS data for all small vessels becuase no mandatory) and deduce and apply typical fuel cons per hour (computed from kW and speed) to each trip.
  # this is assuning a mean kW for each segment which might be a rough assumption given the kW is driving very much the fuel consumption rate. 
  # for towed gears, this is also assuming 0.9 of max speed when within a certain interval in speed (i.e. when towing...) (see GetTripFuelConsFromAISdata.R for details)
  load(file=file.path(getwd(), "AIS_data", "fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m.RData"))    # get fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m (but a rough average)
  eflalo$fuel_cons_in_trip_level_6 <-   fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m[ as.character(eflalo$LE_MET) ]
  
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

  } # end FALSE
  
 
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


     
# lgbkonly
  #a_width <- 3000; a_height <- 2300 
a_width <- 4000; a_height <- 2500   
 namefile <- paste0("barplot_and_ts_effort_nb_vessels_", years[1], "-", years[length(years)], "_lgbkonly.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
print(p3)
dev.off()




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

   dd <- tapply(eflalo$LE_EFF, paste0(eflalo$target, "_", eflalo$F_SUBAREA, "_", eflalo$LE_MET, "_", eflalo$VesselSize), sum)


   pel <- dd[grep("SmallMesh",names(dd))]
   pel <- pel[order(pel, decreasing=TRUE)]
   oth_mets_pel <-  names(pel)[cumsum(pel)/sum(pel)>.75]
   oth_mets_pel <- c(oth_mets_pel, "NA_27.3_No_Matrix6_(0,12]", "NA_27.4_No_Matrix6_(0,12]")

   dem <- dd[grep("LargeMesh",names(dd))]
   dem <- dem[order(dem, decreasing=TRUE)]
   oth_mets_dem <-  names(dem)[cumsum(dem)/sum(dem)>.75]
   oth_mets_dem <- c(oth_mets_dem, "NA_27.3_No_Matrix6_(0,12]", "NA_27.4_No_Matrix6_(0,12]")

  eflalo$LE_MET_init <- eflalo$LE_MET
  eflalo$LE_MET <-  factor(paste0(eflalo$target, "_", eflalo$F_SUBAREA, "_", eflalo$LE_MET, "_", eflalo$VesselSize))
  levels(eflalo$LE_MET)[levels(eflalo$LE_MET) %in% oth_mets_pel] <- "SmallMesh_OTHER_0_0_0"
  levels(eflalo$LE_MET)[levels(eflalo$LE_MET) %in% oth_mets_dem] <- "LargeMesh_OTHER_0_0_0"

 }


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



  # capture an export for quickmap2020.r
  if(per_metier_level6 && !per_vessel_size){
    save(eflalo, file=file.path(getwd(), "outputs2020_lgbkonly", paste("AggregatedEflaloWithSmallVids",years[1],"-",years[length(years)],".RData", sep="")))
  }
  
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

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
 ## QUICK & EASY TABLES
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
  # capture an export for later doing some quick table
  aggResultPerMetAlly <- aggResultPerMet
  #save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep=""))) 
  #save(aggResultPerMetAlly, file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep=""))) 


   if(per_metier_level6 && per_vessel_size){
      load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  
  
      # add a fuel efficiency metric
      aggResultPerMetAlly$LPUE <- aggResultPerMetAlly$LE_KG_LITRE_FUEL / (aggResultPerMetAlly$LE_EFF)
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
         facet_wrap(. ~ LE_MESH_GROUP, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
         labs(y = "Fuel efficiency (Litre fuel per hour)", x = "Year")   +
         scale_colour_manual(values=some_color_seg, name="Fleet-segment") +   guides(fill =guide_legend(ncol=1))  +
          xlab("")
      print(a_lpue_plot)
      # for paper:
      a_width <- 6000;  a_height <- 4000
      namefile <- paste0("ts_LPUE_", years[1], "-", years[length(years)],  "_PEL_DEM.tif")
      tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
      library(ggpubr)
      ggarrange(a_lpue_plot, ncol=1, common.legend = TRUE, legend="right")

      dev.off()

   }
   if(per_metier_level6 && !per_vessel_size){
      load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
   }
    
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
  VarThisStk <- sweep(PercentThisStk[,colnames(PercentThisStk)]/100, 1, aggResultPerMetAlly[,"LE_EFF"], FUN="*")
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
  sum_y_hoursatsea <- aggregate(aggResultPerMetAlly[, grepl("LE_EFFORT_", nm)], list(Year=aggResultPerMetAlly$Year), sum, na.rm=TRUE)  # annual average
  nm <- colnames(sum_y_hoursatsea)
  average_y_hoursatsea <- apply(sum_y_hoursatsea[, grepl("LE_EFFORT_", nm)], 2, mean)  # annual average
  round(average_y_hoursatsea[order(average_y_hoursatsea, decreasing=TRUE)]/1e3,2)
  info4 <-  round(average_y_hoursatsea[order(average_y_hoursatsea, decreasing=TRUE)]/1e3,3) # thousands of hours at sea

 
  spp <- sapply(strsplit(as.character(names(info3)), split="_"), function(x) x[3])  # give the order on the plot
  spp <- spp[spp!="FUEL"]
  a_summary <- rbind.data.frame(info1[paste0("LE_KG_", spp)], info2[paste0("LE_EURO_", spp)], info3[paste0("LE_LITRE_", spp)], info4[paste0("LE_EFFORT_", spp)] )
  colnames(a_summary) <- spp
  rownames(a_summary) <- c("Thousands tons", "Millions euros", "Millions litres", "Thousands hours at sea")
  a_summary
   #=> supplementary data

  # dem
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
    
   
  a_width <- 2000;  a_height <- 5500
  namefile <- paste0("barplot_fuel_efficiency_smallvids_per_species",years[1],"-",years[length(years)],".tif")
  tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
    library(ggplot2)
    p <- ggplot(data=long[!long$Species %in% c("TODO"),], aes(x=Species, y=value))  +
            geom_boxplot(outlier.size = -1, fill='#A4A4A4', color="black") +  scale_color_grey() + 
            facet_wrap(~VarType, scales="free", ncol=1, labeller=as_labeller(var_names),  strip.position = "left") +  
            labs(y = "", x = "Species") + 
           theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12), strip.background = element_blank(),strip.placement = "outside")
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

 variables <- c("LE_KG_LITRE_FUEL", "CPUEallsp", "CPUFallsp", "VPUFallsp",  "KKGallsp", "KEUROallsp")
 prefixes  <- c("LE_KG_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_",   "LE_KG_", "LE_EURO_")
 
 count <- 0
 for(a_variable in variables){
    count <- count+1

    dd <- get(paste0("aggResultPerMetAlly"))
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


  # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  if(a_variable %in% c("CPUEallsp", "CPUFallsp", "VPUFallsp")){
     nm <- colnames(aggResultPerMetAlly)
     tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
     tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
     tot[,-1] <- round(tot[,-1]) # kg
     head(tot, 5)
     colnames(tot)[1] <- "LE_MET" 
     a_long_for_filter <- melt(setDT(tot[,c("LE_MET", paste0("LE_KG_", spp))]), id.vars = c("LE_MET"), variable.name = "Var2", value.name="value2")
     a_long_for_filter$Var2 <- gsub("LE_KG_", "", a_long_for_filter$Var2)
     long <- merge(long, a_long_for_filter, by.x=c("LE_MET", "Stock"), by.y=c("LE_MET", "Var2"))
     long <- long[long$value2>500,] # here the actual filtering....i.e. keep only seg and value when total catch kg is > threshold in kg
     long <- as.data.frame(long)
     long <- long[,c("LE_MET","Stock", a_variable, "Year", "value")]
  }
 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 library(ggplot2)
 if(a_variable=="LE_KG_LITRE_FUEL") {a_ylab <- "Fuel use (litre)"; ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="CPUEallsp") {a_ylab <- "CPUE (kg per effort)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="CPUFallsp") {a_ylab <- "CPUF (kg per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFallsp") {a_ylab <- "VPUF  (euro per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFSWAallsp") {a_ylab <- "VPUFSWA  (euro per swept area)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="KKGallsp") {a_ylab <- "Landings (tons)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="KEUROallsp") {a_ylab <- "Landings  (keuros)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}

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
       scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  + facet_grid(. ~ Year) + theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
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
       scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  + facet_grid(. ~ Year) + theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
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
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_line(aes(color=Stock), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=Stock), size=3)   +
   scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_line(aes(color=Stock), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=Stock), size=3)   + 
    scale_color_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
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
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value, group=Stock)) +    facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_area(aes( fill=Stock))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
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
     facet_wrap(. ~ LE_MET, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_area(aes(fill=Stock))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=1))  +
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

 variables <- c("LE_KG_LITRE_FUEL", "CPUEallseg", "CPUFallseg", "VPUFallseg", "KKGallseg", "KEUROallseg")
 prefixes  <- c("LE_LITRE_",           "LE_CPUE_",  "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_")


 count <- 0
 for(a_variable in variables){
    count <- count+1

    dd <- get(paste0("aggResultPerMetAlly"))
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

   # assign area to species as a proxy of stock
   long$Stock <- paste(long$Stock, sapply(strsplit(as.character(long$LE_MET), split="_"), function(x) x[2]))

  
  # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  if(a_variable %in% c("CPUEallseg", "CPUFallseg", "VPUFallseg")){
     nm <- colnames(aggResultPerMetAlly)
     tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$LE_MET), mean)  # annual average
     tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
     tot[,-1] <- round(tot[,-1]) # kg
     head(tot, 5)
     colnames(tot)[1] <- "LE_MET" 
     a_long_for_filter <- melt(setDT(tot[,c("LE_MET", paste0("LE_KG_", spp))]), id.vars = c("LE_MET"), variable.name = "Var2", value.name="value2")
     a_long_for_filter$Var2 <- gsub("LE_KG_", prefixes[count], a_long_for_filter$Var2)
     long <- merge(long, a_long_for_filter, by.x=c("LE_MET", "Var"), by.y=c("LE_MET", "Var2"))
     long <- long[long$value2>500,] # here the actual filtering....i.e. keep only seg and value when total catch kg is > threshold in kg
     long <- as.data.frame(long)
     long <- long[,c("LE_MET","Var", "Year", "Stock", "value",  a_variable)]
  }


 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 library(ggplot2)
 if(a_variable=="LE_KG_LITRE_FUEL") {a_unit <- 1e6; a_ylab <- "Fuel use (millions litres)"; ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="CPUEallseg") {a_unit <- 1; a_ylab <- "CPUE (kg per effort)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="CPUEallseg") {a_unit <- 1; a_ylab <- "CPUF (kg per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFallseg") {a_unit <- 1; a_ylab <- "VPUF  (euro per litre)";  ylims=c(0,max(as.data.frame(long)[,a_variable],15))}
 if(a_variable=="VPUFSWAallseg") {a_unit <- 1; a_ylab <- "VPUFSWA  (euro per swept area)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="KKGallseg") {a_unit <- 1e3; a_ylab <- "Landings (tons)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}
 if(a_variable=="KEUROallseg") {a_unit <- 1e3; a_ylab <- "Landings  (keuros)";  ylims=c(0,max(as.data.frame(long)[,a_variable],100000))}

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


 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_line(aes(color=LE_MET), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=LE_MET), size=3)   +
  scale_color_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1))  +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

# pel
 namefile <- paste0("ts_fuel_efficiency_per_stk", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 the_agg <- long[grep("SmallMesh",long$LE_MET),]
  the_agg$LE_MET <- gsub("SmallMesh_", "", the_agg$LE_MET)
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_line(aes(color=LE_MET), size=1.5) +     labs(y = a_ylab, x = "Year")     + geom_point(aes(color=LE_MET), size=3)   +
  scale_color_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1))  +
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
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_area(aes( fill=LE_MET))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1))  +
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
 p <- ggplot(the_agg, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +    facet_wrap(. ~ Stock, scales = "free_y")  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   labs(y = a_ylab) +
  geom_area(aes( fill=LE_MET))  +     labs(y = a_ylab, x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1))  +
  xlab("")     #    + ylim(ylims[1], ylims[2])
 print(p)
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##





} # end a_variable






















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
     dd[grepl("LLS",dd$LE_MET), "met_desc2"] <- "longline for\n"
     dd[grepl("LHP",dd$LE_MET), "met_desc2"] <- "handline for\n"
     dd[grepl("LLD",dd$LE_MET), "met_desc2"] <- "longline for\n"
     dd[grepl("FPN",dd$LE_MET), "met_desc2"] <- "pots for\n"
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
     dd[grepl("CRU_>0_0",dd$LE_MET), "met_desc3"] <- "crustaceans"
     dd[grepl("CRU_>=120_0_0",dd$LE_MET), "met_desc3"] <- "crustaceans (>120mm)"
     dd[grepl("LHP_FIF",dd$LE_MET), "met_desc3"] <- "fish"
     dd[grepl("ANA",dd$LE_MET), "met_desc3"] <- "mig. fish"
     dd[grepl("CAT",dd$LE_MET), "met_desc3"] <- "catadromus sp"
     dd[grepl("MOL",dd$LE_MET), "met_desc3"] <- "molluscs"
     dd[grepl("TBB_CRU_16-31",dd$LE_MET), "met_desc3"] <- "shrimp"
     dd[grepl("LLS_DEF_0_0_0",dd$LE_MET), "met_desc3"] <- "fish"
     dd[grepl("157_0_0",dd$LE_MET), "met_desc3"] <- "fish (>157mm)"
     dd[grepl("110-156_0_0",dd$LE_MET), "met_desc3"] <- "fish (110-156mm)"
     dd$met_desc2[is.na(dd$met_desc2)] <- ""
     dd$met_desc3[is.na(dd$met_desc3)] <- ""

    return(paste(dd$met_desc1, dd$met_desc2, dd$met_desc3))
    }
  

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
#REDO SOME STUFF FOR A STANDALONE CODE:
outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
years <- 2005:2019
load(file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))
eflalo <- eflalo_small_vids
spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                    "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                    "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")
some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                        "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                         "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                         "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                          "WIT"="red", "WHG"="yellow", "OTH"="blue",
                          "COC"="#108291", "OYF"="#6a9110", "LUM"="red", "SAL"="#c2a515", "BLL"="cyan")  # specific to small vids


 
# search in Baltic and North Sea
eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=TRUE)
  if(per_metier_level6 && per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
  if(per_metier_level6 && !per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
library(doBy)
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_", "LE_MPRICE_" )
 the_names <- c("(z)","(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     aggResultPerMetAlly$met_desc <- friendly_met_names(aggResultPerMetAlly)
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     
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
     long <- melt(setDT(dd[,c("met_desc","LE_MET",a_variable, "Year", colnames(VarThisStk))]), id.vars = c("met_desc","LE_MET",a_variable, "Year"), variable.name = "Stock")

    #as.data.frame(long)
    long <- long[complete.cases(long),]
    long$met_desc <- factor(long$met_desc)

  # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
     nm <- colnames(aggResultPerMetAlly)
     tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$met_desc, aggResultPerMetAlly$LE_MET), mean)  # annual average
     tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
     tot[,-c(1:2)] <- round(tot[,-c(1:2)]) # kg
     head(tot, 5)
     colnames(tot)[1:2] <- c("met_desc","LE_MET") 
     a_long_for_filter <- melt(setDT(tot[,c("met_desc","LE_MET", paste0("LE_KG_", spp))]), id.vars = c("met_desc","LE_MET"), variable.name = "Var2", value.name="value2")
     a_long_for_filter$Var2 <- gsub("LE_KG_", "", a_long_for_filter$Var2)
     long <- merge(as.data.frame(long), as.data.frame(a_long_for_filter), by.x=c("met_desc","LE_MET", "Stock"), by.y=c("met_desc","LE_MET", "Var2"))
     long <- long[long$value2>500,] # here the actual filtering....i.e. keep only seg and value when total catch kg is > threshold in kg
     long <- as.data.frame(long)
     long <- long[,c("met_desc","LE_MET","Stock", a_variable, "Year", "value")]
  

    
     long$LE_MET <- paste(long$LE_MET, the_names[count])
     
     colnames(long)[colnames(long)==a_variable] <- "Total"
   
     if(y==years[1]){ agg <- long } else{
      agg <- rbind.data.frame(agg,
             long)
     }
     
   }
    
     the_agg <- rbind.data.frame(the_agg, agg)
 }


  

##!!!!!!!!!!!!!!!!!!!!!!##
## SUMMARY AREAPLOT WITH TIME SERIES OVER THE PERIOD 
##!!!!!!!!!!!!!!!!!!!!!!##
 library(ggplot2)
 a_comment<-""
 a_unit <- 1
 

# DEM
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), Year=levels(factor(the_agg_plot$Year)))
 dd$met_desc <- friendly_met_names (dd)

 
 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---

   a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}


 
the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("LargeMesh_", "", the_agg_plot1$LE_MET)
   the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET) 
 p1_area_bottomfishing_dem_land_smallvids <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +
  geom_area(aes(fill=Stock))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p1)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("LargeMesh_", "", the_agg_plot3$LE_MET)
   the_agg_plot3$LE_MET <- gsub("\\(b)","", the_agg_plot3$LE_MET) 
 p3_area_bottomfishing_dem_cpuf_smallvids <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +
  geom_area(aes(fill=Stock))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p2)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot4$LE_MET <- gsub("LargeMesh_", "", the_agg_plot4$LE_MET)
   the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET) 
 p4_area_bottomfishing_dem_vpuf_smallvids <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +
  geom_area(aes(fill=Stock))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p3)

 
  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
 p5_area_bottomfishing_dem_fpuc_smallvids <- ggplot(the_agg_plot5, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p5)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p6_area_bottomfishing_dem_vpuc_smallvids <- ggplot(the_agg_plot6, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p6)

  ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p8_area_bottomfishing_dem_mprice_smallvids <- ggplot(the_agg_plot8, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ met_desc, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p8)

 
a_width <- 6200 ; a_height <- 8500
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot_land_and_FPUC.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_area_bottomfishing_dem_land_smallvids, p4_area_bottomfishing_dem_vpuf_smallvids, p5_area_bottomfishing_dem_fpuc_smallvids, ncol=3, common.legend = TRUE, legend="bottom")
dev.off()


 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##




 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 
#REDO SOME STUFF FOR A STANDALONE CODE:
outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
years <- 2005:2019
load(file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))
eflalo <- eflalo_small_vids
spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                    "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                    "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")
some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                        "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                         "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                         "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                          "WIT"="red", "WHG"="yellow", "OTH"="blue",
                          "COC"="#108291", "OYF"="#6a9110", "LUM"="red", "SAL"="#c2a515", "BLL"="cyan")  # specific to small vids

# search in Baltic and North Sea
eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=TRUE)
  if(per_metier_level6 && per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
  if(per_metier_level6 && !per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
library(doBy)
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_", "LE_MPRICE_" )
 the_names <- c("(z)","(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     aggResultPerMetAlly$met_desc <- friendly_met_names(aggResultPerMetAlly)
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     
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
     long <- melt(setDT(dd[,c("met_desc","LE_MET",a_variable, "Year", colnames(VarThisStk))]), id.vars = c("met_desc","LE_MET",a_variable, "Year"), variable.name = "Stock")

    #as.data.frame(long)
    long <- long[complete.cases(long),]
    long$met_desc <- factor(long$met_desc)

  # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
     nm <- colnames(aggResultPerMetAlly)
     tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$met_desc, aggResultPerMetAlly$LE_MET), mean)  # annual average
     tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
     tot[,-c(1:2)] <- round(tot[,-c(1:2)]) # kg
     head(tot, 5)
     colnames(tot)[1:2] <- c("met_desc","LE_MET") 
     a_long_for_filter <- melt(setDT(tot[,c("met_desc","LE_MET", paste0("LE_KG_", spp))]), id.vars = c("met_desc","LE_MET"), variable.name = "Var2", value.name="value2")
     a_long_for_filter$Var2 <- gsub("LE_KG_", "", a_long_for_filter$Var2)
     long <- merge(as.data.frame(long), as.data.frame(a_long_for_filter), by.x=c("met_desc","LE_MET", "Stock"), by.y=c("met_desc","LE_MET", "Var2"))
     long <- long[long$value2>500,] # here the actual filtering....i.e. keep only seg and value when total catch kg is > threshold in kg
     long <- as.data.frame(long)
     long <- long[,c("met_desc","LE_MET","Stock", a_variable, "Year", "value")]
  

    
     long$LE_MET <- paste(long$LE_MET, the_names[count])
     
     colnames(long)[colnames(long)==a_variable] <- "Total"
   
     if(y==years[1]){ agg <- long } else{
      agg <- rbind.data.frame(agg,
             long)
     }
     
   }
    
     the_agg <- rbind.data.frame(the_agg, agg)
 }


 library(ggplot2)

# PEL
 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), Year=levels(factor(the_agg_plot$Year)))
 dd$met_desc <- friendly_met_names (dd)


 dd$value <- 0
 dd[,"Total"] <- 0
 dd <- dd[,colnames(the_agg_plot)]
 rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$Year)
 rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---


  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)
  
the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
   the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 p1_area_bottomfishing_pel_land_smallvids <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p1)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
   the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_area_bottomfishing_pel_cpuf_smallvids <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p2)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
   the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_pel_vpuf_smallvids <- ggplot(the_agg_plot4, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p4)

 
  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
    the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p5_area_bottomfishing_pel_fpuc_smallvids <- ggplot(the_agg_plot5, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p5)

   the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
 p6_area_bottomfishing_pel_fpuv_smallvids <- ggplot(the_agg_plot6, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +  
  geom_area(aes(fill=Stock))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p6)
 
   ### ADD-ON mean price
  the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
 p8_area_bottomfishing_pel_mprice_smallvids <- ggplot(the_agg_plot8, aes(x=as.character(Year), y=value/a_unit, group=Stock)) +
     facet_wrap(. ~ LE_MET, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=Stock))  +     labs(y = "Euro catch per kg", x = "Year")   +
   scale_fill_manual(values=some_color_species, name="Species") +   guides(fill =guide_legend(ncol=8))  +
    xlab("")
 #print(p8)



a_width <- 6200 ; a_height <- 8500
namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot_land_and_FPUC.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
 ggarrange(p1_area_bottomfishing_pel_land_smallvids, p4_area_bottomfishing_pel_vpuf_smallvids, p5_area_bottomfishing_pel_fpuc_smallvids, ncol=3, common.legend = TRUE, legend="bottom")
dev.off()

 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##








##!!!!!!!!!!!!!!!!!!!!!!##
## SUMMARY BARPLOT WITH AVERAGE OVER THE PERIOD 
##!!!!!!!!!!!!!!!!!!!!!!##
 
#REDO SOME STUFF FOR A STANDALONE CODE:
outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
years <- 2005:2019
load(file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))
eflalo <- eflalo_small_vids
spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                    "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                    "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")
some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                        "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                         "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                         "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                          "WIT"="red", "WHG"="yellow", "OTH"="blue",
                          "COC"="#108291", "OYF"="#6a9110", "LUM"="red", "SAL"="#c2a515", "BLL"="cyan", "GAR"="grey")  # specific to small vids
 Stocknames <-  c("COD"="cod", "CSH"="brown shrimp", "DAB"="dab", "ELE"="eel", "FLE"="flounder",
                        "HAD"="haddock", "HER"="herring", "HKE"="hake","HOM"="Horse mackerel", "LEM"="lemon sole",
                         "MAC"="mackerel", "MON"="monkfish", "MUS"="mussel", "NEP"="Nephrops", "NOP"="Norway pout", "PLE"="plaice",
                         "POK"="saithe", "PRA"="boreal shrimp", "SAN"="sandeel", "SOL"="sole", "SPR"="sprat", "TUR"="turbot", "WHB"="blue whiting",
                          "WIT"="witch flounder", "WHG"="whiting", "OTH"="other",
                          "COC"="cokle", "OYF"="oyster", "LUM"="lumpfish", "SAL"="salmon", "BLL"="brill", "GAR"="garfish")                         
  some_color_speciesnames <- c("cod"="#E69F00", "brown shrimp"="hotpink", "dab"="#56B4E9", "eel"="#F0E442", "flounder"="green",
                        "haddock"="#0072B2", "herring"="mediumorchid4", "hake"="#CC79A7","Horse mackerel"="indianred2", "lemon sole"="#EEC591",
                         "mackerel"="#458B00", "monkfish"="#F0F8FF", "mussel"="black", "Nephrops"="#e3dcbf", "Norway pout"="#CD5B45", "plaice"="lightseagreen",
                         "saithe"="#6495ED", "boreal shrimp"="#CDC8B1", "sandeel"="#00FFFF", "sole"="#8B0000", "sprat"="#008B8B", "turbot"="#A9A9A9", "blue whiting"="#76a5c4",
                          "witch flounder"="red", "whiting"="yellow", "other"="blue",
                          "cockle"="#108291", "oyster"="#6a9110", "lumpfish"="red", "salmon"="#c2a515", "brill"="cyan", "garfish"="grey")

 per_metier_level6 <- TRUE
 per_vessel_size <- FALSE
 
 # search in Baltic and North Sea
eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=TRUE)
  if(per_metier_level6 && per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
  if(per_metier_level6 && !per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
library(doBy)
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp", "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_","LE_KG_",   "LE_CPUF_",  "LE_VPUF_", "LE_KG_", "LE_EURO_", "LE_MPRICE_" )
 the_names <- c("(z)","(a)","(b)", "(c)", "(d)", "(e)", "(f)", "(h)")

 count <- 0
 the_agg <- NULL
 for(a_variable in variables){
 count <- count+1
 for (y in years){
     #dd <- get(paste0("aggResultPerMet_", y))
     aggResultPerMetAlly$met_desc <- friendly_met_names(aggResultPerMetAlly)
     dd <- aggResultPerMetAlly[aggResultPerMetAlly$Year==y, ]
     
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
     long <- melt(setDT(dd[,c("met_desc","LE_MET",a_variable, "Year", colnames(VarThisStk))]), id.vars = c("met_desc","LE_MET",a_variable, "Year"), variable.name = "Stock")

    #as.data.frame(long)
    long <- long[complete.cases(long),]
    long$met_desc <- factor(long$met_desc)

  # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
     nm <- colnames(aggResultPerMetAlly)
     tot <- aggregate(aggResultPerMetAlly[, grepl("LE_KG_", nm)], list(aggResultPerMetAlly$met_desc, aggResultPerMetAlly$LE_MET), mean)  # annual average
     tot <- orderBy(~ - LE_KG_LITRE_FUEL, tot)
     tot[,-c(1:2)] <- round(tot[,-c(1:2)]) # kg
     head(tot, 5)
     colnames(tot)[1:2] <- c("met_desc","LE_MET") 
     a_long_for_filter <- melt(setDT(tot[,c("met_desc","LE_MET", paste0("LE_KG_", spp))]), id.vars = c("met_desc","LE_MET"), variable.name = "Var2", value.name="value2")
     a_long_for_filter$Var2 <- gsub("LE_KG_", "", a_long_for_filter$Var2)
     long <- merge(as.data.frame(long), as.data.frame(a_long_for_filter), by.x=c("met_desc","LE_MET", "Stock"), by.y=c("met_desc","LE_MET", "Var2"))
     long <- long[long$value2>500,] # here the actual filtering....i.e. keep only seg and value when total catch kg is > threshold in kg
     long <- as.data.frame(long)
     long <- long[,c("met_desc","LE_MET","Stock", a_variable, "Year", "value")]
  

    
     long$LE_MET <- paste(long$LE_MET, the_names[count])
     
     colnames(long)[colnames(long)==a_variable] <- "Total"
   
     if(y==years[1]){ agg <- long } else{
      agg <- rbind.data.frame(agg,
             long)
     }
     
   }
    
     the_agg <- rbind.data.frame(the_agg, agg)
 }


## DEM
 library(ggplot2)
 a_comment <- ""
 a_unit <- 1
 

# DEM
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

  the_agg_plot$Stockname <- Stocknames[as.character(the_agg_plot$Stock)]

 # a visual fix adding all combi--
 #dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), Year=levels(factor(the_agg_plot$Year)))
 #dd$value <- 0
 #dd[,"Total"] <- 0
 #dd <- dd[,colnames(the_agg_plot)]
 #rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$Year)
 #rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 #dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 #the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---
   
   
    a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}


  the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)

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


 collecting_table <- NULL
 collecting_table2019 <- NULL

  #namefile <- paste0("barplot_mean_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_CPUF_and_VPUF.tif")
 the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot0$met_desc <- factor(the_agg_plot0$met_desc, level=fleet_segments_ordered2) # reorder)
 p0_barplot_bottomfishing_dem_euro_smallvids <- ggplot(data=the_agg_plot0, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (euros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p0)

 #namefile <- paste0("barplot_mean_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_CPUF_and_VPUF.tif")
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$met_desc <- factor(the_agg_plot1$met_desc, level=fleet_segments_ordered2) # reorder)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
 p1_barplot_bottomfishing_dem_land_smallvids <- ggplot(data=the_agg_plot1, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p1)


 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
 the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot2$met_desc <- factor(the_agg_plot2$met_desc, level=fleet_segments_ordered2) # reorder)
 p2_barplot_bottomfishing_dem_fuel_smallvids <- ggplot(data=the_agg_plot2, aes(x=met_desc, y=value/1e3, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y ="Fuel (thousand litres)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p2)

  the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot3$met_desc <- factor(the_agg_plot3$met_desc, level=fleet_segments_ordered2) # reorder)
  p3_barplot_bottomfishing_dem_cpuf_smallvids <- ggplot(data=the_agg_plot3, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "CPUF (kg per litre)", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot4$met_desc <- factor(the_agg_plot4$met_desc, level=fleet_segments_ordered2) # reorder)
  p4_barplot_bottomfishing_dem_vpuf_smallvids <- ggplot(data=the_agg_plot4, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "VPUF (euro per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
         theme(axis.text.x=element_blank()) 
        #theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot5$met_desc <- factor(the_agg_plot5$met_desc, level=fleet_segments_ordered2) # reorder)
  p5_barplot_bottomfishing_dem_fpuc_smallvids <- ggplot(data=the_agg_plot5, aes(x=met_desc, y=value/a_unit, fill=Stockname)) +#  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_speciesnames[unique(the_agg_plot5$Stockname)], name="Species")  + theme_minimal() + guides(fill =guide_legend(ncol=7)) +
       # theme(axis.text.x=element_blank())   
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p5)

  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot6$met_desc <- factor(the_agg_plot6$met_desc, level=fleet_segments_ordered2) # reorder)
  p6_barplot_bottomfishing_dem_fpuv_smallvids <- ggplot(data=the_agg_plot6, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) +  labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p6)
 
  ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    the_agg_plot8[the_agg_plot8$ value>100,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  p8_barplot_bottomfishing_dem_mprice_smallvids <- ggplot(data=the_agg_plot8, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = a_func_mean) + 
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p8)


 # for paper:
   a_width <- 6000 ; a_height <- 5500
 namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_FPUC_and_FPUV.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
   library(ggpubr)
   ggarrange(p1_barplot_bottomfishing_dem_land_smallvids, p2_barplot_bottomfishing_dem_fuel_smallvids, 
                  p4_barplot_bottomfishing_dem_vpuf_smallvids, p5_barplot_bottomfishing_dem_fpuc_smallvids, ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="bottom")
dev.off()


 # for paper:
   a_width <- 6000 ; a_height <- 5500
tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  "Euro_catch_per_per_kg.tiff"),   width = a_width, height = 3500,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p8_barplot_bottomfishing_dem_mprice_smallvids, ncol=1, heights=c(1),common.legend = TRUE, legend="bottom")
dev.off()


  a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot0$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_mean), cv=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot1$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_mean), cv=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot2$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_mean), cv=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot3$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_mean), cv=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot4$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_mean), cv=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot5$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_mean), cv=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot6$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_mean), cv=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_cv))) 
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot8$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_mean), cv=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_cv))) 

  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot0[the_agg_plot0$Year==2019,]$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0[the_agg_plot0$Year==2019,]$Total, the_agg_plot0[the_agg_plot0$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot0[the_agg_plot0$Year==2019,]$Total, the_agg_plot0[the_agg_plot0$Year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot1[the_agg_plot1$Year==2019,]$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1[the_agg_plot1$Year==2019,]$Total, the_agg_plot1[the_agg_plot1$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot1[the_agg_plot1$Year==2019,]$Total, the_agg_plot1[the_agg_plot1$Year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot2[the_agg_plot2$Year==2019,]$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2[the_agg_plot2$Year==2019,]$Total, the_agg_plot2[the_agg_plot2$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot2[the_agg_plot2$Year==2019,]$Total, the_agg_plot2[the_agg_plot2$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot3[the_agg_plot3$Year==2019,]$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3[the_agg_plot3$Year==2019,]$Total, the_agg_plot3[the_agg_plot3$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot3[the_agg_plot3$Year==2019,]$Total, the_agg_plot3[the_agg_plot3$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot4[the_agg_plot4$Year==2019,]$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4[the_agg_plot4$Year==2019,]$Total, the_agg_plot4[the_agg_plot4$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot4[the_agg_plot4$Year==2019,]$Total, the_agg_plot4[the_agg_plot4$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot5[the_agg_plot5$Year==2019,]$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5[the_agg_plot5$Year==2019,]$Total, the_agg_plot5[the_agg_plot5$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot5[the_agg_plot5$Year==2019,]$Total, the_agg_plot5[the_agg_plot5$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot6[the_agg_plot6$Year==2019,]$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6[the_agg_plot6$Year==2019,]$Total, the_agg_plot6[the_agg_plot6$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot6[the_agg_plot6$Year==2019,]$Total, the_agg_plot6[the_agg_plot6$Year==2019,]$LE_MET, a_func_cv))) 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="DemersalFishing_SmallVLargeMesh", seg=levels(the_agg_plot8[the_agg_plot8$Year==2019,]$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8[the_agg_plot8$Year==2019,]$Total, the_agg_plot8[the_agg_plot8$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot8[the_agg_plot8$Year==2019,]$Total, the_agg_plot8[the_agg_plot8$Year==2019,]$LE_MET, a_func_cv))) 


##!!!!!!!!!!!!!!!!!!!!!!##
## PEL 
 library(ggplot2)

 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

 # a visual fix adding all combi--
 #dd <- expand.grid(LE_MET=levels(factor(the_agg_plot$LE_MET)), Stock=levels(factor(the_agg_plot$Stock)), Year=levels(factor(the_agg_plot$Year)))
 #dd$value <- 0
 #dd[,"Total"] <- 0
 #dd <- dd[,colnames(the_agg_plot)]
 #rownames(the_agg_plot) <- paste0(the_agg_plot$LE_MET,the_agg_plot$Stock,the_agg_plot$Year)
 #rownames(dd) <- paste0(dd$LE_MET,dd$Stock,dd$Year)
 #dd <- dd[!rownames(dd)%in%rownames(the_agg_plot),]
 #the_agg_plot <- rbind.data.frame(the_agg_plot, dd)
 #---


  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)
 
  the_agg_plot$Stockname <- Stocknames[as.character(the_agg_plot$Stock)]

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


  # PEL
 the_agg_plot0 <- as.data.frame(the_agg_plot[grep("(z)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot0$LE_MET <- gsub("\\(z)","", the_agg_plot0$LE_MET)
 the_agg_plot0$LE_MET <- factor(the_agg_plot0$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot0$met_desc <- factor(the_agg_plot0$met_desc, level=fleet_segments_ordered2) # reorder)
 p0_barplot_bottomfishing_dem_euro_smallvids <- ggplot(data=the_agg_plot0, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Value (KEuros)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p0)

# PEL
 the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
 the_agg_plot1$LE_MET <- factor(the_agg_plot1$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot1$met_desc <- factor(the_agg_plot1$met_desc, level=fleet_segments_ordered2) # reorder)
 p1_barplot_bottomfishing_pel_land_smallvids <- ggplot(data=the_agg_plot1, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p1)
 
  the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
  the_agg_plot2$LE_MET <- factor(the_agg_plot2$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot2$met_desc <- factor(the_agg_plot2$met_desc, level=fleet_segments_ordered2) # reorder)
  p2_barplot_bottomfishing_pel_fuel_smallvids <- ggplot(data=the_agg_plot2, aes(x=met_desc, y=value/1e3, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="Fuel (thousands litres)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p2)
 
 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 the_agg_plot3$LE_MET <- factor(the_agg_plot3$LE_MET, level=fleet_segments_ordered) # reorder
 the_agg_plot3$met_desc <- factor(the_agg_plot3$met_desc, level=fleet_segments_ordered2) # reorder)
 p3_barplot_bottomfishing_pel_cpuf_smallvids <- ggplot(data=the_agg_plot3, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=7))  
  #print(p3)
 
  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$LE_MET <- factor(the_agg_plot4$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot4$met_desc <- factor(the_agg_plot4$met_desc, level=fleet_segments_ordered2) # reorder)
  p4_barplot_bottomfishing_pel_vpuf_smallvids <- ggplot(data=the_agg_plot4, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "VPUF (euro per litre)", x= "") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
       theme(axis.text.x=element_blank()) 
       # theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p4)
 
   the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  the_agg_plot5$LE_MET <- factor(the_agg_plot5$LE_MET, level=fleet_segments_ordered) # reorder
  #the_agg_plot5 <- the_agg_plot5[the_agg_plot5$value<3,]  # debug for other
  the_agg_plot5$met_desc <- factor(the_agg_plot5$met_desc, level=fleet_segments_ordered2) # reorder)
 p5_barplot_bottomfishing_pel_fpuc_smallvids <- ggplot(data=the_agg_plot5, aes(x=met_desc, y=value/a_unit, fill=Stockname)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per kg catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_speciesnames[unique(the_agg_plot5$Stockname)], name="Species")  + theme_minimal() + guides(fill =guide_legend(ncol=7)) +
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)) #+
        #theme(axis.text.x=element_blank()) + 
         #ylim(c(0,10))  # caution: this removes some points!...
  #print(p5)
 
  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  the_agg_plot6$LE_MET <- factor(the_agg_plot6$LE_MET, level=fleet_segments_ordered) # reorder
  the_agg_plot6$met_desc <- factor(the_agg_plot6$met_desc, level=fleet_segments_ordered2) # reorder)
  p6_barplot_bottomfishing_pel_fpuv_smallvids <- ggplot(data=the_agg_plot6, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per euro catch", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12)) +
        ylim(c(0,4)) # caution: this removes some points!...
  #print(p6)
  
   ### ADD-ON mean price
   the_agg_plot8 <- as.data.frame(the_agg_plot[grep("(h)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot8$LE_MET <- gsub("\\(h)","", the_agg_plot8$LE_MET)
    the_agg_plot8 <- the_agg_plot8[!the_agg_plot8$LE_MET=="OTHER_0_0_0 ",] # remove outlier
    the_agg_plot8[the_agg_plot8$value>100,"value"] <- 0 # remove outlier
  the_agg_plot8$LE_MET <- factor(the_agg_plot8$LE_MET, level=fleet_segments_ordered) # reorder
  p8_barplot_bottomfishing_pel_mprice_smallvids <- ggplot(data=the_agg_plot8, aes(x=met_desc, y=value/a_unit, fill=Stock)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") + 
   labs(y = "Euro catch per kg", x= "Fleet-segments") +
       scale_fill_manual(values=some_color_species, name="Species") + theme_minimal() + guides(fill =guide_legend(ncol=7))  + 
        # theme(axis.text.x=element_blank()) 
         theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p8)

 
 
  # for paper:
  a_width <- 6000 ; a_height <- 5500
 namefile <- paste0("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)],  a_comment, "_PEL_plot_land_and_FPUC_and_FPUV.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
   library(ggpubr)
   ggarrange(p1_barplot_bottomfishing_pel_land_smallvids, p2_barplot_bottomfishing_pel_fuel_smallvids, 
                   p4_barplot_bottomfishing_pel_vpuf_smallvids, p5_barplot_bottomfishing_pel_fpuc_smallvids, ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="bottom")
 dev.off()
 
 
  # for paper:
  a_width <- 6000 ; a_height <- 5500
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  "Euro_catch_per_per_kg_PEL.tiff"),   width = a_width, height = 3000,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

  library(ggpubr)
  ggarrange(p8_barplot_bottomfishing_pel_mprice_smallvids, ncol=1, heights=c(1),common.legend = TRUE, legend="bottom")
dev.off()

 
# export underlying data
  a_func_mean <- function (x) mean(x[x!=0 & !is.na(x)])
  a_func_cv <- function(x) {sqrt(var(x[x!=0 & !is.na(x)]))/mean(x[x!=0 & !is.na(x)])}
   collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot0$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_mean), cv=tapply(the_agg_plot0$Total, the_agg_plot0$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot1$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_mean), cv=tapply(the_agg_plot1$Total, the_agg_plot1$LE_MET, a_func_cv) ))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot2$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_mean), cv=tapply(the_agg_plot2$Total, the_agg_plot2$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot3$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_mean), cv=tapply(the_agg_plot3$Total, the_agg_plot3$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot4$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_mean), cv=tapply(the_agg_plot4$Total, the_agg_plot4$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot5$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_mean), cv=tapply(the_agg_plot5$Total, the_agg_plot5$LE_MET, a_func_cv)))
  collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot6$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_mean), cv=tapply(the_agg_plot6$Total, the_agg_plot6$LE_MET, a_func_cv))) 
   collecting_table <- rbind.data.frame(collecting_table, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot8$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_mean), cv=tapply(the_agg_plot8$Total, the_agg_plot8$LE_MET, a_func_cv))) 

  collecting_table[,4] <- round(collecting_table[,4],4)
  collecting_table[,5] <- round(collecting_table[,5],4)
  write.table(collecting_table,
            file=file.path(getwd(), "outputs2020_lgbkonly", "output_plots", 
            paste("barplot_mean_fuel_efficiency_", years[1], "-", years[length(years)], "_DEM_PEL_plot_land_and_FPUC_and_FPUV.dat")),
             row.names=FALSE, quote=FALSE, sep=";")
            
 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot0[the_agg_plot0$Year==2019,]$LE_MET), var="Value (KEuros)", average=tapply(the_agg_plot0[the_agg_plot1$Year==2019,]$Total, the_agg_plot0[the_agg_plot0$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot0[the_agg_plot0$Year==2019,]$Total, the_agg_plot0[the_agg_plot0$Year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot1[the_agg_plot1$Year==2019,]$LE_MET), var="Landings (tons)", average=tapply(the_agg_plot1[the_agg_plot1$Year==2019,]$Total, the_agg_plot1[the_agg_plot1$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot1[the_agg_plot1$Year==2019,]$Total, the_agg_plot1[the_agg_plot1$Year==2019,]$LE_MET, a_func_cv) ))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot2[the_agg_plot2$Year==2019,]$LE_MET), var="Fuel (litres)", average=tapply(the_agg_plot2[the_agg_plot2$Year==2019,]$Total, the_agg_plot2[the_agg_plot2$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot2[the_agg_plot2$Year==2019,]$Total, the_agg_plot2[the_agg_plot2$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot3[the_agg_plot3$Year==2019,]$LE_MET), var="CPUF (kg per litre)", average=tapply(the_agg_plot3[the_agg_plot3$Year==2019,]$Total, the_agg_plot3[the_agg_plot3$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot3[the_agg_plot3$Year==2019,]$Total, the_agg_plot3[the_agg_plot3$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot4[the_agg_plot4$Year==2019,]$LE_MET), var="VPUF (euro per litre)", average=tapply(the_agg_plot4[the_agg_plot4$Year==2019,]$Total, the_agg_plot4[the_agg_plot4$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot4[the_agg_plot4$Year==2019,]$Total, the_agg_plot4[the_agg_plot4$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot5[the_agg_plot5$Year==2019,]$LE_MET), var="Litre per kg catch", average=tapply(the_agg_plot5[the_agg_plot5$Year==2019,]$Total, the_agg_plot5[the_agg_plot5$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot5[the_agg_plot5$Year==2019,]$Total, the_agg_plot5[the_agg_plot5$Year==2019,]$LE_MET, a_func_cv)))
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot6[the_agg_plot6$Year==2019,]$LE_MET), var="Litre per euro catch", average=tapply(the_agg_plot6[the_agg_plot6$Year==2019,]$Total, the_agg_plot6[the_agg_plot6$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot6[the_agg_plot6$Year==2019,]$Total, the_agg_plot6[the_agg_plot6$Year==2019,]$LE_MET, a_func_cv))) 
  collecting_table2019 <- rbind.data.frame(collecting_table2019, cbind.data.frame(type="PelagicFishing_SmallVSmallOrNoMesh", seg=levels(the_agg_plot8[the_agg_plot8$Year==2019,]$LE_MET), var="Euro catch per kg", average=tapply(the_agg_plot8[the_agg_plot8$Year==2019,]$Total, the_agg_plot8[the_agg_plot8$Year==2019,]$LE_MET, a_func_mean), cv=tapply(the_agg_plot8[the_agg_plot8$Year==2019,]$Total, the_agg_plot8[the_agg_plot8$Year==2019,]$LE_MET, a_func_cv))) 

  collecting_table2019[,4] <- round(collecting_table2019[,4],4)
  collecting_table2019[,5] <- round(collecting_table2019[,5],4)
  # export underlying data
  write.table(collecting_table2019,
            file=file.path(getwd(), "outputs2020_lgbkonly", "output_plots", 
            paste("barplot_mean_fuel_efficiency_2019_DEM_PEL_plot_land_and_FPUC_and_FPUV.dat")),
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
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp",  "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_EURO_", "LE_MPRICE_")
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


   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]




   
 
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 # DEM
 some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

 a_width <- 6200 ; a_height <- 6500
 library(ggplot2)
 a_comment <- ""
 a_unit <- 1

 #--------------
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

 # caution filter out non-relevant species for these fleets
 the_agg_plot<-  the_agg_plot[! (grepl("NOP", the_agg_plot$Stock) | grepl("OTH", the_agg_plot$Stock) | grepl("HER", the_agg_plot$Stock)| grepl("OYF", the_agg_plot$Stock) | grepl("COC", the_agg_plot$Stock) | grepl("WHB", the_agg_plot$Stock) | grepl("SPR", the_agg_plot$Stock) | grepl("HOM", the_agg_plot$Stock) | grepl("CSH", the_agg_plot$Stock) | grepl("PRA", the_agg_plot$Stock) | grepl("MAC", the_agg_plot$Stock) | grepl("SAN", the_agg_plot$Stock) | grepl("ELE", the_agg_plot$Stock)| grepl("MUS", the_agg_plot$Stock) | grepl("WHG", the_agg_plot$Stock)   ),]

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
  
  # order fleets in the legend
  the_agg_plot$met_desc <- factor(the_agg_plot$met_desc)
  the_agg_plot$met_desc <-  factor(as.character(the_agg_plot$met_desc), levels= levels(the_agg_plot$met_desc)[order(substr(levels(the_agg_plot$met_desc),4,9) )]  )  # reorder the fleet desc in alphabetical order


  
   
  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  p1_area_bottomfishing_dem_land_per_stk <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/1e3, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +
  geom_area(aes(fill=LE_MET))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 
 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_area_bottomfishing_dem_cpuf_per_stk <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +  
  geom_area(aes(fill=LE_MET))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p2)
 
 the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_dem_vpuf_per_stk <- ggplot(the_agg_plot4, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=LE_MET))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p3)

 
 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  p5_area_bottomfishing_dem_fpuc_per_stk <- ggplot(the_agg_plot5, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)

 the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  p6_area_bottomfishing_dem_fpuv_per_stk <- ggplot(the_agg_plot6, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p6)



 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_areaplot_per_stk_land_and_CPUF.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
  ggarrange(p1_area_bottomfishing_dem_land_per_stk, p4_area_bottomfishing_dem_vpuf_per_stk, p5_area_bottomfishing_dem_fpuc_per_stk, ncol=3, common.legend = TRUE, legend="right")
dev.off()




#!!!!!!!!!!!!
#!!!!!!!!!!!!
#!!!!!!!!!!!!
# PEL
### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp",  "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_EURO_", "LE_MPRICE_")
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


   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]




   
 
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!##
 # PEL
 some_color_seg <-  c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
   "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C",
   "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
   "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

 library(ggplot2)
 a_comment <- ""
 a_unit <- 1

 #--------------
 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

 # caution filter out non-relevant species for these fleets
 the_agg_plot<-  the_agg_plot[! (grepl("BLL", the_agg_plot$Stock) | grepl("DAB", the_agg_plot$Stock)| grepl("ELE", the_agg_plot$Stock)  |  grepl("FLE", the_agg_plot$Stock) | grepl("GAR", the_agg_plot$Stock) | grepl("LEM", the_agg_plot$Stock)| 
                                   grepl("LUM", the_agg_plot$Stock) | grepl("MUS", the_agg_plot$Stock) | grepl("NEP", the_agg_plot$Stock) | 
                                   grepl("OYF", the_agg_plot$Stock) | grepl("PLE", the_agg_plot$Stock) | grepl("COD", the_agg_plot$Stock) | 
                                   grepl("SAL", the_agg_plot$Stock) | grepl("SOL", the_agg_plot$Stock) | grepl("TUR", the_agg_plot$Stock)    ),]
 the_agg_plot$Stock <- factor(the_agg_plot$Stock)

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

  the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)
  
  # order fleets in the legend
  the_agg_plot$met_desc <- factor(the_agg_plot$met_desc)
  the_agg_plot$met_desc <-  factor(as.character(the_agg_plot$met_desc), levels= levels(the_agg_plot$met_desc)[order(substr(levels(the_agg_plot$met_desc),4,9) )]  )  # reorder the fleet desc in alphabetical order


  
   
  the_agg_plot1 <- as.data.frame(the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot1$LE_MET <- gsub("\\(a)","", the_agg_plot1$LE_MET)
  p1_area_bottomfishing_pel_land_per_stk <- ggplot(the_agg_plot1, aes(x=as.character(Year), y=value/1e3, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +
  geom_area(aes(fill=LE_MET))  +     labs(y = "Landings (tons)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 
 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
 p3_area_bottomfishing_pel_cpuf_per_stk <- ggplot(the_agg_plot3, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +  
  geom_area(aes(fill=LE_MET))  +     labs(y = "CPUF (kg per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p2)
 
 the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
 p4_area_bottomfishing_pel_vpuf_per_stk <- ggplot(the_agg_plot4, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=LE_MET))  +     labs(y = "VPUF (euro per litre)", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p3)

 
 the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot5$LE_MET)
  p5_area_bottomfishing_pel_fpuc_per_stk <- ggplot(the_agg_plot5, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per kg catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=1, position="right"))  +
    xlab("")
 #print(p5)

 the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot6$LE_MET)
  p6_area_bottomfishing_pel_fpuv_per_stk <- ggplot(the_agg_plot6, aes(x=as.character(Year), y=value/a_unit, group=LE_MET)) +
     facet_wrap(. ~ Stock, scales = "free_y", ncol=1)  +  theme_minimal() + theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1))  +   
  geom_area(aes(fill=met_desc))  +     labs(y = "Litre per euro catch", x = "Year")   +
   scale_fill_manual(values=some_color_seg, name="Fleet-segments") +   guides(fill =guide_legend(ncol=2))  +
    xlab("")
 #print(p6)


 a_width <- 6000 ; a_height <- 5500
 namefile <- paste0("ts_fuel_efficiency", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_areaplot_per_stk_land_and_CPUF.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 library(ggpubr)
  ggarrange(p1_area_bottomfishing_pel_land_per_stk, p4_area_bottomfishing_pel_vpuf_per_stk, p5_area_bottomfishing_pel_fpuc_per_stk, ncol=3, common.legend = TRUE, legend="right")
dev.off()







#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 
#REDO SOME STUFF FOR A STANDALONE CODE:
outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
years <- 2005:2019
load(file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))
eflalo <- eflalo_small_vids
spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                    "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                    "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")
some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                        "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                         "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                         "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                          "WIT"="red", "WHG"="yellow", "OTH"="blue",
                          "COC"="#108291", "OYF"="#6a9110", "LUM"="red", "SAL"="#c2a515", "BLL"="cyan")  # specific to small vids


 # search in Baltic and North Sea
eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=TRUE)
  if(per_metier_level6 && per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
  if(per_metier_level6 && !per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
library(doBy)
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



## SPECIES ORIENTED - SECTORED PER FLEET-SEG
 ## DEM
  
 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp",  "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_EURO_", "LE_MPRICE_")
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


   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]




     
#----------
## DEM
 library(ggplot2)

# DEM
 the_agg_plot <- as.data.frame(the_agg[grep("LargeMesh",the_agg$LE_MET),])

  # DEM
 the_agg_plot$LE_MET <- gsub("LargeMesh_", "", the_agg_plot$LE_MET)

 
 # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  ss <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE) & as.numeric(as.character(the_agg_plot$value))>10, ]  # 
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% unique(ss$Stock),]

  # caution filter out non-relevant species for these fleets
  the_agg_plot<-  the_agg_plot[! (grepl("ELE", the_agg_plot$Stock) | grepl("GAR", the_agg_plot$Stock)| grepl("HER", the_agg_plot$Stock)  |  grepl("OTHER", the_agg_plot$Stock)    ),]
  the_agg_plot$Stock <- factor(the_agg_plot$Stock)
 
 
   # find order of stcok
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
  p1_barplot_bottomfishing_dem_land_per_stk <- ggplot(data=the_agg_plot1, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2))  
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
  the_agg_plot2$Stock <- factor(the_agg_plot2$Stock, level=stock_ordered) # reorder
  p2_barplot_bottomfishing_dem_fuel_per_stk <- ggplot(data=the_agg_plot2, aes(x=Stock, y=value/1e3, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="Fuel (thousands litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$Stock <- factor(the_agg_plot3$Stock, level=stock_ordered) # reorder
  p3_barplot_bottomfishing_dem_cpuf_per_stk <- ggplot(data=the_agg_plot3, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$Stock <- factor(the_agg_plot4$Stock, level=stock_ordered) # reorder
  p4_barplot_bottomfishing_dem_vpuf_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "VPUF (euro per litre)", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=2))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot4$LE_MET)
  the_agg_plot5$Stock <- factor(the_agg_plot5$Stock, level=stock_ordered) # reorder
  p5_barplot_bottomfishing_dem_fpuc_per_stk <- ggplot(data=the_agg_plot5, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per kg catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=2))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p5)

  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot4$LE_MET)
  the_agg_plot6$Stock <- factor(the_agg_plot6$Stock, level=stock_ordered) # reorder
  p6_barplot_bottomfishing_dem_fpuv_per_stk <- ggplot(data=the_agg_plot6, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per eour catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=2))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p6)


 a_width <- 6000 ; a_height <- 5500
  namefile <- paste0("barplot_mean_fuel_efficiency_per_stock_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_DEM_plot_land_and_CPUF_and_VPUF.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_barplot_bottomfishing_dem_land_per_stk, p2_barplot_bottomfishing_dem_fuel_per_stk, p4_barplot_bottomfishing_dem_vpuf_per_stk, p5_barplot_bottomfishing_dem_fpuc_per_stk, 
            ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="right")
dev.off()








 
#REDO SOME STUFF FOR A STANDALONE CODE:
outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_lgbkonly")
dataPath  <- "D:/FBA/BENTHIS_2020/EflaloAndTacsat/"
years <- 2005:2019
load(file=file.path(dataPath ,paste("eflalo_small_vids_ally_",years[1],"-",years[length(years)],".RData", sep='')))
eflalo <- eflalo_small_vids
spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
color_species <- c("#E69F00","hotpink","#56B4E9","#F0E442", "green", "#0072B2", "mediumorchid4", "#CC79A7",
                    "indianred2", "#EEC591", "#458B00", "#F0F8FF", "black", "#e3dcbf", "#CD5B45", "lightseagreen",
                    "#6495ED", "#CDC8B1", "#00FFFF", "#8B0000", "#008B8B", "#A9A9A9", "#76a5c4", "red", "yellow", "blue")
some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                        "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                         "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                         "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                          "WIT"="red", "WHG"="yellow", "OTH"="blue",
                          "COC"="#108291", "OYF"="#6a9110", "LUM"="red", "SAL"="#c2a515", "BLL"="cyan")  # specific to small vids


 # search in Baltic and North Sea
eflalo$VesselSize <-  cut(eflalo$VE_LEN, breaks=c(0,11.99,17.99,23.99,39.99,100), right=TRUE)
  if(per_metier_level6 && per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndVsizeAndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
  if(per_metier_level6 && !per_vessel_size){
     load(file=file.path(getwd(), "outputs2020_lgbkonly", paste("aggResultPerMetAllyMet6AndRatiosSmallVids",years[1],"-",years[length(years)],".RData", sep="")))  # aggResultPerMetAlly
  }
library(doBy)
spp <- colnames(aggResultPerMetAlly) [grep("LE_EURO_", colnames(aggResultPerMetAlly))]   ; spp <- gsub("LE_EURO_", "", spp) ; spp <- spp [!spp=="SPECS"]



## SPECIES ORIENTED - SECTORED PER FLEET-SEG
 ## PEL
  
 ### SUMMARIZE LANDINGS AND CPUF ON THE SAME GRAPH.......
 variables <- c("KEUROallsp","KKGallsp", "LE_KG_LITRE_FUEL", "CPUFallsp",  "VPUFallsp", "FPUCallsp", "FPUVallsp",  "mpriceallsp")
 prefixes  <- c("LE_EURO_","LE_KG_",       "LE_KG_",          "LE_KG_",  "LE_KG_", "LE_KG_", "LE_EURO_", "LE_MPRICE_")
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


   the_agg <- the_agg[,-grep("Var", colnames(the_agg))]




     
#----------
# PEL
 the_agg_plot <- as.data.frame(the_agg[grep("SmallMesh",the_agg$LE_MET),])

  # PEL
 the_agg_plot$LE_MET <- gsub("SmallMesh_", "", the_agg_plot$LE_MET)

 
 # filtering the ratios:
  # a quick informative table (for kg) for filtering out the ratios that are misleading because low catch kg
  ss <- the_agg_plot[grep("(a)",the_agg_plot$LE_MET, fixed=TRUE) & as.numeric(as.character(the_agg_plot$value))>10, ]  # 
  the_agg_plot <- the_agg_plot[the_agg_plot$Stock %in% unique(ss$Stock),]

  # caution filter out non-relevant species for these fleets
  the_agg_plot$Stock <- as.character(the_agg_plot$Stock )
  the_agg_plot<-  the_agg_plot[( grepl("COD 27.3", the_agg_plot$Stock, fixed=TRUE) | grepl("MUS", the_agg_plot$Stock, fixed=TRUE) | grepl("OYF", the_agg_plot$Stock, fixed=TRUE) | grepl("NEP", the_agg_plot$Stock, fixed=TRUE) | grepl("SAL", the_agg_plot$Stock, fixed=TRUE) | grepl("ELE 27.4", the_agg_plot$Stock, fixed=TRUE)    ),]
  the_agg_plot<-  the_agg_plot[!(grepl("OTHER", the_agg_plot$Stock, fixed=TRUE)),]
  the_agg_plot$Stock <- factor(the_agg_plot$Stock)
 
 
   # find order of stcok
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
  p1_barplot_bottomfishing_pel_land_per_stk <- ggplot(data=the_agg_plot1, aes(x=Stock, y=value/a_unit, fill=,met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Landings (tons)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2))  
  #print(p1)

 the_agg_plot2 <- as.data.frame(the_agg_plot[grep("(b)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot2$LE_MET <- gsub("\\(b)","", the_agg_plot2$LE_MET)
  the_agg_plot2$Stock <- factor(the_agg_plot2$Stock, level=stock_ordered) # reorder
  p2_barplot_bottomfishing_pel_fuel_per_stk <- ggplot(data=the_agg_plot2, aes(x=Stock, y=value/1e3, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="Fuel (thousands litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2))  
  #print(p2)

 the_agg_plot3 <- as.data.frame(the_agg_plot[grep("(c)",the_agg_plot$LE_MET, fixed=TRUE),])
 the_agg_plot3$LE_MET <- gsub("\\(c)","", the_agg_plot3$LE_MET)
  the_agg_plot3$Stock <- factor(the_agg_plot3$Stock, level=stock_ordered) # reorder
  p3_barplot_bottomfishing_pel_cpuf_per_stk <- ggplot(data=the_agg_plot3, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y ="CPUF (kg per litre)", x= "") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments")  + theme_minimal() + theme(axis.text.x=element_blank()) + guides(fill =guide_legend(ncol=2))  
  #print(p3)

  the_agg_plot4 <- as.data.frame(the_agg_plot[grep("(d)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot4$LE_MET <- gsub("\\(d)","", the_agg_plot4$LE_MET)
  the_agg_plot4$Stock <- factor(the_agg_plot4$Stock, level=stock_ordered) # reorder
  p4_barplot_bottomfishing_pel_vpuf_per_stk <- ggplot(data=the_agg_plot4, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "VPUF (euro per litre)", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=2))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p4)

  the_agg_plot5 <- as.data.frame(the_agg_plot[grep("(e)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot5$LE_MET <- gsub("\\(e)","", the_agg_plot4$LE_MET)
  the_agg_plot5$Stock <- factor(the_agg_plot5$Stock, level=stock_ordered) # reorder
  p5_barplot_bottomfishing_pel_fpuc_per_stk <- ggplot(data=the_agg_plot5, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per kg catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=2))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p5)

  the_agg_plot6 <- as.data.frame(the_agg_plot[grep("(f)",the_agg_plot$LE_MET, fixed=TRUE),])
  the_agg_plot6$LE_MET <- gsub("\\(f)","", the_agg_plot4$LE_MET)
  the_agg_plot6$Stock <- factor(the_agg_plot6$Stock, level=stock_ordered) # reorder
  p6_barplot_bottomfishing_pel_fpuv_per_stk <- ggplot(data=the_agg_plot6, aes(x=Stock, y=value/a_unit, fill=met_desc)) + #  geom_bar(stat="identity", position=position_dodge())
  geom_bar(stat = "summary", fun = "mean") +  labs(y = "Litre per eour catch", x= "Species") +
       scale_fill_manual(values=some_color_seg, name="Fleet-segments") + theme_minimal() + guides(fill =guide_legend(ncol=2))  + 
        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1, size=12))
  #print(p6)


  a_width <- 6000 ; a_height <- 5500
 namefile <- paste0("barplot_mean_fuel_efficiency_per_stock_", a_variable, "_", years[1], "-", years[length(years)],  a_comment, "_PEL_plot_land_and_CPUF_and_VPUF.tif")
 tiff(filename=file.path(getwd(), "outputs2020_lgbkonly", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  library(ggpubr)
  ggarrange(p1_barplot_bottomfishing_pel_land_per_stk, p2_barplot_bottomfishing_pel_fuel_per_stk, p4_barplot_bottomfishing_pel_vpuf_per_stk, p5_barplot_bottomfishing_pel_fpuc_per_stk, 
            ncol=1, heights=c(1,1,1,2),common.legend = TRUE, legend="right")
dev.off()



