
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!COUPLE INTERPOLATED VMS WITH CATCH LANDED!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Add-on to the BENTHIS WP2 workflow. Therefore possible repetation of some steps

cat("start couplingInterpolatedVmsToLandings2020_pel.r\n")  
rm(list=ls())
library(vmstools)
library(maps)
library(mapdata)
library(doBy)

if(.Platform$OS.type == "unix") {
 codePath  <- file.path("/zhome","fe","8","43283","BENTHIS")
 dataPath  <- file.path("/zhome","fe","8","43283","BENTHIS","EflaloAndTacsat")
 #outPath   <- file.path("~","BENTHIS", "outputs")
 outPath   <- file.path("/zhome","fe","8","43283","BENTHIS", "outputs2020_pel")
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
    outPath   <- file.path("D:","FBA","BENTHIS_2020", "outputs2020_pel")
    polPath   <- "D:/FBA/BENTHIS/BalanceMaps"
   # years <- 2012:2019
    years <- 2005:2011
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


     # Gear codes to keep (PELAGIC GEARS IN THIS INSTANCE)
     gears2keep            <- c("OTM","PTM", "PS")
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
         dd <- subset(eflalo, LE_GEAR == a_gear & VE_REF == a_vid)
         if(nrow(dd)>0){
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
            } else{
               cat(paste("Fail for ", iFile, "...no eflalo for this gear!\n"))
            }
           }
         }  # end a_vessel

   }   # end a_year

} # end FALSE
 
 
  if(FALSE){  # do not re-run...this takes ages!

 
  ##-----------------------------------
  ## COMPUTE SWEPT AREA
  ##-----------------------------------
  compute_swept_area <- function(
                              tacsatIntGearVEREF=tacsatIntGearVEREF,
                              gear_param_per_metier=gear_param_per_metier,
                              towedGears=towedGears,
                              seineGears=seineGears,
                              VMS_ping_rate_in_hour=VMS_ping_rate_in_hour,
                              already_informed_width_for=NULL
                              ){

  if(is.null(already_informed_width_for)){
     tacsatIntGearVEREF <- tacsatIntGearVEREF[,!colnames(tacsatIntGearVEREF) %in%
                          c('GEAR_WIDTH', 'GEAR_WIDTH_LOWER', 'GEAR_WIDTH_UPPER', 'SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')]  # remove columns if exists
     } else{
     tacsatIntGearVEREF <- tacsatIntGearVEREF[,!colnames(tacsatIntGearVEREF) %in%
                          c('SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')]  # remove columns if exists
     
     }
  
  if(is.null(already_informed_width_for)){
  # MERGE WITH GEAR WIDTH
  GearWidth                   <- tacsatIntGearVEREF[!duplicated(data.frame(tacsatIntGearVEREF$VE_REF,tacsatIntGearVEREF$LE_MET,tacsatIntGearVEREF$VE_KW,tacsatIntGearVEREF$VE_LEN)), ]
  GearWidth                   <- GearWidth[,c('VE_REF','LE_MET','VE_KW', 'VE_LEN') ]
  GearWidth$GEAR_WIDTH        <- NA
  GearWidth$GEAR_WIDTH_LOWER  <- NA
  GearWidth$GEAR_WIDTH_UPPER  <- NA
  for (i in 1:nrow(GearWidth)) { # brute force...
    kW      <- GearWidth$VE_KW[i]
    LOA     <- GearWidth$VE_LEN[i]
    this    <- gear_param_per_metier[gear_param_per_metier$a_metier==as.character(GearWidth$LE_MET[i]),]
    a <- NULL ; b <- NULL
    a       <- this[this$param=='a', 'Estimate']
    b       <- this[this$param=='b', 'Estimate']
    GearWidth[i,"GEAR_WIDTH"]  <-   eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
    a       <- this[this$param=='a', 'Estimate']
    b       <- this[this$param=='b', 'Estimate'] +2*this[this$param=='b', 'Std..Error']
    GearWidth[i,"GEAR_WIDTH_UPPER"]  <-  eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
    a       <- this[this$param=='a', 'Estimate']
    b       <- this[this$param=='b', 'Estimate'] -2*this[this$param=='b', 'Std..Error']
    GearWidth[i,"GEAR_WIDTH_LOWER"]  <-  eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
  }
  tacsatIntGearVEREF                    <- merge(tacsatIntGearVEREF, GearWidth,by=c("VE_REF","LE_MET","VE_KW","VE_LEN"),
                                              all.x=T,all.y=F)

  }
                                              
  #  the swept area (note that could work oustide the loop area as well....)
  # for the trawlers...
  if(tacsatIntGearVEREF$LE_GEAR[1] %in% towedGears){
        tacsatIntGearVEREF$SWEPT_AREA_KM2 <- NA
        tacsatIntGearVEREF <- orderBy(~SI_DATIM,data=tacsatIntGearVEREF)
        a_dist             <- distance(c(tacsatIntGearVEREF$SI_LONG[-1],0),  c(tacsatIntGearVEREF$SI_LATI[-1],0),
                                         tacsatIntGearVEREF$SI_LONG, tacsatIntGearVEREF$SI_LATI)
        a_dist[length(a_dist)] <- rev(a_dist)[2]
        tacsatIntGearVEREF$SWEPT_AREA_KM2 <- a_dist * tacsatIntGearVEREF$GEAR_WIDTH
        tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER <- a_dist * tacsatIntGearVEREF$GEAR_WIDTH_LOWER
        tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER <- a_dist * tacsatIntGearVEREF$GEAR_WIDTH_UPPER
        # correct the transition between sequential fishing events
        #idx <- which(diff(tacsatIntGearVEREF$SI_DATIM)/60 > 15)   # if interval > 15 min then points belong to a different fishing event
        # CORRECTION Sep18:
        idx <- which( as.numeric(diff(tacsatIntGearVEREF$SI_DATIM), units='mins') > 15) # if interval > 15 min 
        idx <- c(idx, nrow(tacsatIntGearVEREF))            # to exclude the last observation 
        tacsatIntGearVEREF[ idx, c('SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')] <- NA
  }

  # for the seiners...
  if(tacsatIntGearVEREF$LE_GEAR[1]  %in% seineGears){
      tacsatIntGearVEREF$SWEPT_AREA_KM2         <- pi*(tacsatIntGearVEREF$GEAR_WIDTH/(2*pi))^2
      tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER   <- pi*(tacsatIntGearVEREF$GEAR_WIDTH_LOWER/(2*pi))^2
      tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER   <- pi*(tacsatIntGearVEREF$GEAR_WIDTH_UPPER/(2*pi))^2

      haul_duration                           <- 3 # assumption of a mean duration based from questionnaires to seiners
      tacsatIntGearVEREF$SWEPT_AREA_KM2         <- tacsatIntGearVEREF$SWEPT_AREA_KM2 * VMS_ping_rate_in_hour / haul_duration # correction to avoid counting the same circle are several time.
      tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER   <- tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER * VMS_ping_rate_in_hour / haul_duration # correction to avoid counting the same circle are several time.
      tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER   <- tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER * VMS_ping_rate_in_hour / haul_duration # correction to avoid counting the same circle are several time.
      idx                                     <- grep('SSC', as.character(tacsatIntGearVEREF$LE_GEAR))
      tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2'] <- tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2'] *1.5 # ad hoc correction to account for the SSC specificities
      tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_LOWER'] <- tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_LOWER'] *1.5 # ad hoc correction to account for the SSC specificities
      tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_UPPER'] <- tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_UPPER'] *1.5 # ad hoc correction to account for the SSC specificities
   }

   return(tacsatIntGearVEREF)
   }

  #-----------------------------------------------------------------------------
  # Add "gear width-vessel size" relationships table of parameters.
  #-----------------------------------------------------------------------------
  #gear_param_per_metier       <- read.table(file=file.path(dataPath, "estimates_for_gear_param_per_metier.txt"))
  # an equivalent is:

  gear_param_per_metier <- data.frame(
  a_metier=c('OT_CRU','OT_CRU','OT_DMF','OT_DMF','OT_MIX','OT_MIX','OT_MIX_ARA','OT_MIX_ARA','OT_MIX_DMF_BEN','OT_MIX_DMF_BEN','OT_MIX_DMF_PEL','OT_MIX_DMF_PEL','OT_MIX_DPS','OT_MIX_DPS','OT_MIX_NEP','OT_MIX_NEP','OT_MIX_TGS_CTC','OT_MIX_TGS_CTC','OT_MIX_TGS_OCC','OT_MIX_TGS_OCC','OT_SPF','OT_SPF','TBB_CRU','TBB_CRU','TBB_DMF','TBB_DMF','TBB_MOL','TBB_MOL','DRB_MOL','DRB_MOL','SDN_DEM','SDN_DEM','SSC_DEM','SSC_DEM'),
  param=c('a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b'),
  Estimate=c(5.10393560454806,0.468985756915913,9.6053549509854,0.433672763959314,10.6607888271164,0.292055014993337,37.5271604597435,0.149004797319136,3.21410379943408,77.981158829069,6.63707197355847,0.770594580782091,26.6738247840508,0.210221545999405,3.92727763464472,35.8253721834011,6.23686411376723,0.767375050454527,0.0192465419797634,119.140335982507,0.965238378524667,68.3889717127507,1.48117115311386,0.457788539321641,0.660086393453441,0.507845311175148,0.953001905566232,0.709356826689359,0.314245137194503,1.24544036138755,1948.83466676682,0.236271746198865,4461.27004311913,0.117589220782479),
  Std..Error=c(1.81527145191998,0.0597519960969362,3.98228885098937,0.067572002767068,6.69386377505425,0.104413257104915,10.6717875588847,0.044963446750424,1.67854244656697,40.9297885227685,2.69086696344053,0.126123213329976,5.37466576335144,0.030829495804396,0.928442484509969,21.0228522096513,1.46159830273852,0.0732116002636393,0.000552819642352548,0.510207569180525,0.205245990518183,7.45180177818494,0.278399892100703,0.0346555048025894,0.172902115850281,0.0388684340513048,0.315715856194751,0.138412196798781,0.110027479611801,0.10614681568516,637.25152416296,0.0636712369543136,1665.50234108383,0.118756519107319),
  t.value=c(2.81166521907769,7.84887179593252,2.41201864314765,6.41793562718951,1.59262112068153,2.79710664230959,3.51648308708138,3.31390958851994,1.91481830322951,1.90524216331315,2.46651806415295,6.10985527910701,4.96288066244663,6.81884476260001,4.22996329893018,1.70411568450042,4.26715336360309,10.4816046595234,34.8152281598731,233.513462322532,4.70283670871103,9.17750817164227,5.32030074414718,13.2096918492278,3.81768835047121,13.0657517744299,3.01854305657162,5.12495894939517,2.8560604887363,11.733186279291,3.05818753329251,3.71080816866175,2.67863330664306,0.990170658978435),
  Pr...t..=c(0.00613312535554725,1.21619365805854e-11,0.021410083292817,2.48114253493853e-07,0.114790848188445,0.00631861326022122,0.000513087659147687,0.0010462790834138,0.0692370736030276,0.0705334706657513,0.0147045751318625,7.39218704723967e-09,1.2637878625965e-05,2.97113026239585e-08,0.000166717383514359,0.097483711710908,0.000314181622785133,5.0948672020349e-10,9.05842416252619e-12,5.10054218622276e-20,0.000204968683311441,5.36482029322678e-08,0.00313939649832079,4.44157761915604e-05,0.000458495488420268,5.11509704563588e-16,0.00678642704689924,5.16047183433098e-05,0.0075895814688592,6.18091407283774e-13,0.00391206507124518,0.000614325243514857,0.0438919330122769,0.367557330382699),
  equ=c('DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=(a*kW)+b','DoS=(a*kW)+b','DoS=(a*LOA)+b','DoS=(a*LOA)+b','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(LOA^b)','beamw=a*(LOA^b)','dredgew=a*(LOA^b)','dredgew=a*(LOA^b)','seineropel=a*(kW^b)','seineropel=a*(kW^b)','seineropel=a*(LOA^b)','seineropel=a*(LOA^b)'),
  nb_records=c(124,124,39,39,94,94,271,271,48,48,190,190,45,45,53,53,24,24,12,12,19,19,7,7,42,42,22,22,33,33,47,47,8,8)
  )


    
  # Gear codes to keep (PELAGIC)
  gears2keep            <- c("OTM", "PTM", "PS")
  towedGears            <- c("OTM", "PTM")
  seineGears            <- c("PS")
  VMS_ping_rate_in_hour <- 1 # e.g. 1 hour for Denmark (rev(sort(table(intervalTacsat(sortTacsat(tacsat),level="vessel")$INTV))[1])
  

   spp <- c('LITRE_FUEL', 'COD', 'CSH', 'DAB', 'ELE', 'FLE', 'HAD', 'HER', 'HKE', 'HOM',
         'LEM', 'MAC', 'MON', 'MUS', 'NEP', 'NOP', 'PLE', 'POK', 'PRA', 'SAN',
           'SOL', 'SPR', 'TUR', 'WHB', 'WIT', 'WHG',
            'OTH')

   cols2keep <- c("VE_REF", "VE_LEN", "VE_KW", "SI_LATI","SI_LONG","SI_DATE","LE_GEAR","LE_MET","LE_MET_init","SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER","SWEPT_AREA_KM2_UPPER", "GEAR_WIDTH", "SI_DATIM", "SI_FT", "FT_REF" ) 

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

    print(a_year)
    load(file=file.path(outPath,a_year,"tacsatActivity.RData"))
    # for computing fuel use
    table.fuelcons.per.engine       <-  read.table(file= file.path(dataPath, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
    linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
    fuel_per_h                      <- function (a,x) a*(x^3)  # cubic law
    load(file=file.path(outPath,a_year,"steaming_cons_per_VE_REF_FT_REF.RData"))    # get steaming_cons_per_VE_REF_FT_REF  


    lst <- list(); count <- 0  ;vid_with_errors <- NULL
    for(iFile in fls){
     cat(paste(iFile, "\n"))
     count <- count+1
     load(file.path(outPath,a_year,"interpolated", "plus", iFile))

       
       nbpoints <- 12 # caution: chek this for your case.
  
   a_vessel <- sapply(strsplit(gsub(".RData","",iFile), split="_"), function(x)x[2])
   a_gear   <- sapply(strsplit(gsub(".RData","",iFile), split="_"), function(x)x[3])
   a_max_vessel_speed   <- quantile(as.numeric(as.character(tacsatp[tacsatp$VE_REF==a_vessel, 'SI_SP'])), 0.95) # we assume the towing is done at maximal load
   max_consumed            <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(tacsatp[tacsatp$VE_REF==a_vessel, 'VE_KW'][1]))))
   a                       <- max_consumed/ (a_max_vessel_speed^3) # scaling factor
  
   if(a_gear%in% c(towedGears)){
   #for towed gears and SSC: assume full load when dragging the trawl
      # fuel use
        full_load_factor <- 0.9 # they fish at 90% full load
        tacsatIntGearVEREF$LITRE_FUEL_FISHING      <- (fuel_per_h(as.numeric(as.character(a)), as.numeric(as.character(a_max_vessel_speed))) * full_load_factor) /nbpoints 
        tacsatIntGearVEREF$VE_REF_FT_REF           <-  paste0(tacsatIntGearVEREF$VE_REF,"_",tacsatIntGearVEREF$FT_REF) 
        nb_fishing_pts_per_VE_REF_FT_REF           <-  table(tacsatIntGearVEREF$VE_REF_FT_REF) # for dispatching evenly on fishing pts
        tacsatIntGearVEREF$FUEL_LITRE_STEAMING     <- steaming_cons_per_VE_REF_FT_REF[tacsatIntGearVEREF$VE_REF_FT_REF]  /  table(tacsatIntGearVEREF$VE_REF_FT_REF)[tacsatIntGearVEREF$VE_REF_FT_REF]  
        tacsatIntGearVEREF$LE_KG_LITRE_FUEL       <- tacsatIntGearVEREF$LITRE_FUEL_FISHING + tacsatIntGearVEREF$FUEL_LITRE_STEAMING 
        tacsatIntGearVEREF <- tacsatIntGearVEREF[, !colnames(tacsatIntGearVEREF) %in% c("VE_REF_FT_REF", "max_vessel_speed", "max_consumed", "a", "FUEL_LITRE_STEAMING")] # cleaning
    }
     if(a_gear%in% "PS"){
     #for seiners PS gears: actual speed is enough and a good proxy  
     # fuel use
        tacsatIntGearVEREF$LITRE_FUEL_FISHING      <- fuel_per_h(as.numeric(as.character(a)), (as.numeric(as.character(tacsatIntGearVEREF$SI_SP))))* VMS_ping_rate_in_hour*1.0
        tacsatIntGearVEREF$VE_REF_FT_REF           <-  paste0(tacsatIntGearVEREF$VE_REF,"_",tacsatIntGearVEREF$FT_REF) 
        nb_fishing_pts_per_VE_REF_FT_REF           <-  table(tacsatIntGearVEREF$VE_REF_FT_REF) # for dispatching evenly on fishing pts
        tacsatIntGearVEREF$FUEL_LITRE_STEAMING     <- steaming_cons_per_VE_REF_FT_REF[tacsatIntGearVEREF$VE_REF_FT_REF]  /  table(tacsatIntGearVEREF$VE_REF_FT_REF)[tacsatIntGearVEREF$VE_REF_FT_REF]  
        tacsatIntGearVEREF$LE_KG_LITRE_FUEL        <- tacsatIntGearVEREF$LITRE_FUEL_FISHING + tacsatIntGearVEREF$FUEL_LITRE_STEAMING 
        tacsatIntGearVEREF <- tacsatIntGearVEREF[, !colnames(tacsatIntGearVEREF) %in% c("VE_REF_FT_REF", "max_vessel_speed", "max_consumed", "a", "FUEL_LITRE_STEAMING")] # cleaning
   }  


     tacsatIntGearVEREF$LE_KG_OTH <- apply(tacsatIntGearVEREF[,colkg_to_sum], 1, sum, na.rm=TRUE)
     tacsatIntGearVEREF$LE_EURO_OTH <- apply(tacsatIntGearVEREF[,coleuro_to_sum], 1, sum, na.rm=TRUE)

   
     # compute the swept area
     tacsatIntGearVEREF <- compute_swept_area (tacsatIntGearVEREF, gear_param_per_metier, towedGears, seineGears, VMS_ping_rate_in_hour, already_informed_width_for=NULL)

     if(any(tacsatIntGearVEREF$SWEPT_AREA_KM2>100, na.rm = TRUE) ) {
        print(paste('check for lat long at 0!! for ', iFile))
        vid_with_errors <- c(vid_with_errors, iFile)
        tacsatIntGearVEREF[!is.na(tacsatIntGearVEREF$SWEPT_AREA_KM2) & tacsatIntGearVEREF$SWEPT_AREA_KM2>100, c("SWEPT_AREA_KM2", "SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- NA
     }

     
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
   # Gear codes to keep (PELAGIC)
   gears2keep            <- c("OTM", "PTM", "PS")
   towedGears            <- c("OTM", "PTM")
   seineGears            <- c("PS")

   for (a_year in years){
    cat(paste(a_year, "\n"))
    cat(paste("Gridding", "\n"))

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
    
     # retrieve the harbour dep from FT_REF (LOCODE code for harb)
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


 if(FALSE){  # do not re-run...this takes ages!

    
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

  # Gear codes to keep ()
  gears2keep            <- c("PTM","OTM", "PS")
  towedGears            <- c("PTM","OTM")
  seineGears            <- c("PS")
 
 
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
     aggEffortAndFuelAlly <- aggEffortAndFuelAlly[aggEffortAndFuelAlly$VesselSize!="[0,15)",] # clean up
    
     save(aggEffortAndFuelAlly,file=file.path(outPath,  paste("AggregatedEffortAlly_PelagicFishing.RData", sep=""))) 
    
   
   
    
   
   
   
   
     ###----------------------
     ## do a ggplot 
     load(file=file.path(outPath,  paste("AggregatedEffortAlly_PelagicFishing.RData"))) # aggEffortAlly
    
    
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
       #geom_line(data=dd, aes(x=as.character(Year), y=litre_fuel/10, group=VesselSize, color=VesselSize),size=1 , linetype = "dashed", stat = "summary", fun = "sum") +   
       geom_line(data=dd, aes(x=as.character(Year), y=litre_fuel/100, group=1),size=1.2, color=1, linetype = "dashed", stat = "summary", fun = "sum") +   
        geom_line(data=dd, aes(x=as.character(Year), y=toteuros/100, group=1),size=1.2,  color=5, linetype = "dashed", stat = "summary", fun = "sum") +   
       scale_y_continuous(name = "fished hours effort; or fuel use (litres/100); or euros/100", sec.axis = sec_axis(~./2e4, name = "Nb Vessels") )+
       theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + 
       labs(x = "Year")     + 
       scale_color_manual(values=some_color_vessel_size, name="VesselSize") +  
       scale_fill_manual(values=some_color_vessel_size2) +
       guides(fill =guide_legend(ncol=1)) 
    print(p4)



# pel
#a_width <- 3000; a_height <- 2300
a_width <- 4000; a_height <- 2500   
 namefile <- paste0("barplot_and_ts_effort_nb_vessels_", years[1], "-", years[length(years)], "_PEL.tif")
 tiff(filename=file.path(getwd(), "outputs2020_pel", "output_plots",  namefile),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
print(p4)
dev.off()

