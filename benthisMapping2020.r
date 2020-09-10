
 outPath   <- "C:/BENTHIS/outputs/" 


#- Combine tacsatSweptArea files from 2010-2012
for(iYr in 2010:2012){
  #- read data for this year
  load(file.path(outPath, iYr, "tacsatSweptArea.RData")) 
  
  #- collate
  if(iYr == 2010) tacsatSweptAreaTot <- cbind(tacsatSweptArea,SI_YEAR=iYr)
  if(iYr != 2010) tacsatSweptAreaTot <- rbind(tacsatSweptAreaTot,cbind(tacsatSweptArea,SI_YEAR=iYr))
}
tacsatSweptArea <- tacsatSweptAreaTot; rm(tacsatSweptAreaTot)




 

#- Get outer ranges of latitude and longitude in dataset
xrange  <- range(tacsatSweptArea$SI_LONG,na.rm=T)
yrange  <- range(tacsatSweptArea$SI_LATI,na.rm=T)
xrange  <- c(floor(xrange[1]),ceiling(xrange[2]))
yrange  <- c(floor(yrange[1]),ceiling(yrange[2]))
print(xrange); print(yrange)

#- If xrange and yrange are inappropriate, set your own (rounded) xrange and yrange
if(FALSE){
  xrange  <- c(-10,20) # DEN
  yrange  <- c(50,66) # DEN
  # xrange  <- c(0,9)  # NLD
  # yrange  <- c(51,57) # NLD
  #xrange  <- c(-10,25) # SWE
  #yrange  <- c(50,66) # SWE
  # xrange  <- c(-10,20) # DEU
  #yrange  <- c(50,66) # DEU
 #xrange[1] <- floor(xrange[1]); xrange[2] <- ceiling(xrange[2])
 # yrange[1] <- floor(yrange[1]); yrange[2] <- ceiling(yrange[2])
}
                             
#- Set grid
resx    <- 1/60 #1 minute
resy    <- 1/60 #1 minute
grd     <- createGrid(xrange,yrange,resx=1/60,resy=1/60,type="SpatialGrid",exactBorder=T)

#- Grid all tacsatSweptArea data
#  Convert all tacsat poins first to SpatialPoints
coords                <- SpatialPoints(cbind(SI_LONG=tacsatSweptArea$SI_LONG,SI_LATI=tacsatSweptArea$SI_LATI))
idx                   <- over(coords,grd)
tacsatSweptArea$grID  <- idx

#- Remove records that are not in the study area 
tacsatSweptArea       <- subset(tacsatSweptArea,is.na(grID)==F)

#- Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
aggTacsatSweptArea    <- aggregate(tacsatSweptArea[,c("SWEPT_AREA_KM2",
                                                      "SWEPT_AREA_KM2_LOWER",
                                                      "SWEPT_AREA_KM2_UPPER")],
                                   by=list(tacsatSweptArea$LE_MET,tacsatSweptArea$grID,tacsatSweptArea$SI_YEAR),sum,na.rm=T)
colnames(aggTacsatSweptArea)[1:3] <- c("LE_MET","grID", "Year")

#- Add midpoint of gridcell to dataset
aggResult             <- cbind(aggTacsatSweptArea,CELL_LONG=coordinates(grd)[aggTacsatSweptArea$grID,1],
                                                  CELL_LATI=coordinates(grd)[aggTacsatSweptArea$grID,2])
save(aggResult,file=file.path(outPath,"AggregatedSweptArea.RData"))




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 outPath   <- "C:/BENTHIS/outputs/" 


#- Combine tacsatSweptArea files from 2005-2013
for(iYr in 2005:2013){
  #- read data for this year
  load(file.path(outPath, iYr, "tacsatSweptArea.RData")) 
  
  #- collate
  if(iYr == 2005) tacsatSweptAreaTot <- cbind(tacsatSweptArea,SI_YEAR=iYr)
  if(iYr != 2005) tacsatSweptAreaTot <- rbind(tacsatSweptAreaTot,cbind(tacsatSweptArea,SI_YEAR=iYr))
}
tacsatSweptArea <- tacsatSweptAreaTot; rm(tacsatSweptAreaTot)




#- Get outer ranges of latitude and longitude in dataset
xrange  <- range(tacsatSweptArea$SI_LONG,na.rm=T)
yrange  <- range(tacsatSweptArea$SI_LATI,na.rm=T)
xrange  <- c(floor(xrange[1]),ceiling(xrange[2]))
yrange  <- c(floor(yrange[1]),ceiling(yrange[2]))
print(xrange); print(yrange)

#- add months and days
tacsatSweptArea$SI_DATE <- as.POSIXct(paste(tacsatSweptArea$SI_DATE,    sep=" "), tz="GMT", format="%d/%m/%Y")
tacsatSweptArea$MONTH   <- format(tacsatSweptArea$SI_DATE, "%m") 
tacsatSweptArea$DAY     <- format(tacsatSweptArea$SI_DATE, "%j")  

#- Get outer ranges of latitude and longitude in dataset
xrange <- range(tacsatSweptArea$SI_LONG,na.rm=T)
yrange <- range(tacsatSweptArea$SI_LATI,na.rm=T)
xrange <- c(floor(xrange[1]),ceiling(xrange[2]))
yrange <- c(floor(yrange[1]),ceiling(yrange[2]))
print(xrange); print(yrange)

#- If xrange and yrange are inappropriate, set your own (rounded) xrange and yrange
if(FALSE){
xrange <- c(-10,20) # DEN
yrange <- c(40,66) # DEN
# xrange <- c(0,9) # NLD
# yrange <- c(51,57) # NLD
xrange[1] <- floor(xrange[1]); xrange[2] <- ceiling(xrange[2])
yrange[1] <- floor(yrange[1]); yrange[2] <- ceiling(yrange[2])
}

#- Set grid
resx <- 1/60 #1 minute
resy <- 1/60 #1 minute
grd <- createGrid(xrange,yrange,resx=1/60,resy=1/60,type="SpatialGrid",exactBorder=T)

#- Grid all tacsatSweptArea data
# Convert all tacsat poins first to SpatialPoints
coords <- SpatialPoints(cbind(SI_LONG=tacsatSweptArea$SI_LONG,SI_LATI=tacsatSweptArea$SI_LATI))
idx <- over(coords,grd)
tacsatSweptArea$grID <- idx

#- Remove records that are not in the study area
tacsatSweptArea <- subset(tacsatSweptArea,is.na(grID)==F)

#-1 Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
aggTacsatSweptArea <- aggregate(tacsatSweptArea[,c("SWEPT_AREA_KM2",
"SWEPT_AREA_KM2_LOWER",
"SWEPT_AREA_KM2_UPPER")],
by=list(tacsatSweptArea$LE_MET,tacsatSweptArea$grID,tacsatSweptArea$SI_YEAR),sum,na.rm=T)
colnames(aggTacsatSweptArea)[1:3] <- c("LE_MET","grID", "Year")

#- Add midpoint of gridcell to dataset
aggResult <- cbind(aggTacsatSweptArea,CELL_LONG=coordinates(grd)[aggTacsatSweptArea$grID,1],
CELL_LATI=coordinates(grd)[aggTacsatSweptArea$grID,2])
save(aggResult,file=file.path(outPath,"DEN_2005_2013_AggregatedSweptArea.RData"))


#-2 Aggregate the results by metier and grid ID and BY MONTH(aggregate() can be slow: be patient)
aggTacsatSweptArea2 <- aggregate(tacsatSweptArea[,c("SWEPT_AREA_KM2",
"SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")],
by=list(tacsatSweptArea$LE_MET,tacsatSweptArea$MONTH,tacsatSweptArea$grID,tacsatSweptArea$SI_YEAR),sum,na.rm=T)
colnames(aggTacsatSweptArea2)[1:4] <- c("LE_MET","MONTH", "grID", "Year")

#- Add midpoint of gridcell to dataset
aggResult <- cbind(aggTacsatSweptArea2,CELL_LONG=coordinates(grd)[aggTacsatSweptArea2$grID,1],
CELL_LATI=coordinates(grd)[aggTacsatSweptArea2$grID,2])
save(aggResult,file=file.path(outPath,"DEN_2005_2013_AggregatedSweptAreaMonth.RData"))


#-3 Aggregate the results by metier and grid ID and BY DAY(aggregate() can be slow: be patient)
aggTacsatSweptArea3 <- aggregate(tacsatSweptArea[,c("SWEPT_AREA_KM2",
"SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")],
by=list(tacsatSweptArea$LE_MET, tacsatSweptArea$DAY, tacsatSweptArea$grID,tacsatSweptArea$SI_YEAR),sum,na.rm=T)
colnames(aggTacsatSweptArea3)[1:4] <- c("LE_MET","DAY", "grID", "Year")

#- Add midpoint of gridcell to dataset
aggResult <- cbind(aggTacsatSweptArea3, CELL_LONG=coordinates(grd)[aggTacsatSweptArea3$grID,1],
CELL_LATI=coordinates(grd)[aggTacsatSweptArea3$grID,2])
save(aggResult,file=file.path(outPath,"DEN_2005_2013_AggregatedSweptAreaDay.RData"))





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 

  # 2010-2012
  load("C:\\BENTHIS\\outputs\\SWE_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\DEU_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\DEN_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\UK_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\IRL_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\BEL_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\NLD_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\SCO_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\MED1_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\MED2_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\PRT_AggregatedSweptArea.RData")
  load("C:\\BENTHIS\\outputs\\GRK_AggregatedSweptArea.RData")   # only one metier: OT_MIX
  #load("C:\\BENTHIS\\outputs\\DEN_2005_2013_AggregatedSweptArea.RData")  
  load("C:\\BENTHIS\\outputs\\UKNI_AggregatedSweptArea.RData")   # Northern Ireland
  load("C:\\BENTHIS\\outputs\\NOR_AggregatedSweptArea.RData")   
  load("C:\\BENTHIS\\outputs\\SPN_AggregatedSweptArea.RData")   
  load("C:\\BENTHIS\\outputs\\ITA_AggregatedSweptArea.RData") # rapido TBB
 
  xrange  <- c(-10,20) # DEN
  yrange  <- c(50,66) # DEN
  xrange  <- c(-10,25) # SWE
  yrange  <- c(50,66) # SWE
  xrange  <- c(-10,20) # DEU
  yrange  <- c(50,66) # DEU
  xrange  <- c(-20,10) # UK
  yrange  <- c(45,67) # UK
  xrange  <- c(-20,10) # IRL
  yrange  <- c(45,67) # IRL
  xrange  <- c(-20,10)  # BEL
  yrange  <- c(45,67) # BEL
  xrange  <- c(0,9)  # NLD
  yrange  <- c(51,57) # NLD
  xrange  <- c(-20,10)  # SCO
  yrange  <- c(45,67) # SCO
  xrange  <- c(1,30)  # MED
  yrange  <- c(30,50) # MED
  xrange  <- c(-15,10)  # PRT
  yrange  <- c(30,45) # PRT
  xrange  <- c(1,30)  # GRK
  yrange  <- c(30,50) # GRK
  xrange  <- c(-20,10) # UKNI
  yrange  <- c(45,67) # UKNI
  xrange  <- c(-1,30) # NOR
  yrange  <- c(55,80) # NOR
   xrange  <- c(-15,10)  # SPN
  yrange  <- c(30,45) # SPN

   

#- Plot all of it
library(RColorBrewer)
colintens             <- brewer.pal(9,"YlOrRd")


#- Create polygon set of gridcells to plot (takes a bit longer)
uniqueCells           <- aggResult[!duplicated(aggResult$grID),c("grID","CELL_LONG","CELL_LATI")]
resx                  <- 1/60 #1 minute
resy                  <- 1/60 #1 minute
grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2), 
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
#- Add any subset to plot: First, combine all metiers over all years
 subAggResult          <- aggResult
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)

 #save(data2plot,file=file.path(outPath,"IRL_agg3y_data2plot.RData"))


colnames(data2plot)[1]<- "grID"
idx                   <- match(uniqueCells$grID, data2plot$grID)
#col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,0.5,0.75,1,1.5,2, 10000))]
col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]


# do the plot
#plot(1,1,col="white",xlim=xrange,ylim=yrange,asp=1/lonLatRatio(mean(xrange),mean(yrange)),
#plot(1,1,col="white", xlim=c(-1,19), ylim=c(51, 63),asp=1/lonLatRatio(mean(xrange),mean(yrange)),
#     xlab="Longitude",ylab="Latitude",las=1)
plot(1,1,col="white", xlim=xrange, ylim=yrange,asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)
plot(grdc2plot,add=TRUE,col=col2plot, border=0)   # sp:::plot.SpatialPolygons
polPath   <- file.path("C:","BENTHIS", "BalanceMaps")
#sh1 <- readShapePoly(file.path(polPath,"francois_EU"),  proj4string=CRS("+proj=longlat +datum=WGS84"))
#plot(sh1, add=TRUE, col=grey(0.7))
map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
box()
# save in tiff


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## BIND ALL COUNTRIES IN ONE BIG DATA FRAME
 aggResultAll <- NULL
 #countries <- c('SWE', 'DEU', 'DEN', 'UK', 'IRL', 'BEL', 'NLD', 'SCO', 'MED1', 'MED2', 'PRT','GRK', 'UKNI', 'NOR')
 #countries <- c('SWE', 'DEU', 'DEN', 'UK', 'IRL', 'BEL', 'NLD', 'MED1', 'MED2', 'PRT', 'GRK', 'NOR', 'SPN')
 countries <- c('SWE', 'DEU', 'DEN', 'UK', 'IRL', 'BEL', 'NLD', 'MED1', 'MED2', 'PRT', 'GRK', 'NOR', 'ITA') # + rapido ITA TBB
 for(ctry in countries){
  load(file=file.path("C:","BENTHIS", "outputs", paste(ctry,"_AggregatedSweptArea.RData",sep='')))
  
  if(ctry=="NOR")  {
    dd        <- aggResult[aggResult$LE_MET=="SSC_DEM" & aggResult$Year=="2011",]
    dd$Year   <- "2010"
    aggResult <- rbind.data.frame(aggResult, dd) # assume copy/paste for seiners to fill in the gap of 2010
    }
  if(ctry=="ITA")  {
    colnames(aggResult) <- c("LE_MET","grID","Year","SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER","SWEPT_AREA_KM2_UPPER","CELL_LONG","CELL_LATI")
  }  
  
  aggResultAll <- rbind.data.frame(aggResultAll, cbind.data.frame(aggResult, ctry=ctry) )
 }
 colnames(aggResult)[1:3] <- c("LE_MET","grID", "Year")
 aggResult <- aggResultAll 
 #save(aggResult, file=file.path(outPath,paste(paste(countries, collapse="_"), "_AggregatedSweptArea_2105015.RData", sep=''))) 
 #save(aggResult, file=file.path(outPath,paste(paste(countries, collapse="_"), "_AggregatedSweptArea_1006015.RData", sep=''))) 
 save(aggResult, file=file.path(outPath,paste(paste(countries, collapse="_"), "_AggregatedSweptArea_1206015.RData", sep='')))  # + rapido TBB ITA


 #xrange  <- c(-20,20) # ALL
 #yrange  <- c(45,70) # ALL
 xrange  <- c(-30,50) # ALL
 yrange  <- c(30,81) # ALL
 
 #- Set grid
 resx    <- 1/60 #1 minute
 resy    <- 1/60 #1 minute
 grd     <- createGrid(xrange,yrange,resx=1/60,resy=1/60,type="SpatialGrid",exactBorder=T)

 #- Grid all tacsatSweptArea data
 #  Convert all tacsat poins first to SpatialPoints
 coords                <- SpatialPoints(cbind(SI_LONG=aggResult$CELL_LONG, SI_LATI=aggResult$CELL_LATI))
 idx                   <- over(coords,grd)
 aggResult$grID        <- idx

 #- Remove records that are not in the study area 
 aggResult       <- subset(aggResult,is.na(grID)==F)

 #- Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
 aggResult    <- aggregate(aggResult[,c("SWEPT_AREA_KM2",
                                                      "SWEPT_AREA_KM2_LOWER",
                                                      "SWEPT_AREA_KM2_UPPER")],
                                   by=list(aggResult$LE_MET, aggResult$grID, aggResult$Year),sum,na.rm=T)
 colnames(aggResult)[1:3] <- c("LE_MET","grID", "Year")

 #- Add midpoint of gridcell to dataset
 aggResult             <- cbind(aggResult,CELL_LONG=coordinates(grd)[aggResult$grID,1],
                                                  CELL_LATI=coordinates(grd)[aggResult$grID,2])
 #save(aggResult,file=file.path(outPath,"ALL_AggregatedSweptArea_21052015.RData"))
 #save(aggResult,file=file.path(outPath,"ALL_AggregatedSweptArea_10062015.RData"))

 #save(aggResult,file=file.path(outPath,"ALL_AggregatedSweptArea_12062015.RData"))  # + rapido TBB ITA 
 load(file=file.path(outPath,"ALL_AggregatedSweptArea_12062015.RData")) # aggResult


 # send to ArcGIS
 if(FALSE){
    aggResult2 <- aggResult
    aggResult2$cell_area <- (cos(aggResult2$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
    aggResult2[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
                  aggResult2[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  aggResult2$cell_area # standardize
  write.table(aggResult2, file=file.path(outPath,"aggResult2GIS_03102016.txt"), row.names=FALSE, col.names=TRUE, sep=";")

  # caution: replace levels to simplify the naming of metiers in 4 categories
  aggResult2$LE_MET2 <- factor(aggResult2$LE_MET) # init
  levels(aggResult2$LE_MET2) <-  c("Otter Trawl", "Otter Trawl", "Otter Trawl", "Otter Trawl", "Seine",
                                 "Beam Trawl", "Beam Trawl", "NA", "Dredge",
                                 "Otter Trawl", "Otter Trawl", "Seine", "Otter Trawl",  "Seine", "Otter Trawl",  "Beam Trawl", "Seine", "Otter Trawl","Otter Trawl", "Not relevant",
                                  "Beam Trawl")
  aggResult2_Otter          <- aggResult2 [aggResult2$LE_MET2 %in% "Otter Trawl", ]
  write.table(aggResult2_Otter, file=file.path(outPath,"aggResultOtter2GIS_03102016.txt"), row.names=FALSE, col.names=TRUE, sep=";")
  aggResult2_Seine          <- aggResult2 [aggResult2$LE_MET2 %in% "Seine", ]
  write.table(aggResult2_Seine, file=file.path(outPath,"aggResultSeine2GIS_03102016.txt"), row.names=FALSE, col.names=TRUE, sep=";")
  aggResult2_Beam          <- aggResult2 [aggResult2$LE_MET2 %in% "Beam Trawl", ]
  write.table(aggResult2_Beam, file=file.path(outPath,"aggResultBeam2GIS_03102016.txt"), row.names=FALSE, col.names=TRUE, sep=";")
  aggResult2_Dredge          <- aggResult2 [aggResult2$LE_MET2 %in% "Dredge", ]
  write.table(aggResult2_Dredge, file=file.path(outPath,"aggResultDredge2GIS_03102016.txt"), row.names=FALSE, col.names=TRUE, sep=";")

 
  # compute the subsurface swept area
  matprop <- read.table(file=file.path("C:", "BENTHIS", "data_gear_spec_questionnaire",
                           "Subsurface_proportion_by_metier_Figure10_Eigaard_et_al.csv"),
                           header=TRUE, sep=";")
   #=> small discrepencies in naming the metier between the paper and the BENTHIS workflow corrected by hand. (e.g. OT_MIX_NEP=>OT_MIX_CRU, etc.)                        
                           
  rownames(matprop)            <- matprop$LE_MET
  aggResult2$multiplier         <- factor(aggResult2$LE_MET)
  levels(aggResult2$multiplier) <- matprop[levels(aggResult2$multiplier), "Subsurface"]
 
  aggResult2_Subsurface <- aggResult2[c("SWEPT_AREA_KM2", "CELL_LONG", "CELL_LATI")]
  aggResult2_Subsurface[,c("SWEPT_AREA_KM2")] <- 
    aggResult2_Subsurface[,c("SWEPT_AREA_KM2")] * as.numeric(as.character(aggResult2$multiplier))/100
  write.table(aggResult2_Subsurface, file=file.path(outPath,"aggResultSubsurface2GIS.txt"), row.names=FALSE, col.names=TRUE, sep=";")

 
 
  ArcGIS 10.1 Workflow for producing the BENTHIS WP2 map final polishing:
1-	Import XY AggResult.txt into ArcGIS in Layers>Add Data, click on the imported data and define X, Y and Z field, and define basic World projection WGS84 (put this system in Favorite). [bug: You might encounter some trouble if not integer values in the Z column i.e. <Null> fields...then round the values before importing]
2-	 Import  ne_50m_ocean.shp – [optional: draw a rectangle to clip in Custumize>toolbars>draw and save the rectangle as .shp by clicking right on Layers in the Table of Contents and then >Convert  Graphics to Features. Then in ArcToolbox>Clip...clip the ocean with the rectangle shape]
3-	Geoprocessing> ArcToolBox >Conversion Tools>to Raster> Point to Raster for rasterizing the XY layer e.g. with 0.0167 degree i.e. 1 minute
4-	In Data Management Tools, reprojection of the raster layer with projection>Raster>project and use the EEA proj definition (Lambert Azimuthal Equal Area, - put this system in Favorite)
5-	In Data Management Tools, reprojection of the feature shape layer with projection>Feature> project and use the EEA proj definition (Lambert Azimuthal Equal Area)
6-	Define the coordinate system of the Layers to be the EEA projection by clicking right on Layers.
7-	For a given layer>properties, define classification for the symbolisation e.g. streched and manual breaks...
8-	Add a graticule (will be only visible in layout mode) in Layers>properties>grid – close and reedit the graticule to adapt.
9-	Add a legend (in layout mode) with menu>Insert>legend – dont forget to look into “style”
10-	T copy/paste the symbology from one layer to the next use the ArcTools>data management tools>Layers and table views>Apply symbology from layers
11-	For layers with labels, click right on the layer and click on Label Features. To edit the lable use menu>Custumize>Toolbars>Label>Managing labels 
12-	Export a tif with menu>File>export map in lwz compression and in 500 dpi
13-	To re-apply symbology of a raster from an external project, first save the raster layer as a layer file and then import the symbology from the layer i.e. to save as a layer file right click on the layer in the table of contents and select 'save as layer file'.To import the symbology click on the open folder in the raster properties dialog. 


Layer query is:
"Countrynam" = 'Belgium' OR  "Countrynam" = 'DE' OR "Countrynam" = 'Denmark' OR "Countrynam" = 'Faroe Islands' OR "Countrynam" = 'Greece' OR "Countrynam" = 'Ireland' OR "Countrynam" = 'Isle of Man' OR "Countrynam" = 'Italy' OR "Countrynam" = 'Netherlands' OR "Countrynam" = 'Norway' OR "Countrynam" = 'Portugal' OR "Countrynam" = 'Sweden' OR "Countrynam" = 'United Kingdom'
Color scale
 R0 G132 B168 – R142 G189 B181 – R255 G255 B191 – R 222 G130 B80 – R168 G0 B0

To do some operations from raster layerss then we need the Spatial Analyst Tools extension:
1-	Enable it by ticking the box in Custumize>Extensions (!)
2-	Use it in Geoprocessing>ArcToolBox>Spatial AnalystTools>Map Algebra> Raster calculator e.g. (100*(sce1/baseline) )-100


# BUT: could be better to do it in R given it fails in ArcGIS when too memory demanding and rasterize fails!!!
 # then rasterize in R before loading in GIS
 
 
library(raster)
xrange      <- range(aggResult2$CELL_LONG, na.rm=TRUE) # ALL
yrange      <- range(aggResult2$CELL_LATI, na.rm=TRUE)  # ALL
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2$CELL_LONG, SI_LATI=aggResult2$CELL_LATI))
# caution: because three years then we need to divide estimates by 3 
rstr        <- rasterize(x=some_coords, y=r, field=aggResult2$SWEPT_AREA_KM2/3, fun="sum") 
plot(rstr, xlim=c(-5,13), ylim=c(53,60))
#a           <- area(rstr)
#rstr_std    <- rstr/a
# equivalent to:
# tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
# tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
#                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
rstr_eea[is.na(rstr_eea)] <- -999
rstr_eea[rstr_eea<0.001]  <- -999
writeRaster(rstr_eea, file.path(outPath,"AnnualAgg2RasterEEA"), format = "GTiff")


library(raster)
xrange      <- range(aggResult2_Subsurface$CELL_LONG, na.rm=TRUE) # ALL
yrange      <- range(aggResult2_Subsurface$CELL_LATI, na.rm=TRUE)  # ALL
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Subsurface$CELL_LONG, SI_LATI=aggResult2_Subsurface$CELL_LATI))
# caution: because three years then we need to divide estimates by 3 
rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Subsurface$SWEPT_AREA_KM2/3, fun="sum") 
plot(rstr, xlim=c(-5,13), ylim=c(53,60))
#a           <- area(rstr)
#rstr_std    <- rstr/a
# equivalent to:
# tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
# tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
#                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
rstr_eea[is.na(rstr_eea)] <- -999
rstr_eea[rstr_eea<0.001]  <- -999
writeRaster(rstr_eea, file.path(outPath,"AnnualAggSub2RasterEEA"), format = "GTiff")






# for otter trawl
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Otter$CELL_LONG, SI_LATI=aggResult2_Otter$CELL_LATI))
# caution: because three years then we need to divide estimates by 3 
rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Otter$SWEPT_AREA_KM2/3, fun="sum") 
plot(rstr, xlim=c(-5,13), ylim=c(53,60))
#a           <- area(rstr)
#rstr_std    <- rstr/a
# equivalent to:
# tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
# tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
#                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
library(rgdal)
rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
rstr_eea[is.na(rstr_eea)] <- -999
rstr_eea[rstr_eea<0.001]  <- -999
writeRaster(rstr_eea, file.path(outPath,"AnnualRstOttertEEA"), format = "GTiff")

# for seine
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Seine$CELL_LONG, SI_LATI=aggResult2_Seine$CELL_LATI))
# caution: because three years then we need to divide estimates by 3 
rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Seine$SWEPT_AREA_KM2/3, fun="sum") 
plot(rstr, xlim=c(-5,13), ylim=c(53,60))
#a           <- area(rstr)
#rstr_std    <- rstr/a
# equivalent to:
# tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
# tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
#                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
library(rgdal)
rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
rstr_eea[is.na(rstr_eea)] <- -999
rstr_eea[rstr_eea<0.001]  <- -999
writeRaster(rstr_eea, file.path(outPath,"AnnualAggRstSeineEEA"), format = "GTiff")


# for beam trawl
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Beam$CELL_LONG, SI_LATI=aggResult2_Beam$CELL_LATI))
# caution: because three years then we need to divide estimates by 3 
rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Beam$SWEPT_AREA_KM2/3, fun="sum") 
plot(rstr, xlim=c(-5,13), ylim=c(53,60))
#a           <- area(rstr)
#rstr_std    <- rstr/a
# equivalent to:
# tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
# tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
#                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
library(rgdal)
rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
rstr_eea[is.na(rstr_eea)] <- -999
rstr_eea[rstr_eea<0.001]  <- -999
writeRaster(rstr_eea, file.path(outPath,"AnnualAggRstBeamEEA"), format = "GTiff")

# for dredge
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Dredge$CELL_LONG, SI_LATI=aggResult2_Dredge$CELL_LATI))
# caution: because three years then we need to divide estimates by 3 
rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Dredge$SWEPT_AREA_KM2/3, fun="sum") 
plot(rstr, xlim=c(-5,13), ylim=c(53,60))
#a           <- area(rstr)
#rstr_std    <- rstr/a
# equivalent to:
# tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
# tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
#                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
library(rgdal)
rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
rstr_eea[is.na(rstr_eea)] <- -999
rstr_eea[rstr_eea<0.001]  <- -999
writeRaster(rstr_eea, file.path(outPath,"AnnualAggRstDredgeEEA"), format = "GTiff")



 
 } else{
 # plot in R! (below)
 }
 



 #- Plot all of it
 library(RColorBrewer)
 colintens             <- brewer.pal(9,"YlOrRd")


 #- Create polygon set of gridcells to plot (takes a bit longer)
 uniqueCells           <- aggResult[!duplicated(aggResult$grID),c("grID","CELL_LONG","CELL_LATI")]
 resx                  <- 1/60 #1 minute
 resy                  <- 1/60 #1 minute
 grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2), 
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
 save(grdc2plot,file=file.path(outPath,"ALL_grdc2plot_13May15.RData"))
 #load(file.path(outPath,"ALL_grdc2plot.RData")) # get "grdc2plot"
 
 # match to grid cell area in km2 and standardize the swept areas
 # to correct for the latitude bias (the longitude goes to 0 when direction to North pole) eg a cell of 1 by 1 minute is:
   # (1/60)*78.847*(1/60)*111 =  2.431116 km^2  at 45 degree in lat
   # (1/60)*28.902*(1/60)*111 =  0.891145 km^2  at 75 degree in lat
   # Length of a degree of longitude = cos (latitude) * 111.325 kilometers 
   
 aggResult$cell_area <- (cos(aggResult$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
 aggResult[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
                  aggResult[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  aggResult$cell_area # standardize
 
 #- Add any subset to plot: First, combine all metiers over all years
 subAggResult          <- aggResult
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)

 
 

 colnames(data2plot)[1]<- "grID"
 idx                   <- match(uniqueCells$grID, data2plot$grID)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]


 # do the plot
 tiff(filename=file.path(outPath, paste("ALL", ".tiff", sep="" )),
                                   width = 3400, height = 3400, 
                                   units = "px", pointsize = 12,  res=500)

#xrange <- c(6,11)
#yrange <- c(56,58)
 plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)
 plot(grdc2plot,add=TRUE,col=col2plot, border=0)   # sp:::plot.SpatialPolygons
 polPath   <- file.path("C:","BENTHIS", "BalanceMaps")
 #sh1 <- readShapePoly(file.path(polPath,"francois_EU"),  proj4string=CRS("+proj=longlat +datum=WGS84"))
 #plot(sh1, add=TRUE, col=grey(0.7))
   map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)


 # save in tiff
 dev.off()
 

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##




 # DO AN AGGREGATION FROM MONTHLY DATA


  outPath   <- "C:/BENTHIS/outputs/"


  # 2010-2012
  #load("C:\\BENTHIS\\outputs\\DEN_AggregatedSweptAreaMonth.RData")
  #load("C:\\BENTHIS\\outputs\\UK_AggregatedSweptAreaMonth.RData")
  #load("C:\\BENTHIS\\outputs\\BEL_AggregatedSweptAreaMonth.RData")
  #load("C:\\BENTHIS\\outputs\\NLD_AggregatedSweptAreaMonth.RData")
  #load("C:\\BENTHIS\\outputs\\DEU_AggregatedSweptAreaMonth.RData")


 aggResultAll <- NULL
 #countries <- c('SWE', 'DEN', 'UK', 'IRL','BEL', 'NLD', 'DEU')
 countries <- c( 'DEU', 'DEN', 'UK', 'BEL', 'NLD')
 for(ctry in countries){
    load(file=file.path("C:","BENTHIS", "outputs", paste(ctry,"_AggregatedSweptAreaMonth.RData",sep='')))
    aggResultAll <- rbind.data.frame(aggResultAll, cbind.data.frame(aggResult, ctry=ctry) )
 }
 colnames(aggResult)[1:3]  <- c("LE_MET","grID", "Year")
 aggResult                 <- aggResultAll
 save(aggResult, file=file.path(outPath,paste(paste(countries, collapse="_"), "_AggregatedSweptAreaMonth_19092017.RData", sep='')))




 xrange  <- c(-30,50) # ALL
 yrange  <- c(30,81)  # ALL

 #- Set grid
 resx    <- 1/60  #1 minute
 resy    <- 1/60  #1 minute
 library(vmstools)
 grd     <- createGrid(xrange,yrange,resx=1/60,resy=1/60,type="SpatialGrid",exactBorder=T)


 # Grid all tacsatSweptArea data
 # Convert all tacsat poins first to SpatialPoints
 coords                <- SpatialPoints(cbind(SI_LONG=aggResult$CELL_LONG, SI_LATI=aggResult$CELL_LATI))
 idx                   <- over(coords,grd)
 aggResult$grID        <- idx


 # Remove records that are not in the study area
 aggResult       <- subset(aggResult,is.na(grID)==F)


 #- Aggregate the results by metier/or month and grid ID (aggregate() can be slow: be patient)
 aggResult1      <- aggregate(aggResult[,c("SWEPT_AREA_KM2",
                                         "SWEPT_AREA_KM2_LOWER",
                                         "SWEPT_AREA_KM2_UPPER")],
                                   by=list(aggResult$MONTH, aggResult$grID, aggResult$Year, aggResult$CELL_LONG, aggResult$CELL_LATI),mean,na.rm=T)
 colnames(aggResult1)[1:5] <- c("MONTH","grID", "Year", "CELL_LONG", "CELL_LATI")

 save(aggResult1, file=file.path(outPath,"ALL_AggregatedSweptAreaMonth_19092017.RData"))



 # subset to German EEZ North Sea--------
 library(maptools)
 #https://gis.stackexchange.com/questions/63793/how-to-overlay-a-polygon-over-spatialpointsdataframe-and-preserving-the-spdf-dat

 ger_eez         <- readShapePoly(file.path('C:','BENTHIS','MapWithArcGIS','GermanEEZ_NorthSea','GermanEEZ_WGS84','GermanEEZ_WGS84', 'GermanEEZ_polygon_WGS84'),
                      proj4string=CRS("+proj=longlat +ellps=WGS84"))
 # convert the point data
 #coords                <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(aggResult1$CELL_LONG)), y=as.numeric(as.character(aggResult1$CELL_LATI))), proj4string = CRS("+proj=longlat +ellps=WGS84"), data=data.frame(dat=rep(0, nrow(aggResult1))))
 coords                <- SpatialPoints(cbind(x=as.numeric(as.character(aggResult1$CELL_LONG)), y=as.numeric(as.character(aggResult1$CELL_LATI))), proj4string = CRS("+proj=longlat +ellps=WGS84"))
 idx                   <- over(coords, as(ger_eez, "SpatialPolygons"))
 aggResult2            <- aggResult1 [!is.na(idx),]

 save(aggResult2, file=file.path(outPath,"ALL_AggregatedSweptAreaMonth_19092017_GermanNorthSeaEEZ.RData"))  #=> TO DARIO


 #---------
 #---------
 #---------
 # CHECK WITH A PLOT
 # plot all of it
 library(RColorBrewer)
 colintens             <- brewer.pal(9,"YlOrRd")
 # create polygon set of gridcells to plot
 uniqueCells           <- aggResult2[!duplicated(aggResult2$grID), c("grID","CELL_LONG","CELL_LATI")]

 # add any subset to plot: First, combine all metiers over all years
 subAggResult          <- aggResult2

 grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2),
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))



 colnames(data2plot)[1]<- "grID"
 colnames(data2plot)[2]<- "SWEPT_AREA_KM2"
 idx                   <- match(uniqueCells$grID, data2plot$grID)

 the_breaks            <- c(-1,0,0.25,0.5,1,2,8,16, 10000)
 a_unit                <- 1
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2")], by=list(subAggResult$grID), FUN=sum, na.rm=T)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"], breaks=the_breaks*a_unit)]

 # do the plot
 xrange  <- c(3,9)     # German EEZ in NS
 yrange  <- c(53.5,56) # German EEZ in NS

 pdf( file=file.path(outPath,"ALL_AggregatedSweptAreaMonth_19092017_GermanNorthSeaEEZ.pdf"))

 plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange), mean(yrange)),
     xlab="Longitude", ylab="Latitude",las=1)

 plot(grdc2plot,add=TRUE,col=col2plot, border=0)   # sp:::plot.SpatialPolygons
 library(mapdata)
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 plot(ger_eez, add=TRUE, border=2)
 box()
 dev.off()





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 ## find pressure per metier on Andy´s stations


    # great circle distance
  `distance` <- function(lon,lat,lonRef,latRef){
                    x1 <- lon
                    y1 <- lat
                    x2 <- lonRef
                    y2 <- latRef

                    pd <- pi/180

                    a1<- sin(((y2-y1)*pd)/2)
                    a2<- cos(y1*pd)
                    a3<- cos(y2*pd)
                    a4<- sin(((x2-x1)*pd)/2)
                    a <- a1*a1+a2*a3*a4*a4

                                      c <- 2*atan2(sqrt(a),sqrt(1-a));
                                      R <- 6371;
                                      dx1 <- R*c
                    return(dx1)}
  


 

 ### EPIFAUNA STATIONS-----------------------------------------------------
 load(file=file.path(outPath,"ALL_AggregatedSweptArea.RData")) # aggResult

  # match to grid cell area in km2 and standardize the swept areas
  # to correct for the latitude bias (the longitude goes to 0 when direction to North pole) eg a cell of 1 by 1 minute is:
   # (1/60)*78.847*(1/60)*111 =  2.431116 km^2  at 45 degree in lat
   # (1/60)*28.902*(1/60)*111 =  0.891145 km^2  at 75 degree in lat
   # Length of a degree of longitude = cos (latitude) * 111.325 kilometers, see http://www.ncgia.ucsb.edu/giscc/units/u014/u014.html
 aggResult$cell_area <- (cos(aggResult$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
 aggResult[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
                  aggResult[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  aggResult$cell_area # standardize


 stations          <- read.table(file=file.path(outPath, 'CEFAS_StefanBolam_Epifaunal stn positions_for_WP2.csv'), sep=';', header=TRUE)
 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))
 
 stations_and_pressure <- list()
 for(year in c(2010:2012)){
    print(year)
    aggResultThisYear <- aggResult[aggResult$Year==year,] 
     for(metier in unique(aggResultThisYear$LE_MET)){
      print(metier)
      aggResultThisYearThisMet <- aggResultThisYear[aggResultThisYear$LE_MET==metier,] 
      for (iStat in 1: nrow(stations)){  # brute force
         cat(paste(iStat, "\n"))
         latRef <- as.numeric(as.character(stations[iStat,'Lat'])) 
         lonRef <- as.numeric(as.character(stations[iStat,'Long']))
         dists  <- distance (aggResultThisYearThisMet$CELL_LONG, aggResultThisYearThisMet$CELL_LATI, lonRef, latRef)
         stations[iStat,"grID"]     <-  aggResultThisYearThisMet[which.min(dists), "grID"]
         stations[iStat,"distance"] <-  dists[which.min(dists)]
         }
     stations_and_pressure[[paste(year,"_",metier,sep='')]] <- merge(stations, aggResultThisYearThisMet)
     }
 } 
 sauv <-  stations_and_pressure
 stations_and_pressure  <- do.call("rbind", stations_and_pressure)
 stations_and_pressure  <- orderBy(~ source+stn.code+LE_MET, stations_and_pressure)
 head(stations_and_pressure, 20)
 
 save(stations_and_pressure, file=file.path(outPath,"stations_epifauna_and_pressure_09012015.RData"))

 write.table(stations_and_pressure, file=file.path(outPath,"stations_epifauna_and_pressure_09012015.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)


 # plot to see which stations are acceptable given the data so far
 plot(stations_and_pressure$Long, stations_and_pressure$Lat, pch="+", col="green")
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$distance>2,]$Long, stations_and_pressure[stations_and_pressure$distance>2,]$Lat, pch="+", col="red")



 ### INFAUNA STATIONS-----------------------------------------------------
 load(file=file.path(outPath,"ALL_AggregatedSweptArea.RData")) # aggResult

 # match to grid cell area in km2 and standardize the swept areas
  # to correct for the latitude bias (the longitude goes to 0 when direction to North pole) eg a cell of 1 by 1 minute is:
   # (1/60)*78.847*(1/60)*111 =  2.431116 km^2  at 45 degree in lat
   # (1/60)*28.902*(1/60)*111 =  0.891145 km^2  at 75 degree in lat
   # Length of a degree of longitude = cos (latitude) * 111.325 kilometers, see http://www.ncgia.ucsb.edu/giscc/units/u014/u014.html
 aggResult$cell_area <- (cos(aggResult$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
 aggResult[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
                  aggResult[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  aggResult$cell_area # standardize

 stations          <- read.table(file=file.path(outPath, 'CEFAS_StefanBolam_Infauna stn positions_for_WP2.csv'), sep=';', header=TRUE)
 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))
 
 stations_and_pressure <- list()
 for(year in c(2010:2012)){
    print(year)
    aggResultThisYear <- aggResult[aggResult$Year==year,] 
     for(metier in unique(aggResultThisYear$LE_MET)){
      print(metier)
      aggResultThisYearThisMet <- aggResultThisYear[aggResultThisYear$LE_MET==metier,] 
      for (iStat in 1: nrow(stations)){  # brute force
         cat(paste(iStat, "\n"))
         latRef <- as.numeric(as.character(stations[iStat,'Lat'])) 
         lonRef <- as.numeric(as.character(stations[iStat,'Long']))
         dists  <- distance (aggResultThisYearThisMet$CELL_LONG, aggResultThisYearThisMet$CELL_LATI, lonRef, latRef)
         stations[iStat,"grID"]     <-  aggResultThisYearThisMet[which.min(dists), "grID"]
         stations[iStat,"distance"] <-  dists[which.min(dists)]
         }
     stations_and_pressure[[paste(year,"_",metier,sep='')]] <- merge(stations, aggResultThisYearThisMet)
     }
 } 
 sauv <-  stations_and_pressure
 stations_and_pressure  <- do.call("rbind", stations_and_pressure)
 stations_and_pressure  <- orderBy(~ source+station_name+LE_MET, stations_and_pressure)
 head(stations_and_pressure, 20)

 
 save(stations_and_pressure, file=file.path(outPath,"stations_infauna_and_pressure_09012015.RData"))

 write.table(stations_and_pressure, file=file.path(outPath,"stations_infauna_and_pressure_09012015.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)


 # plot to see which stations are acceptable given the data so far
 plot(stations_and_pressure$Long, stations_and_pressure$Lat, pch="+", col="green")
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$distance>2,]$long, stations_and_pressure[stations_and_pressure$distance>2,]$lat, pch="+", col="blue")



        


 ### ADDITIONAL STATIONS-----------------------------------------------------
 load(file=file.path(outPath,"ALL_AggregatedSweptArea.RData")) # aggResult

 stations          <- read.table(file=file.path("C:","BENTHIS", "outputs" , 'CEFAS_StefanBolam_All_station_positions_120914.csv'), sep=';', dec=".", header=TRUE)
    stations$Latitude <- as.numeric(as.character(stations[,'Latitude'])) 
      stations$Longitude <- as.numeric(as.character(stations[,'Longitude']))
   
 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))
 
 stations_and_pressure <- list()
 for(year in c(2010:2012)){
    print(year)
    aggResultThisYear <- aggResult[aggResult$Year==year,] 
     for(metier in unique(aggResultThisYear$LE_MET)){
      print(metier)
      aggResultThisYearThisMet <- aggResultThisYear[aggResultThisYear$LE_MET==metier,] 
      for (iStat in 1: nrow(stations)){  # brute force
         cat(paste(iStat, "\n"))
         latRef <- as.numeric(as.character(stations[iStat,'Latitude'])) 
         lonRef <- as.numeric(as.character(stations[iStat,'Longitude']))
         dists  <- distance (aggResultThisYearThisMet$CELL_LONG, aggResultThisYearThisMet$CELL_LATI, lonRef, latRef)
         stations[iStat,"grID"]     <-  aggResultThisYearThisMet[which.min(dists), "grID"]
         stations[iStat,"distance"] <-  dists[which.min(dists)]
         }
     stations_and_pressure[[paste(year,"_",metier,sep='')]] <- merge(stations, aggResultThisYearThisMet)
     }
 } 
 sauv <-  stations_and_pressure
 stations_and_pressure  <- do.call("rbind", stations_and_pressure)
 stations_and_pressure  <- orderBy(~ source+stn.code+LE_MET, stations_and_pressure)
 head(stations_and_pressure, 20)
 
 save(stations_and_pressure, file=file.path(outPath,"stations_epifauna_and_pressure.RData"))

 write.table(stations_and_pressure, file=file.path(outPath,"stations_epifauna_and_pressure.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)


 # plot to see which stations are acceptable given the data so far
 plot(stations_and_pressure$Longitude, stations_and_pressure$Latitude, pch="+", col="green")
  plot(stations$Longitude, stations$Latitude, pch="+", col="green")
  library(mapdata)
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$distance>2,]$Long, stations_and_pressure[stations_and_pressure$distance>2,]$Lat, pch="+", col="red")



 
            


 ### MORE ADDITIONAL STATIONS-----------------------------------------------------
 load(file=file.path(outPath,"ALL_AggregatedSweptArea.RData")) # aggResult

 stations          <- read.table(file=file.path("C:","BENTHIS", "outputs" , 'CEFAS_StefanBolam_stations_biogeo_sediment.txt'), sep='\t', dec=".", header=TRUE)
    stations$Latitude <- as.numeric(as.character(stations[,'Latitude'])) 
      stations$Longitude <- as.numeric(as.character(stations[,'Longitude']))
   
 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))
 
 stations_and_pressure <- list()
 for(year in c(2010:2012)){
    print(year)
    aggResultThisYear <- aggResult[aggResult$Year==year,] 
     for(metier in unique(aggResultThisYear$LE_MET)){
      print(metier)
      aggResultThisYearThisMet <- aggResultThisYear[aggResultThisYear$LE_MET==metier,] 
      for (iStat in 1: nrow(stations)){  # brute force
         cat(paste(iStat, "\n"))
         latRef <- as.numeric(as.character(stations[iStat,'Latitude'])) 
         lonRef <- as.numeric(as.character(stations[iStat,'Longitude']))
         dists  <- distance (aggResultThisYearThisMet$CELL_LONG, aggResultThisYearThisMet$CELL_LATI, lonRef, latRef)
         stations[iStat,"grID"]     <-  aggResultThisYearThisMet[which.min(dists), "grID"]
         stations[iStat,"distance"] <-  dists[which.min(dists)]
         }
     stations_and_pressure[[paste(year,"_",metier,sep='')]] <- merge(stations, aggResultThisYearThisMet)
     }
 } 
 sauv <-  stations_and_pressure
 stations_and_pressure  <- do.call("rbind", stations_and_pressure)
 stations_and_pressure  <- orderBy(~ source+stn.code+LE_MET, stations_and_pressure)
 head(stations_and_pressure, 20)
 
 save(stations_and_pressure, file=file.path(outPath,"CEFAS_StefanBolam_stations_biogeo_sediment_and_pressure.RData"))

 write.table(stations_and_pressure, file=file.path(outPath,"CEFAS_StefanBolam_stations_biogeo_sediment_and_pressure.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)



  #- Plot all of it FOR CHECKING
 xrange <-  range(stations_and_pressure$Longitude)
 yrange <-  range(stations_and_pressure$Latitude)
 aggResult <- aggResult[aggResult$CELL_LONG >xrange[1] &
                        aggResult$CELL_LONG <xrange[2] &
                        aggResult$CELL_LATI >yrange[1] &
                        aggResult$CELL_LATI <yrange[2],
                        ]

 library(RColorBrewer)
 colintens             <- brewer.pal(9,"YlOrRd")
 #- Create polygon set of gridcells to plot (takes a bit longer)
 uniqueCells           <- aggResult[!duplicated(aggResult$grID),c("grID","CELL_LONG","CELL_LATI")]
 resx                  <- 1/60 #1 minute
 resy                  <- 1/60 #1 minute
 grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2), 
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
 # a subset
 subAggResult          <- aggResult [aggResult$LE_MET=="OT_DMF", ]
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)

 colnames(data2plot)[1]<- "grID"
 idx                   <- match(uniqueCells$grID, data2plot$grID)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]

 plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)
 plot(grdc2plot,add=TRUE,col=col2plot, border=0)   # sp:::plot.SpatialPolygons
 polPath   <- file.path("C:","BENTHIS", "BalanceMaps")
 #sh1 <- readShapePoly(file.path(polPath,"francois_EU"),  proj4string=CRS("+proj=longlat +datum=WGS84"))
 #plot(sh1, add=TRUE, col=grey(0.7))
   map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)


  stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]
 
 
 
 # plot to see which stations are acceptable given the data so far
 # points(stations$Longitude, stations$Latitude,  pch="+", col="red")
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,   stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, pch="+", col="green")
 the_colors <- cut(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$distance, breaks=c(seq(0, 6, by=1),1000))
 ramp <- colorRampPalette(c('blue','red'))
 levels (the_colors) <- ramp(7)
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,   stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, pch="+", col=as.character(the_colors))
  library(mapdata)
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$distance>2,]$Long, stations_and_pressure[stations_and_pressure$distance>2,]$Lat, pch="+", col="red")





 ### EVEN MORE ADDITIONAL STATIONS (ANdy 24June16)-----------------------------------------------------
 load(file=file.path(outPath,"ALL_AggregatedSweptArea_12062015.RData")) # aggResult

 stations          <- read.table(file=file.path("C:","BENTHIS", "outputs" , 'CEFAS_AndyKenny_Benthic_Infauna_Sample_FP_values.txt'), sep='\t', dec=".", header=TRUE)
    stations$Latitude <- as.numeric(as.character(stations[,'Latitude'])) 
      stations$Longitude <- as.numeric(as.character(stations[,'Longitude']))
   
 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))
 
 stations_and_pressure <- list()
 for(year in c(2010:2012)){
    print(year)
    aggResultThisYear <- aggResult[aggResult$Year==year,] 
     for(metier in unique(aggResultThisYear$LE_MET)){
      print(metier)
      aggResultThisYearThisMet <- aggResultThisYear[aggResultThisYear$LE_MET==metier,] 
      for (iStat in 1: nrow(stations)){  # brute force
         cat(paste(iStat, "\n"))
         latRef <- as.numeric(as.character(stations[iStat,'Latitude'])) 
         lonRef <- as.numeric(as.character(stations[iStat,'Longitude']))
         dists  <- distance (aggResultThisYearThisMet$CELL_LONG, aggResultThisYearThisMet$CELL_LATI, lonRef, latRef)
         stations[iStat,"grID"]     <-  aggResultThisYearThisMet[which.min(dists), "grID"]
         stations[iStat,"distance"] <-  dists[which.min(dists)]
         }
     stations_and_pressure[[paste(year,"_",metier,sep='')]] <- merge(stations, aggResultThisYearThisMet)
     }
 } 
 sauv <-  stations_and_pressure
 stations_and_pressure  <- do.call("rbind", stations_and_pressure)
 stations_and_pressure  <- orderBy(~ source+stn.code+LE_MET, stations_and_pressure)
 head(stations_and_pressure, 20)
 
 save(stations_and_pressure, file=file.path(outPath,"CEFAS_StefanBolam_stations_biogeo_sediment_and_pressure.RData"))

 write.table(stations_and_pressure, file=file.path(outPath,"CEFAS_StefanBolam_stations_biogeo_sediment_and_pressure.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)



  #- Plot all of it FOR CHECKING
 xrange <-  range(stations_and_pressure$Longitude)
 yrange <-  range(stations_and_pressure$Latitude)
 aggResult <- aggResult[aggResult$CELL_LONG >xrange[1] &
                        aggResult$CELL_LONG <xrange[2] &
                        aggResult$CELL_LATI >yrange[1] &
                        aggResult$CELL_LATI <yrange[2],
                        ]

 library(RColorBrewer)
 colintens             <- brewer.pal(9,"YlOrRd")
 #- Create polygon set of gridcells to plot (takes a bit longer)
 uniqueCells           <- aggResult[!duplicated(aggResult$grID),c("grID","CELL_LONG","CELL_LATI")]
 resx                  <- 1/60 #1 minute
 resy                  <- 1/60 #1 minute
 grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2), 
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
 # a subset
 subAggResult          <- aggResult [aggResult$LE_MET=="OT_DMF", ]
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)

 colnames(data2plot)[1]<- "grID"
 idx                   <- match(uniqueCells$grID, data2plot$grID)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]

 plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)
 plot(grdc2plot,add=TRUE,col=col2plot, border=0)   # sp:::plot.SpatialPolygons
 polPath   <- file.path("C:","BENTHIS", "BalanceMaps")
 #sh1 <- readShapePoly(file.path(polPath,"francois_EU"),  proj4string=CRS("+proj=longlat +datum=WGS84"))
 #plot(sh1, add=TRUE, col=grey(0.7))
   map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)


  stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]
 
 
 
 # plot to see which stations are acceptable given the data so far
 # points(stations$Longitude, stations$Latitude,  pch="+", col="red")
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,   stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, pch="+", col="green")
 the_colors <- cut(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$distance, breaks=c(seq(0, 6, by=1),1000))
 ramp <- colorRampPalette(c('blue','red'))
 levels (the_colors) <- ramp(7)
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,   stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, pch="+", col=as.character(the_colors))
  library(mapdata)
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$distance>2,]$Long, stations_and_pressure[stations_and_pressure$distance>2,]$Lat, pch="+", col="red")









##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   
 ## map pressure per metier per habitat for Ole Tromso (and Rasmus)
 
  load("C:\\BENTHIS\\outputs\\ALL_AggregatedSweptArea.RData")

 ## subset for the Western Baltic Sea and Kattegat
 ## because the coverage is not that good for Eastern Baltic Sea 
 idx_pts <- point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
       pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
       pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sd2224
            57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
           point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'], #sd2224  
       pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), 
       pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
       pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
       pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262),
       pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask

  aggResult <- aggResult[idx_pts,] 

   
  # load the BALANCE map
  library(maptools)
  library(raster)
  polPath   <- "C:/BENTHIS/BalanceMaps"
  anf       <- function(x) as.numeric(as.character(x))
  sh_coastlines            <- readShapePoly(file.path(polPath,"francois_EU"))

  ## use point-raster overlay.......
  library(raster)
  landscapes       <- raster(file.path(polPath, "landscapes.tif"))    # probably need an update of rgdal here....

  
  coord           <- cbind(x=anf(aggResult$CELL_LONG), y=anf(aggResult$CELL_LATI))
  
  # convert to UTM
  library(sp)
  library(rgdal)
  SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
  coord <- cbind.data.frame(coord,
                 spTransform(SP, CRS(paste("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


  dd <- extract (landscapes, coord[,3:4]) # get the landscape on the coord points!

  coord <- cbind(coord,  landscapes_code=dd)  # look at “Towards a representative MPA network in the Baltic Sea”.

  # translate the coding
  bottom <- substr(coord$landscapes_code, 1,1)
  photic <- substr(coord$landscapes_code, 2,2)
  salinity <- substr(coord$landscapes_code, 3,3)
  
  # caution!!:
  bottom_levels <- factor(bottom)
  levels(bottom_levels) <- c('Bedrock', 'Hard Bottom', 'Sand', 'Hard Clay', 'Mud')
 
  photic_levels <- factor(photic)
   levels(photic_levels) <- c('Photic', 'Aphotic')
  
  salinity_levels <- factor(salinity)
  levels(salinity_levels) <- c('5-7.5psu', '7.5-11psu', '11-18psu', '18-30psu', '>30psu')
 
  
  coord <- cbind.data.frame (coord, landscape=paste(bottom_levels, photic_levels, salinity_levels, sep="_"))
 
  
  aggResult <- cbind.data.frame(aggResult, coord)

 # check
   plot(coord[,"x"], coord[,"y"], pch=".", col=1)
   points(coord[is.na(coord$landscapes_code),"x"], coord[is.na(coord$landscapes_code),"y"], pch=".", col=2)

  plot(aggResult[,"x"], aggResult[,"y"], pch=".", col=1)
 points(aggResult[aggResult$landscape=="Hard Clay_Aphotic_5-7.5psu","x"], aggResult[aggResult$landscape=="Hard Clay_Aphotic_5-7.5psu","y"], pch=".", col=2)
 "Hard Clay_Aphotic_5-7.5psu"


##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
## create a fine grid for sampling
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
 do.regular.grid <- function(dx, dy, xdep, xarr, ydep, yarr){
    # noisy regular grid with dx and dy
    ext.x <- seq(xdep ,xarr ,by=dx)
    ext.y <- seq(ydep, yarr, by=dy)
    x <-  rep(c(ext.x), length(ext.y)) 
    y  <- rep(c(ext.y), each=length(ext.x))
    x <- x+ rnorm(length(x),0,0.0001)
    y <- y+ rnorm(length(y),0,0.0001)
 return(cbind(x,y))
 }
 grid.xy <- do.regular.grid(0.005,   0.005,   7,  15, 53,   60.1)  # fine grain
 idx_pts <- point.in.polygon(point.x = grid.xy[,'x'], point.y = grid.xy[,'y'],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | point.in.polygon(point.x = grid.xy[,'x'], point.y = grid.xy[,'y'],
       pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
       pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sd2224
            57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
           point.in.polygon(point.x = grid.xy[,'x'], point.y = grid.xy[,'y'], #sd2224  
       pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), 
       pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid.xy[,'x'], point.y = grid.xy[,'y'],
       pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid.xy[,'x'], point.y = grid.xy[,'y'],
       pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262),
       pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask
   grid.xy <- grid.xy[idx_pts,]


  coord           <- cbind(x=anf(grid.xy[,'x']), y=anf(grid.xy[,'y']))
  
  # convert to UTM
  library(sp)
  library(rgdal)
  SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
  coord <- cbind.data.frame(coord,
                 spTransform(SP, CRS(paste("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


  dd <- extract (landscapes, coord[,3:4]) # get the landscape on the coord points!

  coord <- cbind(coord,  landscapes_code=dd)  # look at “Towards a representative MPA network in the Baltic Sea”.

  # translate the coding
  bottom <- substr(coord$landscapes_code, 1,1)
  photic <- substr(coord$landscapes_code, 2,2)
  salinity <- substr(coord$landscapes_code, 3,3)
  
  
  bottom_levels <- factor(bottom)
  levels(bottom_levels) <- c('Bedrock', 'Hard Bottom', 'Sand', 'Hard Clay', 'Mud')
 
  photic_levels <- factor(photic)
   levels(photic_levels) <- c('Photic', 'Aphotic')
  
  salinity_levels <- factor(salinity)
  levels(salinity_levels) <- c('0-5psu', '5-7.5psu', '7.5-11psu', '11-18psu', '18-30psu', '>30psu')
 
  
  coord <- cbind.data.frame (coord, landscape=paste(bottom_levels, photic_levels, salinity_levels, sep="_"))
 
  grid.xy <- cbind.data.frame(grid.xy, coord)

  # remove the NA_NA_NA (likely on land)
  points(grid.xy[grid.xy[,'landscape']=="NA_NA_NA",'x'], grid.xy[grid.xy[,'landscape']=="NA_NA_NA",'y'], col=2)
  grid.xy <- grid.xy[grid.xy[,'landscape']!="NA_NA_NA",]


  percent_habitats <- round(table( grid.xy$landscape)/sum(table( grid.xy$landscape))*100, 2) # get an idea of the importance of a given landscape in terms of number of nodes  over the total number of nodes
  sum(percent_habitats) # should be 100
 
  percent_habitats[order(percent_habitats, decreasing=TRUE)]
 
 # check
  plot(grid.xy[,"x"], grid.xy[,"y"], pch=".", col=1)
 points(grid.xy[grid.xy$landscape=="Mud_Aphotic_>30psu","x"], grid.xy[grid.xy$landscape=="Mud_Aphotic_>30psu","y"], pch=".", col=2)
 "Mud_Aphotic_>30psu"
 
##------------------------------------------------------------------------------##
##------------------------------------------------------------------------------##
##------------------------------------------------------------------------------##
##------------------------------------------------------------------------------##
##------------------------------------------------------------------------------##
##------------------------------------------------------------------------------##


   #  do the aggregation
  aggResult               <- aggResult[aggResult$landscape!="NA_NA_NA",]
  aggResultPerHab         <- aggregate(aggResult$SWEPT_AREA_KM2, list(aggResult$landscape), sum)
  aggResultPerMet         <- aggregate(aggResult$SWEPT_AREA_KM2, list(aggResult$LE_MET), sum)
  aggResultPerHabMet      <- aggregate(aggResult$SWEPT_AREA_KM2, list(aggResult$LE_MET, aggResult$landscape), sum)
  aggResultPerHabMetUpper <- aggregate(aggResult$SWEPT_AREA_KM2_UPPER, list(aggResult$LE_MET, aggResult$landscape), sum)
  aggResultPerHabMetLower <- aggregate(aggResult$SWEPT_AREA_KM2_LOWER, list(aggResult$LE_MET, aggResult$landscape), sum)

  
  colnames(aggResultPerHab)    <- c('habitat', 'sweptarea')
  colnames(aggResultPerMet)    <- c('benthis_met', 'sweptarea')
  colnames(aggResultPerHabMet) <- c('benthis_met', 'habitat', 'sweptarea')

  aggResultPerHab$sweptarea <- round(aggResultPerHab$sweptarea)

  # select habitats with > 1000km2 swept area for the period 2010-2012
  habitats_to_keep    <- as.character(aggResultPerHab[aggResultPerHab$sweptarea>1000, "habitat"])
  # select metiers with > 1000km2 swept area for the period 2010-2012
  metiers_to_keep    <- as.character(aggResultPerMet[aggResultPerMet$sweptarea>1000, "benthis_met"])
  aggResultPerHabMet  <- aggResultPerHabMet[
                                          aggResultPerHabMet$habitat %in% habitats_to_keep &
                                          aggResultPerHabMet$benthis_met %in% metiers_to_keep,
                                          ]

 
 # reshape to get a matrix
  aggResultPerHabMetWide <- reshape(aggResultPerHabMet, v.names = "sweptarea", idvar = "habitat",
                timevar = "benthis_met", direction = "wide")
  rownames(aggResultPerHabMetWide) <- aggResultPerHabMetWide$habitat
  

  # the plot
  namefile <- "cumul_sweptarea_per_metier"
  tiff(filename=file.path("C:", "BENTHIS", paste(namefile, ".tiff", sep="" )),
                                   width = 2000, height = 2000, 
                                   units = "px", pointsize = 12,  res=300)
  par(mar=c(5,10,3,1))
   rownames(aggResultPerMet) <- aggResultPerMet$benthis_met 
   aggResultPerMet <- aggResultPerMet[aggResultPerMet$benthis_met!="NA",]
   aggResultPerMet <- aggResultPerMet[aggResultPerMet$benthis_met!="OT_FWS",]
    dd <- aggResultPerMet[,-1]
    names(dd) <- rownames(aggResultPerMet)
    dd <- c(dd, DENMARK_LAND= 43100)
    
    bar <- barplot(dd, xpd=FALSE,# log="x",
            col= "black",
            #col=brewer.pal(6, "Dark2"), border=brewer.pal(6, "Dark2"),
            axisnames = TRUE, beside=TRUE,  horiz=TRUE,  axes=FALSE,space=c(0.5), las=2)
    mtext(side=1,"Cumulated swept area (km2)",line=3.0, cex=1)
   axis(1)
   box()
   dev.off()


  # the plot
  namefile <- "cumul_sweptarea_per_habitat_and_metier"
  tiff(filename=file.path("C:", "BENTHIS", paste(namefile, ".tiff", sep="" )),
                                   width = 4600, height = 2800, 
                                   units = "px", pointsize = 12,  res=300)

  par(mfrow=c(2,3))
  par(mar=c(5,15,5,1))
  for(i in 1: length(metiers_to_keep)){

 
    the_mat <- t(as.matrix(aggResultPerHabMetWide[,-1]))
   
    bar <- barplot(the_mat[i,],  log="x", xlim=c(1,1e5),  xpd=FALSE,
            col= "black",
            #col=brewer.pal(6, "Dark2"), border=brewer.pal(6, "Dark2"),
            axisnames = TRUE, beside=TRUE,  horiz=TRUE,  axes=FALSE,space=c(0), las=2)
    mtext(side=1,"Cumulated swept area (km2)",line=3.0, cex=1)
    mtext(side=3,paste("(",letters[i], ")", sep=''),line=3.0, adj=-0.65, cex=1.5)
    axis(1)
    
    par(new=TRUE)
    barplot(-percent_habitats[  colnames(the_mat)],space=c(4), axisnames = FALSE, horiz=TRUE, axes=FALSE, xlim=c(-25,0), col=grey(0.8), border=grey(0.8) )
    axis(3, at=c(0,-5,-10,-15,-20), labels=c(0,5,10,15,20))                                                                                                              
    mtext(side=3,"% of the habitat", line=3.0, cex=1)

    #par(new=TRUE)
    #bar <- barplot(the_mat[i,], log="x",   xlim=c(1,1e5),
    #        col= "black",
    #        axisnames = TRUE, beside=TRUE,  horiz=TRUE,  axes=FALSE,space=c(0.1, 3), las=2)
    box()

    colnames(t(as.matrix(aggResultPerHabMetWide[,-1])))
    }
    
    #legend("bottom", legend=c("OT_CRU", "OT_DMF", "OT_MIX_NEP", "OT_SPF", "OT_MIX_DMF_PEL", "SDN_DEM"), 
    # bty="n", fill=grey(0.5), border=FALSE)#fill=brewer.pal(6, "Accent"))
  
  dev.off()
















 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ## Aggregate pressure per BENTHIS metier per EUNIS habitat for Niels & Adriaan
 
  load("C:\\BENTHIS\\outputs\\ALL_AggregatedSweptArea.RData")            # get 'aggResult'
  load("C:\\BENTHIS\\benthis_Rworkflows\\overlayGridNS_Eunis3_4.RData")  # get 'layers'
                     
  rlong  <- range(layers[,'CELL_LONG'])
  rlati  <- range(layers[,'CELL_LATI'])

  an <- function(x) as.numeric(as.character(x))
  
  aggResult <- aggResult[  
                           an(aggResult$CELL_LONG)>=rlong[1] & 
                           an(aggResult$CELL_LONG)<=rlong[2] & 
                           an(aggResult$CELL_LATI)>=rlati[1] & 
                           an(aggResult$CELL_LATI)<=rlati[2], 
                        ]   

 
  layers <- rbind(
                  cbind(layers, Year=2010),
                  cbind(layers, Year=2011),
                  cbind(layers, Year=2012)
                  )
  aggResult <- merge(aggResult, layers, all = TRUE)
 
  # aggregations...
  aggResultPerHab3MetYear      <- aggregate(aggResult$SWEPT_AREA_KM2, list(aggResult$LE_MET, aggResult$Eunis_3, aggResult$Year), sum)
  aggResultPerHab4MetYear      <- aggregate(aggResult$SWEPT_AREA_KM2, list(aggResult$LE_MET, aggResult$Eunis_4, aggResult$Year), sum)
  colnames(aggResultPerHab3MetYear) <- c('LE_MET', 'Eunis_3', 'Year', 'SWEPT_AREA_KM2')
  colnames(aggResultPerHab4MetYear) <- c('LE_MET', 'Eunis_4', 'Year', 'SWEPT_AREA_KM2')

  #...or bins
  nbCellGridPerBinSweptAreaKm2 <- lapply(
    split(aggResult, f=paste(aggResult$LE_MET, aggResult$Eunis_3, aggResult$Year, sep=".")),
    function(x){
     table(cut(x$SWEPT_AREA_KM2, breaks=c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10, 1000)))
    })

  nbCellGridPerBinSweptAreaKm2PerMetPerEunis3PerYear <- do.call("rbind", nbCellGridPerBinSweptAreaKm2)
  
  
  nbCellGridPerBinSweptAreaKm2 <- lapply(
    split(aggResult, f=paste(aggResult$LE_MET, aggResult$Eunis_4, aggResult$Year, sep=".")),
    function(x){
     table(cut(x$SWEPT_AREA_KM2, breaks=c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10, 1000)))
    })

  nbCellGridPerBinSweptAreaKm2PerMetPerEunis4PerYear <- do.call("rbind", nbCellGridPerBinSweptAreaKm2)

  
  # to compute unfished proportion per habitat per metier...
  # (Lazy code: some copy/paste to avoid dealing with a 3d array)
  # EUNIS 3 - 2010
  aggResult2010                    <- aggResult[aggResult$Year=="2010",]
  nbCellGridPerMetPerEunis3For2010 <-   table(aggResult2010$LE_MET, aggResult2010$Eunis_3)
  nbCellGridPerMetPerEunis3For2010
  nbCellGridPerMetPerEunis3For2010 <-   rbind( 
                                            nbCellGridPerMetPerEunis3For2010,
                                            total_nb_cells_met= apply(nbCellGridPerMetPerEunis3For2010, 2, sum) - nbCellGridPerMetPerEunis3For2010["NA",], 
                                            total_nb_cells_eunis3=table(aggResult2010[!duplicated(data.frame(aggResult2010$CELL_LONG, aggResult2010$CELL_LATI)), 'Eunis_3'])
                                            )

  # EUNIS 4 - 2010
  nbCellGridPerMetPerEunis4For2010 <-   table(aggResult2010$LE_MET, aggResult2010$Eunis_4)
  nbCellGridPerMetPerEunis4For2010
  nbCellGridPerMetPerEunis4For2010 <-   rbind( 
                                            nbCellGridPerMetPerEunis4For2010,
                                            total_nb_cells_met= apply(nbCellGridPerMetPerEunis4For2010, 2, sum) - nbCellGridPerMetPerEunis4For2010["NA",], 
                                            total_nb_cells_eunis4=table(aggResult2010[!duplicated(data.frame(aggResult2010$CELL_LONG, aggResult2010$CELL_LATI)), 'Eunis_4'])
                                            )
                                            


  # EUNIS 3 - 2011
  aggResult2011                    <- aggResult[aggResult$Year=="2011",]
  nbCellGridPerMetPerEunis3For2011 <-   table(aggResult2011$LE_MET, aggResult2011$Eunis_3)
  nbCellGridPerMetPerEunis3For2011 <-   rbind( 
                                            nbCellGridPerMetPerEunis3For2011,
                                            total_nb_cells_met= apply(nbCellGridPerMetPerEunis3For2011, 2, sum) - nbCellGridPerMetPerEunis3For2011["NA",], 
                                            total_nb_cells_eunis3=table(aggResult2011[!duplicated(data.frame(aggResult2011$CELL_LONG, aggResult2011$CELL_LATI)), 'Eunis_3'])
                                            )

  # EUNIS 4 - 2011
  nbCellGridPerMetPerEunis4For2011 <-   table(aggResult2011$LE_MET, aggResult2011$Eunis_4)
  nbCellGridPerMetPerEunis4For2011 <-   rbind( 
                                            nbCellGridPerMetPerEunis4For2011,
                                            total_nb_cells_met= apply(nbCellGridPerMetPerEunis4For2011, 2, sum) - nbCellGridPerMetPerEunis4For2011["NA",], 
                                            total_nb_cells_eunis4=table(aggResult2011[!duplicated(data.frame(aggResult2011$CELL_LONG, aggResult2011$CELL_LATI)), 'Eunis_4'])
                                            )

   # EUNIS 3 - 2012
  aggResult2012                    <- aggResult[aggResult$Year=="2012",]
  nbCellGridPerMetPerEunis3For2012 <-   table(aggResult2012$LE_MET, aggResult2012$Eunis_3)
  nbCellGridPerMetPerEunis3For2012 <-   rbind( 
                                            nbCellGridPerMetPerEunis3For2012,
                                            total_nb_cells_met= apply(nbCellGridPerMetPerEunis3For2012, 2, sum) - nbCellGridPerMetPerEunis3For2012["NA",], 
                                            total_nb_cells_eunis3=table(aggResult2012[!duplicated(data.frame(aggResult2012$CELL_LONG, aggResult2012$CELL_LATI)), 'Eunis_3'])
                                            )

  # EUNIS 4 - 2012
  nbCellGridPerMetPerEunis4For2012 <-   table(aggResult2012$LE_MET, aggResult2012$Eunis_4)
  nbCellGridPerMetPerEunis4For2012 <-   rbind( 
                                            nbCellGridPerMetPerEunis4For2012,
                                            total_nb_cells_met= apply(nbCellGridPerMetPerEunis4For2012, 2, sum) - nbCellGridPerMetPerEunis4For2012["NA",], 
                                            total_nb_cells_eunis4=table(aggResult2012[!duplicated(data.frame(aggResult2012$CELL_LONG, aggResult2012$CELL_LATI)), 'Eunis_4'])
                                            )


  save(aggResult,
       rlong,
       rlati,
       aggResultPerHab3MetYear, 
       aggResultPerHab4MetYear,
       nbCellGridPerMetPerEunis3For2010, nbCellGridPerMetPerEunis4For2010,
       nbCellGridPerMetPerEunis3For2011, nbCellGridPerMetPerEunis4For2011,
       nbCellGridPerMetPerEunis3For2012, nbCellGridPerMetPerEunis4For2012,
       nbCellGridPerBinSweptAreaKm2PerMetPerEunis3PerYear, 
       nbCellGridPerBinSweptAreaKm2PerMetPerEunis4PerYear,
         file="C:\\BENTHIS\\outputs\\NielsRequestForWP7-NS.RData")












  
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ##------------------------------------------------------------------------------##
 ## plot the missing effort (from countries with these data)


  # get aggResult missing effort.
  load("C:\\BENTHIS\\outputs\\DEN_missingEffortTable.RData")
  aggResultAll <- aggResult
  load("C:\\BENTHIS\\outputs\\NLD_missingEffortTable.RData")
  aggResultAll <- rbind.data.frame(aggResultAll, aggResult)
  load("C:\\BENTHIS\\outputs\\IRL_missingEffortTable.RData")
  aggResultAll <- rbind.data.frame(aggResultAll, aggResult)
  load("C:\\BENTHIS\\outputs\\BEL_missingEffortTable.RData")
  aggResultAll <- rbind.data.frame(aggResultAll, aggResult)
  load("C:\\BENTHIS\\outputs\\SCO_missingEffortTable.RData")
  aggResultAll <- rbind.data.frame(aggResultAll, aggResult)
  load("C:\\BENTHIS\\outputs\\SWE_missingEffortTable.RData")
  aggResultAll <- rbind.data.frame(aggResultAll, aggResult)
  load("C:\\BENTHIS\\outputs\\UK_missingEffortTable.RData")
  aggResultAll <- rbind.data.frame(aggResultAll, aggResult)
  
  
  

  aggResult2 <- aggregate(list(INTVDAY=aggResultAll$INTVDAY, INTVDAY_TOT=aggResultAll$INTVDAY_TOT), list(LE_RECT=aggResultAll$LE_RECT), sum, na.rm=TRUE)
  aggResult2$percentMissed <-  100 - (aggResult2$INTVDAY /  aggResult2$INTVDAY_TOT *100)

   aggResult2$SI_LONG <- an(ICESrectangle2LonLat(ac(aggResult2$LE_RECT),midpoint=TRUE)[, 2])
   aggResult2$SI_LATI <- an(ICESrectangle2LonLat(ac(aggResult2$LE_RECT),midpoint=TRUE)[, 1])

   aggResult2$the_colors              <- cut(aggResult2$percentMissed, breaks=c(-1,0, 20,40,60,80,100, 101))
   levels (aggResult2$the_colors)     <- terrain.colors(7)
   
   #plot
   map(xlim=c(-15, 25), ylim=c(45,70))
   for(i in 1: nrow(aggResult2)) {
     polygon(
             x=c(aggResult2$SI_LONG[i]- 1/2,aggResult2$SI_LONG[i]+ 1/2,aggResult2$SI_LONG[i] + 1/2, aggResult2$SI_LONG[i]- 1/2),
             y=c(aggResult2$SI_LATI[i] -0.5/2,aggResult2$SI_LATI[i]-0.5/2, aggResult2$SI_LATI[i] +0.5/2, aggResult2$SI_LATI[i] +0.5/2),
             border=NA,
             col=as.character(aggResult2$the_colors[i])
             )
   }
  map(xlim=c(-15, 25), ylim=c(45,70), fill=TRUE, col=grey(0.5), add=TRUE)

  




















#- Second, take only TBB_CRU metiers
subAggResult          <- subset(aggResult,LE_MET %in% c("TBB_CRU"))
data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)
colnames(data2plot)[1]<- "grID"
idx                   <- match(uniqueCells$grID, data2plot$grID)
col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.5,1,2,5,10,20,50,100,200))]
plot(grdc2plot,add=T,col=col2plot,border=0)







#-----------------------------------------------------------------------------
 # LINK TO HABITAT MAPS
 #-----------------------------------------------------------------------------


  #------------------
  # SHAPEFILE--------
  #------------------
  library(maptools)
  library(rgdal)
  
  # load a habitat map shape file
  habitat_map           <- readShapePoly(file.path(polPath,"sediment_lat_long"),
                                         proj4string=CRS("+proj=longlat +ellps=WGS84"))

  sh_coastlines            <- readShapePoly(file.path(polPath,"francois_EU"))


  load(file.path(outPath, a_year, "tacsatSweptArea.RData")) # get 'tacsatSweptArea' with all data
  #....or load only one instance eg load(file.path(outPath, a_year, "interpolated","tacsatSweptArea_DNK000001744_OTB.RData"))
  #tacsatSweptArea <- tacsatIntGearVEREF

  coord <-  tacsatSweptArea[, c('SI_LONG', 'SI_LATI')]
  names(habitat_map) # return the name of the coding variable

  #Turn the polygons into spatial polygons
  sp <- SpatialPolygons(habitat_map@polygons, proj4string=CRS("+proj=longlat +ellps=WGS84"))
 
  #Turn the point into a spatial point
  spo <- SpatialPoints(data.frame(SI_LONG=coord[,1], SI_LATI=coord[,2]), proj4string=CRS("+proj=longlat +ellps=WGS84"))         
 
  #Use the magic 'over' function to see in which polygon it is located
  idx <- over(spo,sp); print(idx)
  tacsatSweptArea$SUBSTRATE <- habitat_map$BAL_CODE[idx]

  # plot
  plot(habitat_map, xlim=c(11,14), ylim=c(55,56))
  axis(1) ; axis(2, las=2) ; box()
  points(tacsatSweptArea[, c("SI_LONG","SI_LATI")], col=tacsatSweptArea$SUBSTRATE, pch=".")
  #plot(sh_coastlines, add=TRUE)

  # save
  savePlot(filename=file.path(outPath,a_year,"VMSpingsAttachedToSedimentMap.jpeg"), type="jpeg")


  #------------------
  # RASTER-----------
  #------------------
  sh_coastlines            <- readShapePoly(file.path(polPath,"francois_EU"))

  ## use point-raster overlay.......
  library(raster)
  landscapes       <- raster(file.path(polPath, "landscapes.tif"))    # probably need an update of rgdal here....
  newproj          <- "+proj=longlat +datum=WGS84"
  landscapes_proj  <- projectRaster(landscapes, crs=newproj)

  save(landscapes_proj, file=file.path(polPath,a_year, "landscapes_proj.RData"))

  load(file.path(polPath, "landscapes_proj.RData"))

  load(file.path(outPath, a_year, "tacsatSweptArea.RData")) # get 'tacsatp' with all data
  #....or load only one instance eg load("C:\\merging\\BENTHIS\\outputs\\interpolated\\tacsatSweptArea_DNK000005269_OTB.RData"))
  #tacsatp <- tacsatpGearVEREF


  coord <- cbind(x=anf(tacsatp$SI_LONG), y=anf(tacsatp$SI_LATI))

  dd <- extract (landscapes_proj, coord[,1:2]) # get the landscape on the coord points!

  coord <- cbind(coord,  landscapes_code=cut(dd, breaks=c(0,100,200,300,400,500,600)))

  tacsatp <- cbind(tacsatp, landscapes_code= coord[,'landscapes_code'])

  # plot and save...
  plot(landscapes_proj, xlim=c(10,14), ylim=c(54.5,56.5))
  plot(sh_coastlines,  xlim=c(10,14), ylim=c(54.5,56.5), add=TRUE)  # better for plotting the western baltic sea coastline!
  points(coord[,"x"], coord[,"y"], col=coord[,"landscapes_code"], pch=".", cex=1)

  # save
  savePlot(filename=file.path(outPath,a_year, "VMSpingsAttachedToLandscapeMap.jpeg"), type="jpeg")



 #-----------------------------------------------------------------------------
 #  SEVERITY OF IMPACT
 #-----------------------------------------------------------------------------

  # create a fake input file to show the required format
  dd <- tacsatp[!duplicated(data.frame(tacsatp$VE_REF,tacsatp$LE_GEAR, tacsatp$LE_MET)), ]
  fake_gear_metier_habitat_severity_table <- dd[,c('VE_REF', 'LE_GEAR', 'LE_MET')]
  fake_gear_metier_habitat_severity_table <- cbind(fake_gear_metier_habitat_severity_table, HAB_SEVERITY=1)
  gear_metier_habitat_severity_table  <- fake_gear_metier_habitat_severity_table
  gear_metier_habitat_severity_table  <-   gear_metier_habitat_severity_table[complete.cases( gear_metier_habitat_severity_table),]
  save(gear_metier_habitat_severity_table,   file=paste(dataPath,a_year,"gear_metier_habitat_severity_table.RData",   sep=""))

  # load a table for HAB_SEVERITY per vid LE_REF per gr LE_GEAR per met LE_MET
  load(file.path(dataPath, "gear_metier_habitat_severity_table.RData"))
  tacsatp <- merge(tacsatp, gear_metier_habitat_severity_table)
  save(tacsatp,   file=paste(outPath,a_year,"tacsatMergedHabSeverity.RData",   sep=""))

  # pressure: weigh the swept area by the severity
  tacsatp$pressure <- tacsatp$HAB_SEVERITY * tacsatp$SWEPT_AREA_KM2

