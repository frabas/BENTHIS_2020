

 setwd (file.path("D:","FBA","BENTHIS_2020"))   # adapt to your need
 
 
 
 
##!!!!!!!!!!!## 
##!!!UTILS!!!##
##!!!!!!!!!!!##
 legend.gradient2 <-
function (pnts, cols = heat.colors(100), limits = c(0, 1), title = "Legend", legend="",
    ...)
{
    pnts = try(as.matrix(pnts), silent = T)
    if (!is.matrix(pnts))
        stop("you must have a 4x2 matrix")
    if (dim(pnts)[1] != 4 || dim(pnts)[2] != 2)
        stop("Matrix must have dimensions of 4 rows and 2 columms")
    if (length(cols) < 2)
        stop("You must have 2 or more colors")

    rect(min(pnts[, 1])-0.5, min(pnts[, 2])-0.5, max(pnts[, 1])+2, max(pnts[, 2])+0.5, col="white", border=NA)    

    yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length = length(cols) +
        1)
    for (i in 1:length(cols)) {
        polygon(x = pnts[, 1], y = c(yvals[i], yvals[i], yvals[i +
            1], yvals[i + 1]), col = cols[i], border = F)
    }
    text(max(pnts[, 1]), min(pnts[, 2]), labels = limits[1],
        pos = 4, ...)
    text(max(pnts[, 1]), max(pnts[, 2]), labels = limits[2],
        pos = 4, ...)
    start_pos <- (min(pnts[, 2])+((max(pnts[, 2])-min(pnts[, 2]))/length(legend))/2)
    for (i in 1: length(legend)){
    text(max(pnts[, 1])-0, start_pos + ((i-1) * ((max(pnts[, 2])-min(pnts[, 2]))/length(legend)) ), labels = legend[i],
        pos = 4, ...)
    #browser()
    }
    
    rect(min(pnts[, 1]), max(pnts[, 2])+0.1, min(pnts[, 1])+8, max(pnts[, 2])+1, col="white", border=NA)    
     text(min(pnts[, 1])-0.1, max(pnts[, 2])-0.1, labels = title, adj = c(0,
        -1), ...)
        
}
         
##!!!!!!!!!!!!!!!!!!!## 
##!!!CORE FUNCTION!!!##
##!!!!!!!!!!!!!!!!!!!##
quickmap <- function(namefile = paste0("LE_KG_COD_2019", ".tif"), 
                     aggResult   =  aggResult, 
                     # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     #nameobj  = "aggResult",
                     a_met    = NULL,
                     nametype = "LE_KG_COD",
                     a_unit   = 1,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp")),
                     spatial_polys=NULL,
                     a_width=2000,
                     a_height=2000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots" )
){
 
  dir.create(output_dir, recursive=TRUE)
  
  an <- function(x) as.numeric(as.character(x))
 
  
  #load(a_file)
  #my_data <- get(nameobj)
  if(!is.null(a_met))  aggResult <- aggResult[aggResult$LE_MET==a_met,]

  # caution: test if early stop.
  do_it <- FALSE
    cat("look into ", namefile, " and ", nametype[1],"\n")
    if( grepl("sp_with_max_vpuf",nametype[1]) || grepl("sp_with_max_cpue",nametype[1]) || grepl("sp_with_max_cpuf",nametype[1]) ||  grepl("sp_with_max_vpufswa",nametype[1])
         || any(aggResult[!is.na(aggResult[,nametype[1]]),nametype[1]]>0.5))
    {
      do_it <- TRUE
    }
    if(!do_it){
      cat("irrelevant combination of met-sp \n")
      return()
    } # early stop to avoid plotting for a irrelevant combination of met-sp

   # viable namefile
  namefile <- gsub("<","u", namefile)
  namefile <- gsub(">","o", namefile)
  namefile <- gsub("\\[","", namefile)
  namefile <- gsub("\\)","", namefile)
  namefile <- gsub("\\,","-", namefile)
  
  multi1 <- 1 ; multi2 <- 1
  if(length(nametype)==4) {multi1 <- 2 ; multi2 <- 2}
  if(length(nametype)==3) {multi1 <- 3 ; multi2 <- 1} 
  if(length(nametype)==1) {multi1 <- 1 ; multi2 <- 1} 
  
  tiff(filename=file.path(output_dir,  namefile),   width = a_width*multi1, height = a_height*multi2,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  par(mfrow=c(1,length(nametype)))
  if(length(nametype)==4) par(mfrow=c(2,2))
  for(itype in 1: length(nametype))
    {

    a_nametype <- nametype[itype]
    my_data   <- aggResult
    

    if(is.numeric(my_data[1, a_nametype]))
    {
            my_data[,nametype] <-   an(my_data[, a_nametype])/a_unit
    }
    my_data <- my_data[!is.na(my_data[,long]),]
    my_data <- my_data[!is.na(my_data[,lat]),]
 
 
   


    if(use_fao_areas){
       library(rgdal)
       an_area <- unlist(lapply(strsplit(a_met, split="_"), function(x) x[2]))
      if(an_area=="OTHER" || is.na(an_area)) an_area <- c("27.3","27.4")
       fao_areas  <- fao_areas[ fao_areas$F_SUBAREA %in% c(an_area),] 
       xlims <- c(extent(fao_areas)@xmin,extent(fao_areas)@xmax) # override 
       ylims <- c(extent(fao_areas)@ymin,extent(fao_areas)@ymax) # override 
    }

  
  my_data <- my_data[my_data[,long]>=xlims[1] & my_data[,long]<=xlims[2],]
  my_data <- my_data[my_data[,lat]>=ylims[1] & my_data[,lat]<=ylims[2],]
  print(head(my_data,2))
  
  if(nrow(my_data)==0) return(0)
   
 #-------------------------
 # possible post treatment on data (TODO: REVISE IF REQUIRED)
 if(FALSE){
  # compute the subsurface swept area
  matprop <- read.table(file=file.path("C:", "BENTHIS", "data_gear_spec_questionnaire",
                           "Subsurface_proportion_by_metier_Figure10_Eigaard_et_al.csv"),
                           header=TRUE, sep=";")
   #=> small discrepencies in naming the metier between the paper and the BENTHIS workflow corrected by hand. (e.g. OT_MIX_NEP=>OT_MIX_CRU, etc.)                        
                           
  rownames(matprop)            <- matprop$LE_MET
  my_data$multiplier         <- factor(my_data$LE_MET)
  levels(my_data$multiplier) <- matprop[levels(my_data$multiplier), "Subsurface"]
 }
 #-------------------------
 
 
 #-------------------------
 # export to ArcGIS, if it comes it is more convenient for the final rendering
 if(FALSE){
    library(raster)
    xrange      <- range(aggResult$CELL_LONG, na.rm=TRUE) # ALL
    yrange      <- range(aggResult$CELL_LATI, na.rm=TRUE)  # ALL
    r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
    some_coords <- SpatialPoints(cbind(SI_LONG=aggResult$CELL_LONG, SI_LATI=aggResult$CELL_LATI))
    # caution: because three years then we need to divide estimates by 3 
    rstr        <- rasterize(x=some_coords, y=r, field=aggResult$SWEPT_AREA_KM2/3, fun="sum") 
    plot(rstr, xlim=c(-5,13), ylim=c(53,60))
    a           <- area(rstr)
    rstr_std    <- rstr/a   # transform to SAR
   # equivalent to:
   # tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
   # tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- 
   #                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize
   rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   rstr_eea[is.na(rstr_eea)] <- -999
   rstr_eea[rstr_eea<0.001]  <- -999
   writeRaster(rstr_eea, file.path(getwd(), "outputs2020", "output_plots","AnnualAgg2RasterEEA"), format = "GTiff")
   
   ##
   #ArcGIS 10.1 Workflow for producing the BENTHIS WP2 map final polishing:
   #1-	Import XY AggResult.txt into ArcGIS in Layers>Add Data, click on the imported data and define X, Y and Z field, and define basic World projection WGS84 (put this system in Favorite). [bug: You might encounter some trouble if not integer values in the Z column i.e. <Null> fields...then round the values before importing]
   #2-	 Import  ne_50m_ocean.shp – [optional: draw a rectangle to clip in Custumize>toolbars>draw and save the rectangle as .shp by clicking right on Layers in the Table of Contents and then >Convert  Graphics to Features. Then in ArcToolbox>Clip...clip the ocean with the rectangle shape]
   #3-	Geoprocessing> ArcToolBox >Conversion Tools>to Raster> Point to Raster for rasterizing the XY layer e.g. with 0.0167 degree i.e. 1 minute
   #4-	In Data Management Tools, reprojection of the raster layer with projection>Raster>project and use the EEA proj definition (Lambert Azimuthal Equal Area, - put this system in Favorite)
   #5-	In Data Management Tools, reprojection of the feature shape layer with projection>Feature> project and use the EEA proj definition (Lambert Azimuthal Equal Area)
   #6-	Define the coordinate system of the Layers to be the EEA projection by clicking right on Layers.
   #7-	For a given layer>properties, define classification for the symbolisation e.g. streched and manual breaks...
   #8-	Add a graticule (will be only visible in layout mode) in Layers>properties>grid – close and reedit the graticule to adapt.
   #9-	Add a legend (in layout mode) with menu>Insert>legend – dont forget to look into “style”
   #10-	T copy/paste the symbology from one layer to the next use the ArcTools>data management tools>Layers and table views>Apply symbology from layers
   #11-	For layers with labels, click right on the layer and click on Label Features. To edit the lable use menu>Custumize>Toolbars>Label>Managing labels 
   #12-	Export a tif with menu>File>export map in lwz compression and in 500 dpi
   #13-	To re-apply symbology of a raster from an external project, first save the raster layer as a layer file and then import the symbology from the layer i.e. to save as a layer file right click on the layer in the table of contents and select 'save as layer file'.To import the symbology click on the open folder in the raster properties dialog. 
   
   #Layer query is:
   #"Countrynam" = 'Belgium' OR  "Countrynam" = 'DE' OR "Countrynam" = 'Denmark' OR "Countrynam" = 'Faroe Islands' OR "Countrynam" = 'Greece' OR "Countrynam" = 'Ireland' OR "Countrynam" = 'Isle of Man' OR "Countrynam" = 'Italy' OR "Countrynam" = 'Netherlands' OR "Countrynam" = 'Norway' OR "Countrynam" = 'Portugal' OR "Countrynam" = 'Sweden' OR "Countrynam" = 'United Kingdom'
   #Color scale
   # R0 G132 B168 – R142 G189 B181 – R255 G255 B191 – R 222 G130 B80 – R168 G0 B0

   #To do some operations from raster layerss then we need the Spatial Analyst Tools extension:
   #1-	Enable it by ticking the box in Custumize>Extensions (!)
   #2-	Use it in Geoprocessing>ArcToolBox>Spatial AnalystTools>Map Algebra> Raster calculator e.g. (100*(sce1/baseline) )-100

   }
 
 # get the spatial extent of the variable distribution!!
 #where_there_is_effort <- my_data[!is.na(my_data[, nametype]),]
 #where_there_is_effort <- where_there_is_effort [as.numeric(as.character(where_there_is_effort[, nametype]))>10,] # cuation: a threshold in hour here. And also scale dependent (here grid 0.05 degress)....
 #where_there_is_effort <- where_there_is_effort[!duplicated(where_there_is_effort$c_square),]
 #sum(where_there_is_effort[, "cell_area"])


 # ...and map
 Satellite.Palette.baseline <-colorRampPalette(c("cyan","aquamarine","orange","red"))
 #Satellite.Palette.baseline <- colorRampPalette(c("red","orange","aquamarine","cyan"))  # reverse palette

   
  if(a_nametype %in% c('SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')){
    if(grid_agg_res== (1/60)) the_breaks_baseline<-  c(0, round(exp(seq(-1.5, 2.5, by=0.3)),1), 10000)  # if 1 minute  i.e. 0.16 degree
    if(grid_agg_res== (3/60)) the_breaks_baseline<-  c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 10000)    # if 3 minutes i.e. 0.05 degrees
    if(grid_agg_res== 0.01)   the_breaks_baseline<-  c(0, round(exp(seq(-1.5, 2.5, by=0.3)),1), 10000)  # less than 1 minute  i.e. 0.01 degree
  }
  if(length(grep("LE_KG", a_nametype)>0)){
    if(grid_agg_res== (1/60)) the_breaks_baseline<-   c(0, round(exp(seq(3, 8, by=0.6)),1), 10000)  # if 1 minute  i.e. 0.16 degree
    if(grid_agg_res== (3/60)) the_breaks_baseline<-   c(0, round(exp(seq(3, 8, by=0.6)),1), 10000)    # if 3 minutes i.e. 0.05 degrees
    if(grid_agg_res== 0.01)   the_breaks_baseline<-   c(0, round(exp(seq(3, 8, by=0.6)),1), 10000)  # less than 1 minute  i.e. 0.01 degree
  }
  if(grepl("CPUF",a_nametype) || grepl("VPUE",a_nametype) ||  grepl("VPUF",a_nametype) ||
      grepl("CPUEallsp",a_nametype) || grepl("CPUFallsp",a_nametype) || grepl("VPUEallsp",a_nametype) || grepl("VPUFallsp",a_nametype)){
   if(grid_agg_res== (1/60)) the_breaks_baseline<-     c(0, round(exp(seq(-1, 1.7, by=0.25)),2), 10000)   # if 1 minute  i.e. 0.16 degree
    if(grid_agg_res== (3/60)) the_breaks_baseline<-   c(0, round(exp(seq(-1, 1.7, by=0.25)),2), 10000)    # if 3 minutes i.e. 0.05 degrees
    if(grid_agg_res== 0.01)   the_breaks_baseline<-   c(0, round(exp(seq(-1, 1.7, by=0.25)),2), 10000)   # less than 1 minute  i.e. 0.01 degree
         # c(0, round(exp(seq(-0, 4, by=0.5)),0), 10000)
  }
  if(grepl("CPUE",a_nametype) ){
   if(grid_agg_res== (1/60)) the_breaks_baseline<-    c(0, round(exp(seq(-0, 4, by=0.5)),0), 10000)   # if 1 minute  i.e. 0.16 degree
    if(grid_agg_res== (3/60)) the_breaks_baseline<-  c(0, round(exp(seq(-0, 4, by=0.5)),0), 10000)    # if 3 minutes i.e. 0.05 degrees
    if(grid_agg_res== 0.01)   the_breaks_baseline<-   c(0, round(exp(seq(-0, 4, by=0.5)),0), 10000)   # less than 1 minute  i.e. 0.01 degree
         # c(0, round(exp(seq(-0, 4, by=0.5)),0), 10000)
  }
  if(a_met=="PelagicGears" && (grepl("FPUCallsp",a_nametype) || grepl("FPUVallsp",a_nametype))){
   if(grid_agg_res== (1/60)) the_breaks_baseline<-  c(0, round(exp(seq(-1, 0.8, by=0.15)),2), 10000)       # if 1 minute  i.e. 0.16 degree
    if(grid_agg_res== (3/60)) the_breaks_baseline<-  c(0, round(exp(seq(-1, 0.8, by=0.15)),2), 10000)       # if 3 minutes i.e. 0.05 degrees
    if(grid_agg_res== 0.01)   the_breaks_baseline<-    c(0, round(exp(seq(-1, 0.8, by=0.15)),2), 10000) # less than 1 minute  i.e. 0.01 degree
                                  # c(0, round(exp(seq(-1, 1.7, by=0.3)),1), 10000)      

  }
  if(a_met=="BottomContactingGears" && (grepl("FPUCallsp",a_nametype) || grepl("FPUVallsp",a_nametype))){
   if(grid_agg_res== (1/60)) the_breaks_baseline<- c(0, round(exp(seq(-1, 0.8, by=0.15)),2), 10000)      # if 1 minute  i.e. 0.16 degree
    if(grid_agg_res== (3/60)) the_breaks_baseline<- c(0, round(exp(seq(-1, 0.8, by=0.15)),2), 10000)       # if 3 minutes i.e. 0.05 degrees
    if(grid_agg_res== 0.01)   the_breaks_baseline<-  c(0, round(exp(seq(-1, 0.8, by=0.15)),2), 10000)    # less than 1 minute  i.e. 0.01 degree
                                     # c(0, round(exp(seq(-0, 1.6, by=0.2)),1), 10000)    
  }
 
  #-------------------------
   a_func           <- "sum"
  if( grepl("FPUCallsp",a_nametype) || grepl("FPUVallsp",a_nametype) || grepl("CPUE",a_nametype) || grepl("CPUF",a_nametype) || grepl("VPUE",a_nametype) ||  grepl("VPUF",a_nametype) ||
      grepl("CPUEallsp",a_nametype) || grepl("CPUFallsp",a_nametype) || grepl("VPUEallsp",a_nametype) || grepl("VPUFallsp",a_nametype)) a_func <- "mean"    # do an average in cells when ratios provided in input
  if( grepl("sp_with_max_vpuf",a_nametype) || grepl("sp_with_max_cpue",a_nametype) || grepl("sp_with_max_cpuf",a_nametype) ||  grepl("sp_with_max_vpufswa",a_nametype)) {
              a_func <- function(x) {names(table(x))[which.max(table(x))]}  # find the most frequent sp
       }


           
  if(plot_per_c_square){
  #aggregate per c_square
    library(vmstools) # for c_square
    if(!any(colnames(my_data) %in% "c_square") ||
         grid_agg_res==0.05) my_data$c_square <- vmstools::CSquare (lon=my_data[,long], lat=my_data[,lat], degrees=grid_agg_res)

    # then, aggregate the data per c_square...
    my_data           <- aggregate(an(my_data[,a_nametype]), list(my_data$c_square), a_func, na.rm=TRUE)
    colnames(my_data) <- c("c_square", a_nametype)
    my_data          <- cbind.data.frame(my_data, CSquare2LonLat(my_data$c_square, grid_agg_res)) # get the mid point coordinates
 
    colnames(my_data) <- c("gridCellID", a_nametype, "mid_lat", "mid_lon")
  } else{
  #-------------------------
  #aggregate on a grid
    # Set grid
    library(vmstools)
    library(RColorBrewer)
    colintens             <- brewer.pal(9,"YlOrRd")
    resx <- grid_agg_res
    resy <- grid_agg_res
    grd <- createGrid(xlims,ylims,resx=resx, resy=resy, type="SpatialGrid", exactBorder=TRUE)

    # Grid all tacsatSweptArea data
    # Convert all tacsat points first to SpatialPoints
    coords <- SpatialPoints(cbind(SI_LONG=an(my_data[, long]), SI_LATI=an(my_data[, lat])))
    idx <- over(coords,grd)
    my_data$gridCellID <- idx

    # Remove records that are not in the study area
    my_data <- subset(my_data,is.na(gridCellID)==FALSE)

    # then, do an agg
    if(grepl("sp_with_max_vpuf",a_nametype) || grepl("sp_with_max_cpue",a_nametype) || grepl("sp_with_max_cpuf",a_nametype) ||  grepl("sp_with_max_vpufswa",a_nametype)){
        my_data[my_data[,"CPUEallsp"]==0,a_nametype] <- as.character(0) # debug misclassified (false positive in pelagic)
        my_data           <- aggregate(as.character(my_data[,a_nametype]), list(my_data$gridCellID), a_func)   
        } else{
        my_data           <- aggregate(an(my_data[,a_nametype]), list(my_data$gridCellID), a_func, na.rm=TRUE)    # as.numeric
       }
       
    colnames(my_data)[1:2] <- c("gridCellID", a_nametype)
    my_data           <- cbind(my_data, mid_lat=an(coordinates(grd)[my_data$gridCellID,2]), mid_lon=an(coordinates(grd)[my_data$gridCellID,1]))

   #plot(my_data$mid_lon, my_data$mid_lat, col=Satellite.Palette.baseline(length(the_breaks_baseline[-1]))[as.numeric(cut(as.numeric(my_data[,a_nametype]),breaks=the_breaks_baseline, right=FALSE))], pch="+")
  }
   
   

       
   #-------------------------
   #  transform to SAR
  if(a_nametype %in% c('SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')){
     my_data[,a_nametype]  <- replace (my_data[,a_nametype],
                                                 my_data[,a_nametype]>the_breaks_baseline[length(the_breaks_baseline)],
                                                 the_breaks_baseline[length(the_breaks_baseline)])
    # caution, transform to SAR for the plot grid resolution
    my_data$cell_area <- (cos(my_data$mid_lat *pi/180) * 111.325 )*grid_agg_res  * (111*grid_agg_res) # 0.05 degree is 3 minutes
    my_data[,a_nametype]  <- round(my_data[,a_nametype])  / an(my_data$cell_area)  # transform to SAR
  }
   
  # plot(my_data$mid_lon, my_data$mid_lat, col=Satellite.Palette.baseline(length(the_breaks_baseline[-1]))[cut(as.numeric(my_data[,a_nametype]),breaks=the_breaks_baseline, right=FALSE)], pch="+")
  
   spdata                <- unique(my_data[,c("gridCellID", "mid_lat", "mid_lon")])  # make sure to remove duplicates...(but should already have vanished from the prior aggregation)
   rownames(spdata) <- spdata$gridCellID


  #-------------------------
   r1 <- rbind(x = c(-1, -1, 1,  1, -1) * grid_agg_res /2,  # 1 by 1 minute or 3 by 3
                y = c(-1,  1, 1, -1, -1) * grid_agg_res /2)

 library(sp)
 spatialLookup <-
      SpatialPolygons(
        lapply(1:nrow(spdata),
               function(i) {
                 Polygons(
                   list(
                     Polygon(
                       t(r1 + c(spdata$mid_lon[i], spdata$mid_lat[i]))
                     )
                   ), ID = as.character(spdata$gridCellID[i]))
                 }
               )
        )

  # now assign data to the columns
  out      <- spatialLookup[as.character(my_data$gridCellID),]
  out$data <- my_data[,a_nametype]

   
  # save output
  #rgdal::writeOGR(out,  paste0("a_shp", year), 
  #                    driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # or:
  # r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
  #  some_coords <- SpatialPoints(cbind(SI_LONG=vmspp_this$mid_lon, SI_LATI=vmspp_this$mid_lat))
  ##  rstr        <- rasterize(x=some_coords, y=r, field=vmspp_this$landings_from_cell*1000, fun="sum")  # converted in kilo 
  # rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # European EEA projection
  #  rstr_eea[is.na(rstr_eea)] <- -999  # arbitrary code, to get rid of true 0s in GIS
  #  rstr_eea[rstr_eea<0.001]  <- -999
  #  writeRaster(rstr_eea, file.path(outPath, paste("DispatchedStecfLandingsOnVMS4", a_species, sep='')), format = "GTiff", overwrite=TRUE)


   if(grepl("sp_with_max_vpuf",a_nametype) || grepl("sp_with_max_cpue",a_nametype) || grepl("sp_with_max_cpuf",a_nametype) ||  grepl("sp_with_max_vpufswa",a_nametype)){
      some_color_species<- c("COD"="#E69F00", "CSH"="hotpink", "DAB"="#56B4E9", "ELE"="#F0E442", "FLE"="green",
                       "HAD"="#0072B2", "HER"="mediumorchid4", "HKE"="#CC79A7","HOM"="indianred2", "LEM"="#EEC591",
                        "MAC"="#458B00", "MON"="#F0F8FF", "MUS"="black", "NEP"="#e3dcbf", "NOP"="#CD5B45", "PLE"="lightseagreen",
                        "POK"="#6495ED", "PRA"="#CDC8B1", "SAN"="#00FFFF", "SOL"="#8B0000", "SPR"="#008B8B", "TUR"="#A9A9A9", "WHB"="#76a5c4",
                         "WIT"="red", "WHG"="yellow", "OTH"="blue")
     Stocknames <-  c("COD"="cod", "COD.nsea"="North Sea cod", "COD.2224"="west Baltic cod", "CSH"="brown shrimp", "DAB"="dab", "ELE"="eel", "FLE"="flounder",
                        "HAD"="haddock",  "HAD.nsea"="North Sea haddock", "HER"="herring", "HER.nsea"="North Sea herring", "HKE"="hake","HKE.nsea"="North Sea hake", 
                        "HOM"="Horse mackerel", "LEM"="lemon sole",
                         "MAC"="mackerel", "MAC.nsea"="North Sea mackerel", "MON"="monkfish", "MUS"="mussel", "NEP"="Nephrops", "NEP.kask"="Kattegat Nephrops", 
                         "NOP"="Norway pout", "PLE"="plaice", "PLE.nsea"="North Sea plaice", "PLE.2123"="Kattegat plaice",
                         "POK"="saithe", "POK.nsea"="North Sea saithe", "PRA"="boreal shrimp", "PRA.nsea"="North Sea boreal shrimp", "SAN"="sandeel", "SOL"="sole","SOL.nsea"="North Sea sole",  
                         "SOL.2024"="Baltic sole", "SPR"="sprat", "SPR.2232"="Baltic sprat", "TUR"="turbot", "WHB"="blue whiting",
                          "WIT"="witch flounder", "WHG"="whiting", "OTH"="other",
                          "COC"="cokle", "OYF"="oyster", "LUM"="lumpfish", "SAL"="salmon", "BLL"="brill", "GAR"="garfish")                         
    some_color_speciesnames <- c("cod"="#E69F00", "brown shrimp"="hotpink", "dab"="#56B4E9", "eel"="#F0E442", "flounder"="green",
                        "haddock"="#0072B2", "herring"="mediumorchid4", "hake"="#CC79A7","Horse mackerel"="indianred2", "lemon sole"="#EEC591",
                         "mackerel"="#458B00", "monkfish"="#F0F8FF", "mussel"="black", "Nephrops"="#e3dcbf", "Norway pout"="#CD5B45", "plaice"="lightseagreen",
                         "saithe"="#6495ED", "boreal shrimp"="#CDC8B1", "sandeel"="#00FFFF", "sole"="#8B0000", "sprat"="#008B8B", "turbot"="#A9A9A9", "blue whiting"="#76a5c4",
                          "witch flounder"="red", "whiting"="yellow", "other"="blue",
                          "cockle"="#108291", "oyster"="#6a9110", "lumpfish"="red", "salmon"="#c2a515", "brill"="cyan", "garfish"="grey")
                     
                         
    out$color <- some_color_species[as.character(out$data)] 
     } else{
      out$color <- Satellite.Palette.baseline(length(the_breaks_baseline[-1])) [cut(out$data, the_breaks_baseline)]
     }
     

  # create a rectangle from xlim and ylim to fix bad limits
  library(raster)
  bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
  proj4string(bb) <- proj4string(out)
  #plot(bb, add=FALSE, col=NA)

  # coastline cropped
  sh_coastlines              <- readShapePoly(file.path(getwd(), "ne_50m_ocean","ne_50m_ocean.shp"))
  proj4string(sh_coastlines) <- CRS("+proj=longlat +datum=WGS84")
  CP                         <- as(extent(xlims[1], xlims[2], ylims[1], ylims[2]), "SpatialPolygons")
  proj4string(CP)            <- CRS("+proj=longlat +datum=WGS84")
  library(rgeos)
  sh_coastlines_clipped      <- gIntersection(sh_coastlines, CP, byid=TRUE)  # clip
  
  # plot
  fao_areas_lambert <- spTransform(fao_areas, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
  if(use_fao_areas)
  {
     #plot (fao_areas_lambert, add=FALSE) 
 
     ## plot + graticules to the plot
     library(sf)
     mrc_proj <- st_as_sf(fao_areas_lambert)
     mrc <- st_as_sf(fao_areas)
     plot(mrc_proj["geometry"], axes = TRUE, cex.axis=2, graticule = st_crs(mrc), key.pos = NULL, reset = FALSE) #  key.pos = NULL, reset = FALSE to avoid pbl with par(mfrow)
     #g=st_graticule(mrc_proj["geometry"])
     #plot(g[1], col="grey", add=TRUE)
  } else{
    plot (sh_coastlines_clipped, add=FALSE, axes=FALSE)
  }
 
# invisible(lapply(seq_len(nrow(g)), function(i) {
#if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
#  text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
#    srt = g$angle_start[i], pos = 2, cex = 1.7)
#if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
#  text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
#    srt = g$angle_start[i] - 90, pos = 1, cex = 1.7)
#if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
#  text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
#    srt = g$angle_end[i], pos = 4, cex = 1.7)
#if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
#  text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
#    srt = g$angle_end[i] - 90, pos = 3, cex = 1.7)
#}))
 

  library(rgeos)
  #out2 <- gIntersection(out, bb, byid=TRUE) not returning the data in it(!)...so substitute with:
  out2 <- raster::intersect(out, bb)
  crs(out2) <- "+proj=longlat +datum=WGS84"
  out2 <- spTransform(out2, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
  plot(out2, col =  out2$color, border=NA, xlim = xlims, ylim=ylims, add=TRUE)
 
  plot (fao_areas_lambert, add=TRUE) 
 
  #plot (sh_coastlines_clipped, add=TRUE)
  sh_coastlines              <- spTransform(sh_coastlines, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
  plot (sh_coastlines, add=TRUE)
  
  if(!is.null(spatial_polys)) plot(spatial_polys, add=TRUE, density=10)
 
  #mtext(side=3, namesce[count], cex=1.5, line=0.5)
  #mtext(side=3, someletters[count], cex=2, line=1, adj=0)
  mtext("Latitude", 1, line=2, cex=1.5, outer=TRUE)
  mtext(side=2,"Longitude",line=2, cex=1.5, outer=TRUE)
  #axis(1, cex.axis=1.0)
  #axis(2, las=2, cex.axis=1.0)
  box()
  a_title <- gsub(".tif", "", namefile)
  #if(length(nametype)==1) title(a_title) 
     
  x = c(xlims[1]-5+0.1*(xlims[2]-xlims[1]), xlims[1]-5+0.11*(xlims[2]-xlims[1]), xlims[1]-5+0.11*(xlims[2]-xlims[1]), xlims[1]-5+0.1*(xlims[2]-xlims[1]))
  y = c(ylims[1]-0.5+0.1*(ylims[2]-ylims[1]), ylims[1]-0.5+0.7*(ylims[2]-ylims[1]), ylims[1]-0.5+0.5*(ylims[2]-ylims[1]), ylims[1]-0.7+0.1*(ylims[2]-ylims[1]))
  the_breaks_leg <-NULL
  if(grepl("VPUFSWAallsp", a_nametype)) a_title_leg <- c("euro per area swept") 
  if(grepl("LE_VPUF", a_nametype)) a_title_leg <- c("euro per litre") 
  if(grepl("VPUFallsp", a_nametype)) a_title_leg <- c("euro per litre") 
  if(grepl("LE_CPUF", a_nametype)) a_title_leg <- c("kg per litre") 
  if(grepl("CPUFallsp", a_nametype)) a_title_leg <- c("kg per litre") 
  if(grepl("FPUCallsp", a_nametype)) a_title_leg <- c("litre per kg catch") 
  if(grepl("FPUVallsp", a_nametype)) a_title_leg <- c("litre per euro catch") 
  if(grepl("LE_CPUE", a_nametype)) a_title_leg <- c("kg per effective effort") 
  if(grepl("CPUEallsp", a_nametype)) a_title_leg <- c("kg per effective effort") 
  if(grepl("LE_VPUE", a_nametype)) a_title_leg <- c("euro per effective effort") 
  if(grepl("VPUEallsp", a_nametype)) a_title_leg <- c("euro per effective effort") 
  if(grepl("LE_KG", a_nametype)) a_title_leg <- c("kg") 
  if(grepl("sp_with_max_vpuf", a_nametype)) a_title_leg <- c("Species with max. VPUF") 
  if(grepl("sp_with_max_cpue", a_nametype)) a_title_leg <- c("Species with max. CPUE") 
  if(grepl("sp_with_max_cpuf", a_nametype)) a_title_leg <- c("Species with max. CPUF") 
  if(grepl("sp_with_max_vpufswa", a_nametype)) a_title_leg <- c("Species with max. VPUFSWA") 
  #legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
  #       limits="", title=a_title_leg,
  #       legend= the_breaks_leg,
  #       cex=1.3, col="black") 
   if(grepl("sp_with_max_vpuf",a_nametype) || grepl("sp_with_max_cpue",a_nametype) || grepl("sp_with_max_cpuf",a_nametype) ||  grepl("sp_with_max_vpufswa",a_nametype)){

      name_leg <- names(some_color_speciesnames[Stocknames[unique(out2$data)]])  ;   name_leg <- name_leg[!is.na(name_leg)]
      fill_leg <- some_color_speciesnames[Stocknames[unique(out2$data)]]    ;   fill_leg <- fill_leg[!is.na(fill_leg)]
      legend("topright", legend=name_leg, fill=fill_leg, bty="o", border=NA,  ncol=4, box.col="white", title=a_title_leg)       
   
   } else{
      for(i in 1: length(the_breaks_baseline[-1])){ if(the_breaks_baseline[i]>1) {the_breaks_leg[i] <- round(the_breaks_baseline[i],1)} else{the_breaks_leg[i]<- the_breaks_baseline[i]}}
      legend("topright", legend=the_breaks_leg, fill=Satellite.Palette.baseline(length(the_breaks_baseline[-1])), bty="o", border=NA,  ncol=4, box.col="white", title=a_title_leg, cex=2.2)       
   }
  box()
     
    } # end a_nametype  
  dev.off()
  
 return()
}

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## CALLS ##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  plot_per_c_square <- FALSE
    xrange  <- c(-30,50) # ALL
    yrange  <- c(30,81) # ALL
 



########
########
########
########
########
########
# AVERAGE OVER THE ENTIRE PERIOD 2005-2019 - BOTTOM CONTACTING GEARS
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndRatiosForBottContact_",2012,".RData") ))  # aggResult
# load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 #load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2019,".RData") ))  # aggResult
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndRatiosForBottContact_",2012,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 plot_per_c_square <- FALSE
 library(rgdal);  library(raster); fao_areas <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 agg_dem <- NULL
 an <- function(x) as.numeric(as.character(x))
 for (y in years){
    #load(file.path(getwd(),  "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",y,".RData") )) # aggResult
    load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndRatiosForBottContact_",y,".RData") ))  # aggResult
    
    # litre per kilo catch
    aggResult$FPUCallsp <- an(aggResult$LE_KG_LITRE_FUEL)/(an(aggResult$KKGallsp)*1000) 
    aggResult  <- aggResult [!is.infinite(aggResult$FPUCallsp),] 

    # litre per euro catch
    aggResult$FPUVallsp <-  an(aggResult$LE_KG_LITRE_FUEL)/(an(aggResult$KEUROallsp)*1000)
    aggResult  <-aggResult [!is.infinite(aggResult$FPUVallsp),]

    aggResult <- aggResult[aggResult$FPUCallsp<10000,]   # clean up a bit for outliers coming from merged VMS pts with no logbk catch
    
    agg_dem <- rbind.data.frame(agg_dem, 
               cbind.data.frame(year=y,aggResult[,c("LE_MET","CELL_LONG", "CELL_LATI",
                                                      "CPUEallsp","CPUFallsp","VPUFallsp", "VPUFSWAallsp", "FPUCallsp", "FPUVallsp",
                                                       "sp_with_max_vpuf", "sp_with_max_cpue", "sp_with_max_cpuf", "sp_with_max_vpufswa")])
               )  
 }
 
 
 #PER METIER
if(FALSE) for (a_met in metiers){
   quickmap (namefile = paste0("CPUEallspAndCoFor_",a_met,"_",years[1],"-",years[length(years)],".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     aggResult   =  agg, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("CPUEallsp","FPUCallsp","VPUFallsp"), ## 3 plots
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=3000,
                     a_height=3000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots_maps_met")
         )
 }
 
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 #!PAPER
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 #ALL METIERS POOLED
 years <- 2005:2019
 aggall_dem <- agg_dem
 aggall_dem$LE_MET <- "BottomContactingGears"
 quickmap (namefile = paste0("CPUEallspAndCoFor_","BottomContactingGears","_",years[1],"-",years[length(years)],".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     aggResult   =  aggall_dem, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("CPUEallsp","FPUCallsp","VPUFallsp"), ## 3 plots
                     a_met    = "BottomContactingGears",
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots_maps_met")
         ) 
 
 
 
 
 
########
########
########
########
########
########
# AVERAGE OVER THE ENTIRE PERIOD 2005-2019 -PELAGIC GEARS
 #load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2012,".RData") ))  # aggResult
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndRatiosForPel_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 #load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2019,".RData") ))  # aggResult
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndRatiosForPel_",2019,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 plot_per_c_square <- FALSE
 library(rgdal);  library(raster); fao_areas <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 years <- 2005:2019
 an <- function(x) as.numeric(as.character(x))
 agg_pel <- NULL
 for (y in years){
    #load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") )) # aggResult   
    load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndRatiosForPel_",y,".RData") )) # aggResult   
     
     # litre per kilo catch
    aggResult$FPUCallsp <- an(aggResult$LE_KG_LITRE_FUEL)/(an(aggResult$KKGallsp)*1000) 
    aggResult  <- aggResult [!is.infinite(aggResult$FPUCallsp),] 

    # litre per euro catch
    aggResult$FPUVallsp <-  an(aggResult$LE_KG_LITRE_FUEL)/(an(aggResult$KEUROallsp)*1000)
    aggResult  <-aggResult [!is.infinite(aggResult$FPUVallsp),]

    aggResult <- aggResult[aggResult$FPUCallsp<10000,]   # clean up a bit for outliers coming from merged VMS pts with no logbk catch

    agg_pel <- rbind.data.frame(agg_pel, 
               cbind.data.frame(year=y,aggResult[,c("LE_MET","CELL_LONG", "CELL_LATI",
                                                      "CPUEallsp","CPUFallsp","VPUFallsp", "VPUFSWAallsp", "FPUCallsp", "FPUVallsp",
                                                       "sp_with_max_vpuf", "sp_with_max_cpue", "sp_with_max_cpuf", "sp_with_max_vpufswa")])
               )  
 }
 
 #PER METIER
if(FALSE) for (a_met in metiers){
   quickmap (namefile = paste0("CPUEallspAndCoFor_",a_met,"_",years[1],"-",years[length(years)],".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     aggResult   =  agg_pel, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("CPUEallsp","FPUCallsp","VPUFallsp"), ## 3 plots
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020_pel", "output_plots_maps_met")
         )
 }
 
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 #!PAPER
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 #ALL METIERS POOLED
 aggall_pel <- agg_pel
 aggall_pel$LE_MET <- "PelagicGears"
 quickmap (namefile = paste0("CPUEallspAndCoFor_","PelagicGears","_",years[1],"-",years[length(years)],".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     aggResult   =  aggall_pel, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("CPUEallsp","FPUCallsp","VPUFallsp"), ## 4 plots
                     a_met    = "PelagicGears",
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020_pel", "output_plots_maps_met")
         ) 

 
 
 
 
 
 
 
 
########
########
########
########
########
########
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## PLOT PER CELL THE SEPCIES COLOR CODE WITH THE LOCALLY HIGHEST VPUF etc.
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
 #AVERAGE OVER THE ENTIRE PERIOD 2005-2019 - BOTTOM CONTACTING GEARS
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2019,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 plot_per_c_square <- FALSE
 library(rgdal);  library(raster); fao_areas <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 agg <- NULL
 for (y in years){
    load(file.path(getwd(),  "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",y,".RData") )) # aggResult
    agg <- rbind.data.frame(agg, 
               cbind.data.frame(year=y,aggResult[,c("LE_MET","CELL_LONG", "CELL_LATI",
                                                      "CPUEallsp","CPUFallsp","VPUFallsp", "VPUFSWAallsp",
                                                       "sp_with_max_vpuf", "sp_with_max_cpue", "sp_with_max_cpuf", "sp_with_max_vpufswa")])
               )  
 }
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 #!PAPER
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 aggall <- agg
 aggall$LE_MET <- "BottomContactingGears"
 library(maptools)
 library(sf)
 quickmap (namefile = paste0("ColorSpCodeMapFor_","BottomContactingGears","_",years[1],"-",years[length(years)],".tif"),
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ),
                     #nameobj  = "aggResult",
                     aggResult   =  aggall, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ),
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("sp_with_max_cpue","sp_with_max_cpuf","sp_with_max_vpuf"), ## 3 plots
                     a_met    = "BottomContactingGears",
                     long     = "CELL_LONG",
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots_maps_met")
         )

    

 aggall <- agg
 aggall$LE_MET <- "BottomContactingGears"
 quickmap (namefile = paste0("ColorSpCodeMapFor_","BottomContactingGears","_",years[1],"-",years[length(years)],"_maxvpuf.tif"),
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ),
                     #nameobj  = "aggResult",
                     aggResult   =  aggall, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ),
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("sp_with_max_vpuf"), ## 1 plots
                     a_met    = "BottomContactingGears",
                     long     = "CELL_LONG",
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots_maps_met")
         )



########
########
########
########
########
########
# AVERAGE OVER THE ENTIRE PERIOD 2005-2019 -PELAGIC GEARS
 #load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2012,".RData") ))  # aggResult
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndRatiosForPel_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 #load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2019,".RData") ))  # aggResult
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndRatiosForPel_",2012,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 plot_per_c_square <- FALSE
 library(rgdal);  library(raster); fao_areas <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 years <- 2005:2019
agg <- NULL
 for (y in years){
   load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndRatiosForPel_",y,".RData") ))  # aggResult
    agg <- rbind.data.frame(agg, 
               cbind.data.frame(year=y,aggResult[,c("LE_MET","CELL_LONG", "CELL_LATI",
                                                      "CPUEallsp","CPUFallsp","VPUFallsp", "VPUFSWAallsp",
                                                       "sp_with_max_vpuf", "sp_with_max_cpue", "sp_with_max_cpuf", "sp_with_max_vpufswa")])
               )  
   }
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
 #!PAPER
 #!#!#!#!#!#!#!#!
 #!#!#!#!#!#!#!#!
aggall <- agg
 aggall$LE_MET <- "PelagicGears"
 quickmap (namefile = paste0("ColorSpCodeMapFor_","PelagicGears","_",years[1],"-",years[length(years)],".tif"),
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ),
                     #nameobj  = "aggResult",
                     aggResult   =  aggall, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ),
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("sp_with_max_cpue","sp_with_max_cpuf","sp_with_max_vpuf"), ## 3 plots
                     a_met    = "PelagicGears",
                     long     = "CELL_LONG",
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020_pel", "output_plots_maps_met")
         )


aggall <- agg
 aggall$LE_MET <- "PelagicGears"
 quickmap (namefile = paste0("ColorSpCodeMapFor_","PelagicGears","_",years[1],"-",years[length(years)],"_maxvpuf.tif"),
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ),
                     #nameobj  = "aggResult",
                     aggResult   =  aggall, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ),
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("sp_with_max_vpuf"), ## 1 plot
                     a_met    = "PelagicGears",
                     long     = "CELL_LONG",
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=5000,
                     a_height=5000,
                     output_dir=file.path(getwd(), "outputs2020_pel", "output_plots_maps_met")
         )





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!DIVERSE HELFUL PLOTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # plot average SAR
  quickmap (namefile =  paste0("SAR_Average_2017-2019", ".tif") , 
                     a_file   = file.path(getwd(), "outputs2020", "AggregatedSweptArea_2017-2019.RData"), 
                     nameobj  = "aggResult",
                     a_unit   = 3, # divide by 3 because 3y
                     nametype = "SWEPT_AREA_KM2",
                     a_met    = NULL,
                     long     = "SI_LONG", 
                     lat      = "SI_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=FALSE,
                     fao_namefile= file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"),
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots")
   )

 
 # spatial SAR
 years <- 2012:2019
 for (y in years){
    #load(file.path(getwd(), y, "tacsatSweptArea.RData"))  # aggResult
     quickmap (namefile =  paste0("SAR_", y, ".tif"), 
                     a_file   = file.path(getwd(),"outputs2020", y, "tacsatSweptArea.RData"),  
                     nameobj  = "tacsatSweptArea",
                     a_unit   = 1, # divide by 1 because 1y
                     nametype = "SWEPT_AREA_KM2",
                     a_met    = NULL,
                     long     = "SI_LONG", 
                     lat      = "SI_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=FALSE,
                     fao_namefile= file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"),
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots") 
         )
 
 }
 
 
  
  
  # spatial landings
 spp <- c("PLE", "COD", "NEP", "LITRE_FUEL")
 years <- 2017:2019
 for (sp in spp){
 for (y in years){
 
 quickmap (namefile = paste0("LE_KG_",sp,"_",y, ".tif"), 
                     a_file   = file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlus_",y,".RData") ), 
                     nameobj  = "aggResult",
                     a_unit   = 1, # divide by 1 because only one y
                     nametype = paste0("LE_KG_",sp),
                     a_met    = NULL,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=FALSE,
                     fao_namefile= file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"),
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots")
         )
 }}



 # spatial SAR per metier
 years <- 2017:2019
 metiers <- c("OT_SPF", "OT_DMF","OT_DMF_PEL", "SDN_DEM", "OT_MIX_NEP", "TBB_CRU","SSC_DEM","TBB_DMF","DRB_MOL","OT_CRU","OT_DEM_PEL")
 #load(file.path(getwd(),  paste0("AggregatedSweptArea_2017-2019.RData")))  # aggResult
 for (a_met in metiers){
   quickmap (namefile = paste0("SAR_Average_2017-2019_",a_met,".tif"), 
                     a_file   = file.path(getwd(), "outputs2020",  paste0("AggregatedSweptArea_2017-2019.RData") ), 
                     nameobj  = "aggResult", 
                     a_unit   = 3, # divide because 3y agg
                     nametype = "SWEPT_AREA_KM2",
                     a_met    = a_met,
                     long     = "SI_LONG", 
                     lat      = "SI_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=FALSE,
                     fao_namefile= file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"),
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots")
         )
 }
 
 # spatial SAR per metier per species
 #load(file.path(getwd(),  paste0("AggregatedSweptAreaPlus_",y,".RData") ))  # aggResult
 metiers <- c("OT_SPF", "OT_DMF","OT_DMF_PEL", "SDN_DEM", "OT_MIX_NEP", "TBB_CRU","SSC_DEM","TBB_DMF","DRB_MOL","OT_CRU","OT_DEM_PEL")
 spp <- c("PLE", "COD", "NEP","LITRE_FUEL")
 years <- 2017:2019
 for (y in years){
    load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") )) # aggResult
 for (sp in spp){
 for (a_met in metiers){
   quickmap (namefile = paste0("LE_KG_",sp,"_",a_met,"_",y, ".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     my_data  =  "aggResult",
                     a_unit   = 1, # 1 because 1y agg
                     nametype =paste0("LE_KG_",sp),
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65), 
                     use_fao_areas=FALSE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots", "output_plots_maps_agg")
         )
 }}}
 
 
# fully fledged maps PER SPECIES, here for PELAGIC GEARS
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2019,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 spp <- c("HER","MAC","NOP","SAN","SPR","WHB")
 # note that some association met-spp do not make sense but exported anyway...
 plot_per_c_square <- FALSE
 years <- 2012:2019
 for (y in years){
    load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") )) # aggResult
 for (sp in spp){
 for (a_met in metiers){
   quickmap (namefile = paste0("LE_VPUF_",sp,"_",a_met,"_",y, ".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     my_data  =  "aggResult",
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =paste0("LE_VPUF_",sp), ## !!VPUFs!!
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020_pel", "output_plots_maps")
         )
 }}}
 
     

# fully fledged maps PER SPECIES, here for BOTTOM CONTACTING GEARS
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2019,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 spp <- c("COD", "CSH","DAB","ELE","FLE","HAD","HER","HKE","HOM","LEM","MAC","MON","MUS","NEP","NOP","PLE","POK","PRA", "SAN","SOL","SPR","TUR","WHB","WIT","WHG","OTH")
 # note that some association met-spp do not make sense but exported anyway...
 plot_per_c_square <- FALSE
 years <- 2012:2019
 for (y in years){
    load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") )) # aggResult
 for (sp in spp){
 for (a_met in metiers){
   quickmap (namefile = paste0("LE_VPUF_",sp,"_",a_met,"_",y, ".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     my_data  =  "aggResult",
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =paste0("LE_VPUF_",sp), ## !!VPUFs!!
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     fao_areas= fao_areas,
                     fao_namefile= file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"),
                     spatial_polys=NULL,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots_maps")
         )
 }}}
 

# fully fledged maps PER METIER, here for BOTTOM GEARS
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 load(file.path(getwd(), "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",2019,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 plot_per_c_square <- FALSE
 library(rgdal);  library(raster); fao_areas <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 years <- 2012:2019
 for (y in years){
    load(file.path(getwd(),  "outputs2020", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForBottContact_",y,".RData") )) # aggResult
 for (a_met in metiers){
   quickmap (namefile = paste0("CPUEallspAndCoFor_",a_met,"_",y, ".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     aggResult   =  aggResult, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("CPUEallsp","CPUFallsp","VPUFallsp"), ## 3 plots
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=3000,
                     a_height=3000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots_maps_met")
         )
 }}
 
 
# fully fledged maps PER METIER, here for PELAGIC GEARS
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2012,".RData") ))  # aggResult
 metiers <-   as.character(unique(aggResult$LE_MET))
 load(file.path(getwd(), "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",2019,".RData") ))  # aggResult
 metiers <-  unique(c(metiers, as.character(unique(aggResult$LE_MET))))
 metiers <- metiers[!grepl("NA", metiers)]
 plot_per_c_square <- FALSE
 library(rgdal);  library(raster); fao_areas <- readOGR(file.path(getwd(), "FAO_AREAS", "FAO_AREAS.shp"))
 years <- 2012:2019
 for (y in years){
    load(file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") )) # aggResult
 for (a_met in metiers){
   quickmap (namefile = paste0("CPUEallspAndCoFor_",a_met,"_",y, ".tif"), 
                     #a_file   = file.path(getwd(),  "outputs2020_pel", paste0("AggregatedSweptAreaPlusMet6AndVsizeAndRatiosForPel_",y,".RData") ), 
                     #nameobj  = "aggResult", 
                     aggResult   =  aggResult, # file.path(getwd(),"outputs2020", paste("AggregatedSweptAreaPlus_2019.RData") ), 
                     a_unit   = 1, # 1 because 1 year agg
                     nametype =c("CPUEallsp","CPUFallsp","VPUFallsp"), ## 3 plots
                     a_met    = a_met,
                     long     = "CELL_LONG", 
                     lat      = "CELL_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {3/60},
                     xlims    = c(-7,25),
                     ylims    = c(50,65),
                     use_fao_areas=TRUE,
                     fao_areas= fao_areas,
                     spatial_polys=NULL,
                     a_width=3000,
                     a_height=3000,
                     output_dir=file.path(getwd(), "outputs2020_pel", "output_plots_maps_met")
         )
 }}
 





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!CASE STUDY!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# OWF Krieger Flaks 
# UTM ETRS89 zone32N
# ID East North
dd <- c(1, 739829, 6106961,
2, 745344, 6109440,
3, 745949, 6105854,
4, 746568, 6104243,
5, 746775, 6103995,
6, 747876, 6103610,
7,  748509, 6102922,
8, 748647, 6100719,
9, 749900, 6098597,
10, 747313, 6098149,
11, 741529, 6098509,
12, 739829, 6102553,
13, 746387, 6109914,
14, 752519, 6112702,
15, 758587, 6109053,
16, 764491, 6103428,
17, 762879, 6100851,
18, 755554, 6099575,
19, 755568, 6103263,
20, 755172, 6103830,
21, 752020, 6105730,
22, 750313, 6109047,
23, 750029, 6109332,
24, 749862, 6109393)
dd <- matrix(dd, ncol=3, byrow=TRUE)
colnames(dd) <- c("ID", "East", "North")
poly1 <- dd[1:12,]  # east part
poly2 <- dd[13:24,] # west part

x <- poly1[,'East']
y <- poly1[,'North']
sp1 <- Polygon(cbind(  c(x,x[1]),
                   c(y, y[1])
                    ))
sps1 <- Polygons(list(sp1), ID=paste("sp1")) 

x <- poly2[,'East']
y <- poly2[,'North']
sp2 <- Polygon(cbind(  c(x,x[1]),
                   c(y, y[1])
                    ))
sps2 <- Polygons(list(sp2), ID=paste("sp2")) 

spp <- SpatialPolygons(list(sps1,sps2), 1:2)
plot(spp) ; axis(1), axis(2)

library(raster)
library(rgdal)
library(maptools)                                        
UTMzone <- 32
projection(spp)  <- CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='') )
spp              <- spTransform(spp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(spp) ; axis(1); axis(2)
  
 years <- 2012:2019
 for (y in years){
 quickmap (namefile =  paste0("SAR_KriegersFlaks", y, ".tif"), 
                     a_file   = file.path(getwd(),  "outputs2020", y, "tacsatSweptArea.RData"), 
                     nameobj  = "tacsatSweptArea", 
                     a_unit   = 1, # divide by 1 because 1y
                     nametype = "SWEPT_AREA_KM2",
                     a_met    = NULL,
                     long     = "SI_LONG", 
                     lat      = "SI_LATI",
                     plot_per_c_square =FALSE,
                     grid_agg_res =if(plot_per_c_square){0.05} else {1/60},
                     xlims    = c(12,14), # around Kriegers Flaks
                     ylims    = c(54.5,55.5), # around Kriegers Flaks
                     spatial_polys=spp,
                     a_width=6000,
                     a_height=6000,
                     output_dir=file.path(getwd(), "outputs2020", "output_plots")
         )
 }
 
 
    
 