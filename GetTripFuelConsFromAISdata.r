

setwd (file.path("D:","FBA","BENTHIS_2020"))   # adapt to your need

# build a single dataset (from all AIS sampled small vessels with logbooks)
ais <- read.csv(file=file.path(getwd(), "AIS_data", "Bomtrawl hesterejer_2018.csv"), header=TRUE)
ais <- cbind.data.frame(ais, Year="2018")
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Bomtrawl hesterejer_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Bomtrawl_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Bundtrawl_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Bundtrawl_2019.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2019"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Bundtrawl_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Garn_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Garn_2019.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2019"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Garn_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Liner_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Liner_2019.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2019"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Liner_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Muslingeskrabere_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Muslingeskrabere_2019.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2019"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Muslingeskrabere_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Pelagisk trawl_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Pelagisk trawl_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Pelagisk trawl_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Ruser og Tejner_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Ruser og Tejner_2019.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2019"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Ruser og Tejner_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Snurrevod_2018.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2018"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Snurrevod_2019.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2019"))
dd <- read.csv(file=file.path(getwd(), "AIS_data", "Snurrevod_2020.csv"), header=TRUE)
ais <- rbind.data.frame(ais, cbind.data.frame(dd, Year="2020"))

# remove unecessary Speed cat
ais <- ais[ais$fart_grp %in% c('[0,1)', '[1,2)', '[2,3)', '[3,4)', '[4,5)', '[5,6)',
                                      '[6,7)', '[7,8)', '[8,9)', '[9,10)', '[10,11)', '[11,12)', '[12,13)',
                                       '[13,14)', '[14,15)', '[15,16)', '[16,17)', '[17,18)', '[18,19)', '[19,20)'),]
ais$fart_grp <- factor(ais$fart_grp)

# keep small vessel only
ais <- ais[as.numeric(as.character(ais$oal)) < 12,]


# get a standardized average profile over vessels and years with standardisation to 1
ais_profile <- aggregate(ais$no_points, list(ais$metier_level6_ret, ais$fart_grp), mean, na.rm=TRUE)
colnames(ais_profile) <- c("Level6","Speed", "Prop")
dd <- lapply(split(ais_profile, f=ais_profile$Level6), function(x){
        x$Prop <- x$Prop/sum(x$Prop)
        x
})
ais_profile <- do.call("rbind", dd)
# assume an average kW per Level6 across sampled vessels
kW_means <- aggregate(as.numeric(as.character(ais$kw)), list(ais$metier_level6_ret), mean, na.rm=TRUE)
colnames(kW_means) <- c("Level6", "meankW")
ais_profile <- merge(ais_profile, kW_means)

# reorder
ais_profile$Speed <- factor(ais_profile$Speed, levels=c('[0,1)', '[1,2)', '[2,3)', '[3,4)', '[4,5)', '[5,6)',
                                      '[6,7)', '[7,8)', '[8,9)', '[9,10)', '[10,11)', '[11,12)', '[12,13)',
                                       '[13,14)', '[14,15)', '[15,16)', '[16,17)', '[17,18)', '[18,19)', '[19,20)'))
                                       
ais_profile <- orderBy(~Level6+Speed, ais_profile)


# assume an average max_vessel_speed for each level6
ais_profile$max_vessel_speed <- NA
dd <- lapply(split(ais_profile, f=ais_profile$Level6), function(x){
        ddd <- x[which(x$Prop>0.01), "Speed"] # 1%
        x$max_vessel_speed <- ddd[length(ddd)]
        x
})
ais_profile <- do.call("rbind.data.frame", dd)
levels(ais_profile$max_vessel_speed)  <- as.character(seq(0.5,19.5, by=1))
levels(ais_profile$Speed)  <- as.character(seq(0.5,19.5, by=1))

# check
 head(ais_profile,10)

# export for later use
save(ais_profile,
      file=file.path(getwd(), "AIS_data", "ais_profile_small_vessels.RData"))

                                       
# add fuel consumption estimates
# 1. first, apply the discount depending on vessel speed
table.fuelcons.per.engine       <-  read.table(file= file.path(getwd(),"EflaloAndTacsat", "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
ais_profile$max_consumed_per_h        <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(ais_profile$meankW))))
fuel_per_h                      <- function (a,x) a*(x^3)  # cubic law
ais_profile$a                  <- as.numeric(as.character(ais_profile$max_consumed_per_h))/ (as.numeric(as.character(ais_profile$max_vessel_speed))^3) # scaling factor
ais_profile$fuelcons_per_h     <- fuel_per_h(ais_profile$a, as.numeric(as.character(ais_profile$Speed)))

# 2. Assume max vessel consumption when towing for towed gears (i.e. within a Speed interval assuming fishing) because of the dragging resistance of the towed net
towedGears <- c("OTB", "PTB", "DRB", "OTM", "PTM", "SSC", "SDN")
ais_profile$Gear <- substr(ais_profile$Level6, 1,3)
idx <- ais_profile$Gear %in% c(towedGears) & ais_profile$Speed %in% c("1.5", "2.5", "3.5")
ais_profile[idx, "fuelcons_per_h"] <-  ais_profile[idx, "max_consumed_per_h"]    * 0.9  #assume 90% full load


# finally (a weighted average):
ais_profile$fuel_cons_in_trip <-  as.numeric(as.character(ais_profile$Prop)) * as.numeric(as.character(ais_profile$fuelcons_per_h))
fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m <- tapply(ais_profile$fuel_cons_in_trip, ais_profile$Level6, sum)

# export for later use
save(fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m,
      file=file.path(getwd(), "AIS_data", "fuel_cons_in_trip_per_level6_per_hour_for_vessels_under_12m.RData"))


# Note that the following metiers are at NA in the final dataset because sampled AIS vessels are >12m...so there are not relevant.
# TBB_CRU_16-31_0_0      TBB_DEF_>=120_0_0 OTB_DEF_32-69_0_0   PTB_DEF_>=120_0_0   SDN_DEF_>=105_1_120    SDN_DEF_>=120_0_0

