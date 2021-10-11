
setwd(file.path("D:","FBA","BENTHIS_2020"))


# read for large vessels/bottom contacting gears
dat1 <- read.table(file=file.path(getwd(), "outputs2020", "output_plots", "barplot_mean_fuel_efficiency_ 2005 - 2019 _DEM_and_PEL_plot_land_and_FPUC_and_FPUV.dat"), header=TRUE, sep=";")

# read for large vessels/pelagic gears
dat2 <- read.table(file=file.path(getwd(), "outputs2020_pel", "output_plots", "barplot_mean_fuel_efficiency_ 2005 - 2019 _PEL_plot_land_and_FPUC_and_FPUV.dat"), header=TRUE, sep=";")

# read for small vessels
dat3 <- read.table(file=file.path(getwd(), "outputs2020_lgbkonly", "output_plots", "barplot_mean_fuel_efficiency_ 2005 - 2019 _DEM_PEL_plot_land_and_FPUC_and_FPUV.dat"), header=TRUE, sep=";")

dat <- rbind.data.frame(dat1, dat2, dat3)


library(data.table)
wide <- data.table::dcast(setDT(dat), type + seg  ~ var, value.var="average")


score_table <- as.data.frame(wide)[, c("type", "seg", "Litre per euro catch", "Litre per kg catch", "CPUF (kg per litre)", "VPUF (euro per litre)")]

an <- function(x) as.numeric(as.character(x))
relative_Litre_per_euro_catch <-  an(score_table$'Litre per euro catch') / sum(an(score_table$'Litre per euro catch'), na.rm=TRUE)*100
relative_Litre_per_kg_catch <-  an(score_table$'Litre per kg catch') / sum(an(score_table$'Litre per kg catch'), na.rm=TRUE)*100
score_table <- cbind.data.frame(score_table,
                                relative_Litre_per_euro_catch=relative_Litre_per_euro_catch,
                                relative_Litre_per_kg_catch=relative_Litre_per_kg_catch)
score_table$Average_efficiency <- apply(score_table[,c('relative_Litre_per_kg_catch', 'relative_Litre_per_euro_catch')], 1, mean)

score_table$Relative_efficiency_to_5 <-  score_table$Average_efficiency*5/max(score_table$Average_efficiency, na.rm=TRUE)

score_table$asterik <- cut(score_table$Relative_efficiency_to_5, breaks=c(0,1,2,3,4,5))
levels(score_table$asterik) <- c('*', '**', '***', '****', '*****')


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


# add on
library(doBy)
score_table <- doBy::orderBy(~ type+Relative_efficiency_to_5, score_table)

score_table <- score_table[complete.cases(score_table),]

score_table$seg <- sapply(strsplit(as.character(score_table$seg), " "), function(x)x[1]) # debug white space

score_table$LE_MET <- score_table$seg
score_table <- cbind.data.frame(met_desc=friendly_met_names (score_table) , score_table)

score_table <- score_table[! grepl("misc", score_table$met_desc),]
score_table$met_desc <- gsub("\n", " ", score_table$met_desc)

write.table(score_table$Relative_efficiency_to_5, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
 
# round and export
write.table(score_table[, c("type","met_desc", "seg","Litre per euro catch", "Litre per kg catch","CPUF (kg per litre)", "VPUF (euro per litre)", "Relative_efficiency_to_5", "asterik")],
                file=file.path(getwd(), "ggplots", "scoring_table.csv"), col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)

