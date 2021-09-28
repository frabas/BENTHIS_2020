

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


# add on
library(doBy)
score_table <- doBy::orderBy(~ type+Relative_efficiency_to_5, score_table)

score_table <- score_table[complete.cases(score_table),]

score_table$seg <- sapply(strsplit(as.character(score_table$seg), " "), function(x)x[1]) # debug white space

score_table$LE_MET <- score_table$seg
score_table <- cbind.data.frame(met_desc=friendly_met_names (score_table) , score_table)

score_table <- score_table[! grepl("misc", score_table$met_desc),]
score_table$met_desc <- gsub("\n", " ", score_table$met_desc)

# round and export
write.table(score_table[, c("type","met_desc", "seg","Litre per euro catch", "Litre per kg catch","CPUF (kg per litre)", "VPUF (euro per litre)", "asterik")],
                file=file.path(getwd(), "ggplots", "scoring_table.csv"), col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)

