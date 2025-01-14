################################################################################
#' @description Merge SDG region key onto GHE deaths and aggregate
#' @return  Data frame with deaths and population counts for countries and regions  
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
#' Inputs
## GHE 2021 results aggregated by CA CODE categories
ghe <- readRDS("./gen/ghe-agg-cod.rds")
## Region key
key <- read.csv("./gen/key_region.csv")
################################################################################

# Merge on region key
dat <- merge(ghe, key, by = "iso3", all = TRUE)

# GHE countries that are not included in the UNICEF region key
# A bunch
unique(subset(dat, is.na(UNICEFregion))$iso3)
# GHE countries that are not included in the SDG region key
# All GHE countries are included in the SDG region key
unique(subset(dat, is.na(SDGregion))$iso3)

# Merge on regions
dat <- merge(ghe, key[,c("iso3", "WHOname", "SDGregion", "wbinc13")], by = "iso3")

# Aggregate by SDG region -------------------------------------------------

# Get population count for region/age/sex/country/year
dat_pop <- dat[,c("SDGregion","sex","year","AgeGroup","pop")]
dat_pop <- dat_pop[!duplicated(dat_pop),]
dat_pop <- setDT(dat_pop)[,lapply(.SD, sum),by=list(SDGregion,sex,year,AgeGroup)]

# Delete unnecessary columns prior to aggregation
v_del <- c("iso3","WHOname","wbinc13","pop")
dat_reg <- as.data.frame(dat)[,!(names(dat) %in% v_del)]

# Aggregate deaths over regions
# Grouping variables for aggregation
v_grouping <- c("SDGregion","sex", "year", "AgeGroup", "cacodecause")
dat_agg <- setDT(dat_reg)[,lapply(.SD, sum),by=v_grouping]

# Merge on population counts
dat_agg <- merge(as.data.frame(dat_agg), dat_pop, by = c("SDGregion","sex", "year", "AgeGroup"))

# Create world region
dat_world <- dat_reg
dat_world$SDGregion <- "World"
dat_world <- setDT(dat_world)[,lapply(.SD, sum),by=v_grouping]
dat_pop_world <- dat_pop
dat_pop_world$SDGregion <- "World"
dat_pop_world <- setDT(dat_pop_world)[,lapply(.SD, sum),by=list(SDGregion,sex,year,AgeGroup)]
dat_world <- merge(dat_world, dat_pop_world, by = c("SDGregion","sex", "year", "AgeGroup"))

# Combine with regions
dat_agg_sdg <- rbind(dat_agg, dat_world)

# Aggregate by income region ----------------------------------------------

# Get population count for region/age/sex/country/year
dat_pop <- dat[,c("wbinc13","sex","year","AgeGroup","pop")]
dat_pop <- dat_pop[!duplicated(dat_pop),]
dat_pop <- setDT(dat_pop)[,lapply(.SD, sum),by=list(wbinc13,sex,year,AgeGroup)]

# Delete unnecessary columns prior to aggregation
v_del <- c("iso3","WHOname","SDGregion","pop")
dat_reg <- as.data.frame(dat)[,!(names(dat) %in% v_del)]

# Aggregate deaths over regions
# Grouping variables for aggregation
v_grouping <- c("wbinc13","sex", "year", "AgeGroup", "cacodecause")
dat_agg <- setDT(dat_reg)[,lapply(.SD, sum),by=v_grouping]

# Merge on population counts
dat_agg_inc <- merge(as.data.frame(dat_agg), dat_pop, by = c("wbinc13","sex", "year", "AgeGroup"))

# Combine -----------------------------------------------------------------

# Harmonize column names

# National level data
v_del <- c("iso3","WHOname","SDGregion", "wbinc13")
dat$Name <- dat$iso3
dat$AdminLevel <- "National"
dat <- as.data.frame(dat)[,!(names(dat) %in% v_del)]

# SDG regions
dat_agg_sdg$Name <- dat_agg_sdg$SDGregion
dat_agg_sdg$AdminLevel <- "RegionalSDG"
dat_agg_sdg$AdminLevel[dat_agg_sdg$Name == "World"] <- "Global"
dat_agg_sdg <- as.data.frame(dat_agg_sdg)[,!(names(dat_agg_sdg) %in% v_del)]

# Income regions
dat_agg_inc$Name <- dat_agg_inc$wbinc13
dat_agg_inc$AdminLevel <- "RegionalInc"
dat_agg_inc <- as.data.frame(dat_agg_inc)[,!(names(dat_agg_inc) %in% v_del)]

# Recombine national and regional
dat <- rbind(dat, dat_agg_sdg, dat_agg_inc)

# Tidy
dat <- dat[order(dat$AdminLevel, dat$Name, dat$AgeGroup, dat$sex, dat$year, dat$cacodecause),]

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/ghe-agg-reg.rds")


