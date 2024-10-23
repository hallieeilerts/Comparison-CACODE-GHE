################################################################################
#' @description Merge SDG region key onto GHE deaths and aggregate
#' @return  Data frame with deaths and population counts for countries and regions  
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
#' Inputs
## GHE 2021 results aggregated by CA CODE categories
ghe <- readRDS("./gen/ghe-agg-cod.rds")
## Region key
key <- read.csv("./gen/key_region.csv")
################################################################################

# Merge on region key
dat <- merge(ghe, key, by = "iso3", all = TRUE)

# GHE countries that are not included in the UNICEF region key
unique(subset(dat, is.na(UNICEFregion))$iso3)
# All GHE countries are included in the SDG region key
unique(subset(dat, is.na(SDGregion))$iso3)

# Merge on SDG regions
dat <- merge(ghe, key[,c("iso3", "WHOname", "SDGregion")], by = "iso3")

# Get population count for region/age/sex/country/year
dat_pop <- dat[,c("SDGregion","sex","year","AgeGroup","pop")]
dat_pop <- dat_pop[!duplicated(dat_pop),]
dat_pop <- setDT(dat_pop)[,lapply(.SD, sum),by=list(SDGregion,sex,year,AgeGroup)]

# Delete unnecessary columns prior to aggregation
v_del <- c("iso3","WHOname","pop")
dat_reg <- as.data.frame(dat)[,!(names(dat) %in% v_del)]

# Grouping variables for aggregation
v_grouping <- c("SDGregion","sex", "year", "AgeGroup", "cacodecause")

# Aggregate deaths over regions
dat_agg <- setDT(dat_reg)[,lapply(.SD, sum),by=v_grouping]

# Merge on population counts
dat_agg <- merge(as.data.frame(dat_agg), dat_pop, by = c("SDGregion","sex", "year", "AgeGroup"))

# Harmonize column names
v_del <- c("iso3","WHOname","SDGregion")
dat$Name <- dat$iso3
dat$AdminLevel <- "National"
dat <- as.data.frame(dat)[,!(names(dat) %in% v_del)]
dat_agg$Name <- dat_agg$SDGregion
dat_agg$AdminLevel <- "Regional"
dat_agg <- as.data.frame(dat_agg)[,!(names(dat_agg) %in% v_del)]

# Recombine national and regional
dat <- rbind(dat, dat_agg)

# Tidy
dat <- dat[order(dat$AdminLevel, dat$Name, dat$AgeGroup, dat$sex, dat$year, dat$cacodecause),]

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/ghe-agg-reg.rds")


