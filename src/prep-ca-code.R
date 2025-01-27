################################################################################
#' @description Create CA CODE sex-combined age group for 15-19y, aggregate deaths for SDG regions
#' @return Data frame with deaths and population counts for countries, regions, age, sex, year  
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
#' Inputs
## CA-CODE 2000-2021 results
list_csv_files <- list.files(path = "./data/ca-code/",  pattern = "*.csv")
l_cacode <- lapply(list_csv_files, function(x) read.csv(paste0("./data/ca-code/", x), 
                                                        stringsAsFactors = FALSE))
names(l_cacode) <- sub('\\.csv.*', '', list_csv_files)
## GHE 2021 deaths aggregated by CA CODE COD categories
ghe <- readRDS("./gen/ghe-agg-reg.rds")
## Region key
key_reg <- read.csv("./gen/key_region.csv")
# ## COD reclassification
# dat_filename <- list.files("./data/classification-keys")
# dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
# key_00to28d <- read.csv(paste0("./data/classification-keys/", dat_filename[grepl("00to28d", dat_filename, ignore.case = TRUE)], sep = ""))
# key_01to59m <- read.csv(paste0("./data/classification-keys/", dat_filename[grepl("01to59m", dat_filename, ignore.case = TRUE)], sep = ""))
# key_05to09y <- read.csv(paste0("./data/classification-keys/", dat_filename[grepl("05to09y", dat_filename, ignore.case = TRUE)], sep = ""))
# key_10to14y <- read.csv(paste0("./data/classification-keys/", dat_filename[grepl("10to14y", dat_filename, ignore.case = TRUE)], sep = ""))
# key_15to19yM <- read.csv(paste0("./data/classification-keys/", dat_filename[grepl("15to19yM", dat_filename, ignore.case = TRUE)], sep = ""))
# key_15to19yF <- read.csv(paste0("./data/classification-keys/", dat_filename[grepl("15to19yF", dat_filename, ignore.case = TRUE)], sep = ""))
################################################################################

# Convert to data frame
dat <- do.call(dplyr::bind_rows, l_cacode)

# Remove countries not reported in GHE
dat$ISO3 <- dat$REF_AREA
# Some small countries not reported in GHE
unique(dat$ISO3)[!(unique(dat$ISO3) %in% unique(ghe$Name))]
# Only include countries reported in GHE
dat <- subset(dat, ISO3 %in% unique(ghe$Name))

# Create new age group variable
dat$AgeGroup <- NA
dat$AgeGroup[dat$Age.group == "Neonatal"] <- "00to28d"
dat$AgeGroup[dat$Age.group == "1 to 59 months"] <- "01to59m"
dat$AgeGroup[dat$Age.group == "5 to 9 years"] <- "05to09y"
dat$AgeGroup[dat$Age.group == "10 to 14 years"] <- "10to14y"
dat$AgeGroup[dat$Age.group == "15 to 19 years"] <- "15to19y"
dat <- subset(dat, Age.group != "Under 5 years")

# Recode website display COD name to reclassified COD
dat$cacodecause <- dat$Cause.of.death
dat$cacodecause[dat$Cause.of.death == "Diarrhea"] <- "Diarrhoeal"
dat$cacodecause[dat$Cause.of.death == "HIV/AIDS" ] <- "HIV"
dat$cacodecause[dat$Cause.of.death == "Lower respiratory infections"] <- "LRI"
#dat$cacodecause[dat$Cause.of.death == "Malaria"]
dat$cacodecause[dat$Cause.of.death == "Tuberculosis"] <- "TB"
dat$cacodecause[dat$Cause.of.death == "Other communicable diseases"] <- "OtherCMPN"
dat$cacodecause[dat$Cause.of.death == "Digestive system"] <- "Digestive"
dat$cacodecause[dat$Cause.of.death == "Neoplasms/cancer" ] <- "Neoplasms"
dat$cacodecause[dat$Cause.of.death == "Other NCDs"] <- "OtherNCD"
#dat$cacodecause[dat$Cause.of.death == "Drowning"]
dat$cacodecause[dat$Cause.of.death == "Road traffic injuries"] <- "RTI"
dat$cacodecause[dat$Cause.of.death == "Collective violence"] <- "CollectVio"
dat$cacodecause[dat$Cause.of.death == "Natural disasters"] <- "NatDis"
dat$cacodecause[dat$Cause.of.death == "Other injuries"] <- "OtherInj"
dat$cacodecause[dat$Cause.of.death == "Maternal causes"] <- "Maternal"
dat$cacodecause[dat$Cause.of.death == "Cardiovascular"] <- "Cardiovascular"
dat$cacodecause[dat$Cause.of.death == "Self-harm"] <- "SelfHarm"
dat$cacodecause[dat$Cause.of.death == "Interpersonal violence"] <- "InterpVio"
dat$cacodecause[dat$Cause.of.death == "Birth asphyxia/trauma"] <- "Intrapartum"
#dat$cacodecause[dat$Cause.of.death == "Measles"]
dat$cacodecause[dat$Cause.of.death == "Meningitis/encephalitis"] <- "Meningitis"
dat$cacodecause[dat$Cause.of.death == "Prematurity"] <- "Preterm"
#dat$cacodecause[dat$Cause.of.death == "Sepsis"]
#dat$cacodecause[dat$Cause.of.death == "Tetanus"]
dat$cacodecause[dat$Cause.of.death == "Congenital anomalies"] <- "Congenital"
#dat$cacodecause[dat$Cause.of.death == "Injuries"]
dat$cacodecause[dat$Cause.of.death == "Other neonatal deaths"] <- "Other"
dat$cacodecause[dat$Cause.of.death == "Other under-5 deaths"] <- "Other"

# Rename
names(dat)[which(names(dat) == "TIME_PERIOD")] <- "Year"
names(dat)[which(names(dat) == "Year")] <- "year"
names(dat)[which(names(dat) == "Sex")] <- "sex"

# Remove unnecessary columns
dat <- dat[,-which(names(dat) %in% c("REF_AREA","Geographic.area","Age.group","SERIES_NAME","Unit.of.measure","Cause.of.death","cod_reclass"))]

# Merge on region names
dat <- merge(dat, key_reg[,c("iso3", "WHOregion", "SDGregion", "wbinc13")], all.x = TRUE, by.x = "ISO3", by.y = "iso3")

# Calculate deaths for 15-19y sex-combined --------------------------------

dat15to19y <- subset(dat, Indicator == "Deaths" & AgeGroup == "15to19y")
dat15to19y$sex <- NULL
# Grouping variables for aggregation
v_grouping <- c("Indicator","ISO3", "year", "AgeGroup", "cacodecause", "WHOregion", "SDGregion", "wbinc13")
# Aggregate deaths over sex
dat15to19y <- setDT(dat15to19y)[, lapply(.SD,sum), by=v_grouping]
dat15to19y <- as.data.frame(dat15to19y)
dat15to19y$sex <- "Total"
# Transform into fractions
dat15to19yfrac <- dat15to19y
dat15to19yfrac$Indicator <- "Fraction"
v_grouping <- c("Indicator","ISO3", "year", "AgeGroup", "WHOregion", "SDGregion", "wbinc13")
dat15to19yfrac <- setDT(dat15to19yfrac)[, total_OBS_VALUE := sum(OBS_VALUE), by=v_grouping]
#dat15to19yfrac <- dat15to19yfrac[, total_LOWER_BOUND := sum(LOWER_BOUND), by=v_grouping]
#dat15to19yfrac <- dat15to19yfrac[, total_UPPER_BOUND := sum(UPPER_BOUND), by=v_grouping]
dat15to19yfrac$OBS_VALUE <- dat15to19yfrac$OBS_VALUE/dat15to19yfrac$total_OBS_VALUE
dat15to19yfrac$LOWER_BOUND <- dat15to19yfrac$LOWER_BOUND/dat15to19yfrac$total_OBS_VALUE
dat15to19yfrac$UPPER_BOUND <- dat15to19yfrac$UPPER_BOUND/dat15to19yfrac$total_OBS_VALUE
dat15to19yfrac$total_OBS_VALUE <- NULL

# Add back with all data
dat <- rbind(dat, dat15to19y, dat15to19yfrac)

# Aggregate by SDG region -------------------------------------------------

# Delete unnecessary columns prior to aggregation
v_del <- c("ISO3", "wbinc13", "WHOregion")
dat_reg <- as.data.frame(dat)[,!(names(dat) %in% v_del)]
dat_reg <- subset(dat_reg, Indicator == "Deaths")
# Grouping variables for aggregation
v_grouping <- c("SDGregion","sex", "year", "AgeGroup", "Indicator", "cacodecause")
# Aggregate deaths over regions
dat_agg_sdg <- setDT(dat_reg)[,lapply(.SD, sum), by=v_grouping]
# Create world region
dat_world <- dat_agg_sdg
dat_world$SDGregion <- "World"
dat_world <- setDT(dat_world)[,lapply(.SD, sum), by=v_grouping]
# Combine with regions
dat_agg_sdg <- rbind(dat_agg_sdg, dat_world)
# Transform into fractions
v_grouping <- c("SDGregion","sex", "year", "AgeGroup", "Indicator")
# dat_agg_sdg <- setDT(dat_agg_sdg)[, OBS_VALUE := OBS_VALUE/sum(OBS_VALUE), by=v_grouping]
# dat_agg_sdg <- dat_agg_sdg[, LOWER_BOUND := LOWER_BOUND/sum(LOWER_BOUND), by=v_grouping]
# dat_agg_sdg <- dat_agg_sdg[, UPPER_BOUND := UPPER_BOUND/sum(UPPER_BOUND), by=v_grouping]
dat_agg_sdg <- setDT(dat_agg_sdg)[, total_OBS_VALUE := sum(OBS_VALUE), by=v_grouping]
dat_agg_sdg$OBS_VALUE <- dat_agg_sdg$OBS_VALUE/dat_agg_sdg$total_OBS_VALUE
dat_agg_sdg$LOWER_BOUND <- dat_agg_sdg$LOWER_BOUND/dat_agg_sdg$total_OBS_VALUE
dat_agg_sdg$UPPER_BOUND <- dat_agg_sdg$UPPER_BOUND/dat_agg_sdg$total_OBS_VALUE
dat_agg_sdg$total_OBS_VALUE <- NULL
dat_agg_sdg$Indicator <- "Fraction"

# Some countries may not have been assigned to region
# Remove those with NA region
nrow(subset(dat_agg_sdg, is.na(SDGregion))) # 0
dat_agg_sdg <- subset(dat_agg_sdg, !is.na(SDGregion))

# Aggregate by income region ----------------------------------------------

# Delete unnecessary columns prior to aggregation
v_del <- c("ISO3", "SDGregion", "WHOregion")
dat_reg <- as.data.frame(dat)[,!(names(dat) %in% v_del)]
dat_reg <- subset(dat_reg, Indicator == "Deaths")
# Grouping variables for aggregation
v_grouping <- c("wbinc13","sex", "year", "AgeGroup",  "Indicator", "cacodecause")
v_cod <- names(dat_reg)[!(names(dat_reg) %in% v_grouping)]
# Aggregate deaths over regions
dat_agg_inc <- setDT(dat_reg)[,lapply(.SD, sum), by=v_grouping]
# Transform into fractions
v_grouping <- c("wbinc13","sex", "year", "AgeGroup", "Indicator")
# dat_agg_inc <- setDT(dat_agg_inc)[, OBS_VALUE := OBS_VALUE/sum(OBS_VALUE), by=v_grouping]
# dat_agg_inc <- dat_agg_inc[, LOWER_BOUND := LOWER_BOUND/sum(LOWER_BOUND), by=v_grouping]
# dat_agg_inc <- dat_agg_inc[, UPPER_BOUND := UPPER_BOUND/sum(UPPER_BOUND), by=v_grouping]
dat_agg_inc <- setDT(dat_agg_inc)[, total_OBS_VALUE := sum(OBS_VALUE), by=v_grouping]
dat_agg_inc$OBS_VALUE <- dat_agg_inc$OBS_VALUE/dat_agg_inc$total_OBS_VALUE
dat_agg_inc$LOWER_BOUND <- dat_agg_inc$LOWER_BOUND/dat_agg_inc$total_OBS_VALUE
dat_agg_inc$UPPER_BOUND <- dat_agg_inc$UPPER_BOUND/dat_agg_inc$total_OBS_VALUE
dat_agg_inc$total_OBS_VALUE <- NULL
dat_agg_inc$Indicator <- "Fraction"

# Some countries may not have been assigned to region
# Remove those with NA region
nrow(subset(dat_agg_inc, is.na(wbinc13))) # 0
dat_agg_inc<- subset(dat_agg_inc, !is.na(wbinc13))

# Aggregate by WHO region -------------------------------------------------

# Delete unnecessary columns prior to aggregation
v_del <- c("ISO3", "SDGregion", "wbinc13")
dat_reg <- as.data.frame(dat)[,!(names(dat) %in% v_del)]
dat_reg <- subset(dat_reg, Indicator == "Deaths")
# Grouping variables for aggregation
v_grouping <- c("WHOregion","sex", "year", "AgeGroup",  "Indicator", "cacodecause")
v_cod <- names(dat_reg)[!(names(dat_reg) %in% v_grouping)]
# Aggregate deaths over regions
dat_agg_who <- setDT(dat_reg)[,lapply(.SD, sum), by=v_grouping]
# Transform into fractions
v_grouping <- c("WHOregion","sex", "year", "AgeGroup", "Indicator")
# dat_agg_who <- setDT(dat_agg_who)[, OBS_VALUE := OBS_VALUE/sum(OBS_VALUE), by=v_grouping]
# dat_agg_who <- dat_agg_who[, LOWER_BOUND := LOWER_BOUND/sum(LOWER_BOUND), by=v_grouping]
# dat_agg_who <- dat_agg_who[, UPPER_BOUND := UPPER_BOUND/sum(UPPER_BOUND), by=v_grouping]
dat_agg_who <- setDT(dat_agg_who)[, total_OBS_VALUE := sum(OBS_VALUE), by=v_grouping]
dat_agg_who$OBS_VALUE <- dat_agg_who$OBS_VALUE/dat_agg_who$total_OBS_VALUE
dat_agg_who$LOWER_BOUND <- dat_agg_who$LOWER_BOUND/dat_agg_who$total_OBS_VALUE
dat_agg_who$UPPER_BOUND <- dat_agg_who$UPPER_BOUND/dat_agg_who$total_OBS_VALUE
dat_agg_who$total_OBS_VALUE <- NULL
dat_agg_who$Indicator <- "Fraction"

# Some countries may not have been assigned to region
# Remove those with NA region
nrow(subset(dat_agg_who, is.na(WHOregion))) # 1650
dat_agg_who <- subset(dat_agg_who, !is.na(WHOregion))

# Combine -----------------------------------------------------------------

# Harmonize column names

# National level data
v_del <- c("ISO3", "WHOregion", "SDGregion", "wbinc13")
dat$Name <- dat$ISO3
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

# WHO regions
dat_agg_who$Name <- dat_agg_who$WHOregion
dat_agg_who$AdminLevel <- "RegionalWHO"
dat_agg_who <- as.data.frame(dat_agg_who)[,!(names(dat_agg_who) %in% v_del)]

# Recombine national and regional
dat <- rbind(dat, dat_agg_sdg, dat_agg_inc, dat_agg_who)

# Only keep fractions
dat <- subset(dat, Indicator == "Fraction")
dat$Indicator <- NULL

# Rename columns
names(dat)[which(names(dat) == "OBS_VALUE")] <- "frac"
names(dat)[which(names(dat) == "LOWER_BOUND")] <- "frac_lb"
names(dat)[which(names(dat) == "UPPER_BOUND")] <- "frac_ub"

# Tidy
dat <- dat[,c("AgeGroup","AdminLevel","Name","sex","year","cacodecause","frac","frac_lb","frac_ub")]
dat <- dat[order(dat$AdminLevel, dat$Name, dat$AgeGroup, dat$sex, dat$year),]

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/ca-code-frac.rds")


