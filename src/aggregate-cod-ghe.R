################################################################################
#' @description Aggregate GHE CODs into CA CODE categories
#' @return Data frame with aggregated deaths and population counts by iso3, age, sex, year  
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
#' Inputs
## GHE 2021 results
load("./data/ghe/GHE2021_deaths.RData")
## GHE causes mapped to COD categories
# Indicators included for (i) parent_category: CODs that are an umbrella category for other CODs,
# (ii) CODs that are level 4 causes (were not listed in GHE2019 Methods pdf and are not included in sum for all deaths).
key <- read.csv("./data/classification-keys/GHE-CACODE-cause-mapping_20250115.csv")
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

# Age groups
# Keep age groups of interest
dat <- subset(a, age %in% c(0.1, 0.11, 1.00, 5.00, 10.00, 15.00, 20.00))
rm(a)

# Remove countries not reported in CA CODE
# PRI (Puerto Rico)
dat <- subset(dat, !(iso3 == "PRI"))

# Create new age group variable
# 0 is 0-11m
# 0.1	is 0-27 days (neonates)
# 0.11 is 1 to 11 months (postneonatal under 1 year)
dat$AgeGroup <- NA
dat$AgeGroup[dat$age == 0.10] <- "00to28d"
dat$AgeGroup[dat$age == 0.11] <- "01to11m"
dat$AgeGroup[dat$age == 1.00] <- "01to04y"
dat$AgeGroup[dat$age == 5.00] <- "05to09y"
dat$AgeGroup[dat$age == 10.00] <- "10to14y"
dat$AgeGroup[dat$age == 15.00] <- "15to19y"
dat$AgeGroup[dat$age == 20.00] <- "20to24y"
dat$age <- NULL
# Vector of all included GHE age groups
v_AgeGroup <- c("00to28d", "01to11m", "01to04y", "05to09y", "10to14y", "15to19y", "20to24y")

# Recode sex
dat$sex[dat$sex == 1] <- "Male"
dat$sex[dat$sex == 2] <- "Female"
dat$sex[dat$sex == 3] <- "Total"

# CA CODE cause mapping key does not include all of the above age groups
# Specifically, 01to11m, 01to04y, 20to24y
# These are not groups that CA-CODE currently estimates
# For the below, will use same cause mapping as for 01to59m
key$cacode_01to11m <- key$cacode_01to59m
key$cacode_01to04y <- key$cacode_01to59m
# For the below, will use same cause mapping as for 15to19
key$cacode_20to24yF <- key$cacode_15to19yF
key$cacode_20to24yM <- key$cacode_15to19yM

# Function for age-specific aggregation of causes in CACODE categories and creation of sex-combined category
fn_causeAgg <- function(dat, key, x){
  
  # Subset to age group of interest
  dat_agesp <- subset(as.data.frame(dat), AgeGroup == x)
  # Scalar with age-specific mapping column name
  v_COD_mapping_agesp <- names(key)[grep(x, names(key), ignore.case = TRUE)]
  v_COD_mapping_agesp <- sort(v_COD_mapping_agesp)
  key_agesp <- key[, c("ghecause","causename","parent_category", "level4cause",
                       v_COD_mapping_agesp)]
  
  # Merge with cause mapping
  dat_agesp <- merge(dat_agesp, key_agesp, by = c("ghecause","causename"))
  
  # If CA-CODE has sex-specific mapping for the age group (two reclassification columns), 
  # create one column which uses appropriate mapping depending on sex value
  # If GHE provides sex-specific breakdown but CA CODE does not, the sex-combined CA CODE reclassification will be used
  if(length(v_COD_mapping_agesp)>1){
    dat_agesp$cacodecause <- NA
    dat_agesp[dat_agesp$sex == "Male", "cacodecause"] <- dat_agesp[dat_agesp$sex == "Male", v_COD_mapping_agesp[2]] # Males
    dat_agesp[dat_agesp$sex == "Female", "cacodecause"] <- dat_agesp[dat_agesp$sex == "Female", v_COD_mapping_agesp[1]] # Females
    dat_agesp[dat_agesp$sex == "Total", "cacodecause"] <- dat_agesp[dat_agesp$sex == "Total", v_COD_mapping_agesp[1]] # Female cod mapping for both sexes combined. Includes "Maternal".
    dat_agesp <- dat_agesp[!(names(dat_agesp) %in% v_COD_mapping_agesp)]
  }else{
    # Otherwise, just change name of cause mapping column to cacodecause
    names(dat_agesp)[which(names(dat_agesp) ==  v_COD_mapping_agesp)] <- "cacodecause"
  }
  
  # Delete causes that should be excluded prior to aggregation
  dat_agesp <- subset(dat_agesp, is.na(parent_category) & is.na(level4cause))
  
  # Get population count for age/sex/country/year
  dat_pop <- dat_agesp[,c("iso3","sex","year","AgeGroup","pop")]
  dat_pop <- dat_pop[!duplicated(dat_pop),]
  
  # Delete unnecessary columns prior to aggregation
  v_del <- c("ghecause","causename","parent_category","level4cause","pop")
  dat_agesp <- dat_agesp[,!(names(dat_agesp) %in% v_del)]
  
  # Grouping variables for aggregation
  v_grouping1 <- c("iso3", "sex", "year", "AgeGroup", "cacodecause")
  
  # Aggregate deaths
  dat_agg <- setDT(dat_agesp)[,lapply(.SD, sum),by=v_grouping1]
  
  # Merge on population counts
  dat_agg <- merge(dat_agg, dat_pop, by = c("iso3", "sex", "year", "AgeGroup"))
  
  ## Code to create sexes combined category from M and F
  ## Not necessary because GHE already provides sex-combined category
  # dat_aggsex <- subset(dat_agg, sex %in% c(1,2))
  # # Recode Maternal as OtherCMPN
  # dat_aggsex$cacodecause[dat_aggsex$cacodecause == "Maternal"] <- "OtherCMPN"
  # # Aggregate over sexes
  # v_grouping2 <- c("iso3", "year", "age", "AgeGroup", "cacodecause")
  # dat_aggsex <- setDT(dat_aggsex)[,lapply(.SD, sum),by=v_grouping2]
  # dat_aggsex$sex <- 3
  # res <- rbind(dat_agg, dat_aggsex)
  
  # Tidy
  res <- dat_agg
  res <- res[order(res$iso3, res$AgeGroup, res$sex, res$year, res$cacodecause),]
  
  return(res)
  
}

# Apply function to all age groups
l_dat_agg <- lapply(v_AgeGroup, function(x) fn_causeAgg(dat, key, x))

# Convert into dataframe
dat_agg <- do.call(rbind, l_dat_agg)

# Create 01to59m
v_grouping <- c("iso3", "sex", "year", "AgeGroup", "cacodecause")
dat_01to59m <- subset(dat_agg, AgeGroup %in% c("01to11m", "01to04y"))
dat_01to59m$AgeGroup <- "01to59m"
dat_01to59m <- setDT(dat_01to59m)[,lapply(.SD, sum),by=v_grouping]
dat_agg <- rbind(dat_agg, dat_01to59m)

# Tidy
res <- dat_agg
res <- res[order(res$AgeGroup, res$iso3, res$sex, res$year, res$cacodecause),]

# Save output(s) ----------------------------------------------------------

saveRDS(res, "./gen/ghe-agg-cod.rds")




