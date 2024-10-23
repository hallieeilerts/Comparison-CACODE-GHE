################################################################################
#' @description Apply CA CODE fractions to GHE deaths, calculate rates
#' @return Data frame with cause-specific death rates for CA CODE and GHE
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
#' Inputs
## GHE 2021 deaths aggregated by CA CODE COD categories
ghe <- readRDS("./gen/ghe-agg-reg.rds")
## CA code fractions
cacode <- readRDS("./gen/ca-code-frac.rds")
## Region key
key <- read.csv("./gen/key_region.csv")
################################################################################

# Merge GHE death/pop counts with CA CODE fractions
dat <- merge(ghe, cacode, by = c("AgeGroup","AdminLevel","Name","year","sex","cacodecause"), all = TRUE)
# There should not be any missing ghe values for cacode values
# (every country and region reported)
nrow(subset(dat, is.na(dths) & !is.na(frac)))
# There are many missing ca code values for ghe, as not all sex-splits and age groups reported

# Calculate total deaths
setDT(dat)[,total_dths := sum(dths),by=list(AgeGroup, AdminLevel, Name, year, sex)]
setDT(dat)[,total_dths_lb := sum(dths.low),by=list(AgeGroup, AdminLevel, Name, year, sex)]
setDT(dat)[,total_dths_ub := sum(dths.up),by=list(AgeGroup, AdminLevel, Name, year, sex)]

# Calculate rates
dat$rate_ghe <- dat$dths/dat$pop
dat$rate_lb_ghe <- dat$dths.low/dat$pop
dat$rate_ub_ghe <- dat$dths.up/dat$pop
dat$rate_cacode <- (dat$total_dths * dat$frac)/dat$pop
dat$rate_lb_cacode <- (dat$total_dths_lb * dat$frac_lb)/dat$pop
dat$rate_ub_cacode <- (dat$total_dths_ub * dat$frac_ub)/dat$pop

# Merge on regions for national values
dat <- merge(dat, key[,c("iso3", "SDGregion")], by.x = "Name", by.y = "iso3", all.x = TRUE)

# Rename columns
names(dat)[which(names(dat) == "SDGregion")] <- "Region"
names(dat)[which(names(dat) == "dths")] <- "dths_ghe"
names(dat)[which(names(dat) == "dths.low")] <- "dths_lb_ghe"
names(dat)[which(names(dat) == "dths.up")] <- "dths_ub_ghe"
names(dat)[which(names(dat) == "frac")] <- "frac_cacode"
names(dat)[which(names(dat) == "frac_lb")] <- "frac_ub_cacode"
names(dat)[which(names(dat) == "frac_ub")] <- "frac_lb_cacode"
names(dat)[which(names(dat) == "cacodecause")] <- "COD"
names(dat)[which(names(dat) == "year")] <- "Year"
names(dat)[which(names(dat) == "sex")] <- "Sex"

# Tidy
dat <- dat[,c("AgeGroup","AdminLevel","Name","Region","Sex","Year","COD",
             "total_dths", "total_dths_lb", "total_dths_ub",
             "dths_ghe","dths_lb_ghe","dths_ub_ghe",
             "rate_ghe","rate_lb_ghe","rate_ub_ghe",
             "frac_cacode", "frac_lb_cacode", "frac_ub_cacode",
             "rate_cacode","rate_lb_cacode","rate_ub_cacode")]
dat <- dat[order(dat$AgeGroup, dat$AdminLevel, dat$Name, dat$Sex, dat$Year, dat$COD),]

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/rates.rds")

