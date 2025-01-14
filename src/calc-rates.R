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
# Missing ghe values for non-missing cacode values
# Happens when the cause is Maternal. This is not include in GHE for males. 
# In Cacode it is included and assigned zero.
nrow(subset(dat, is.na(dths) & !is.na(frac))) # 4334
unique(subset(dat, is.na(dths) & !is.na(frac))$cacodecause) # Maternal
# Otherwise, there should not be any missing ghe values for non-missing cacode values (every cacode country and region is reported in GHE)
nrow(subset(dat, is.na(dths) & !is.na(frac) & cacodecause != "Maternal")) # 0
# There are many missing ca code values for ghe, as not all age groups and sex-split for each age group are reported

# Assign zero to Maternal for GHE to match CA CODE
dat$dths[is.na(dat$dths) & dat$cacodecause == "Maternal"] <- 0
dat$dths.low[is.na(dat$dths.low) & dat$cacodecause == "Maternal"] <- 0
dat$dths.up[is.na(dat$dths.up) & dat$cacodecause == "Maternal"] <- 0
# Fill down pop for the NA in Maternal
dat <- dat %>% group_by(AgeGroup, AdminLevel, Name, year, sex) %>%
        fill(pop, .direction = "down")

# Calculate total deaths
setDT(dat)[,total_dths := sum(dths, na.rm = T),by=list(AgeGroup, AdminLevel, Name, year, sex)]
setDT(dat)[,total_dths_lb := sum(dths.low, na.rm = T),by=list(AgeGroup, AdminLevel, Name, year, sex)]
setDT(dat)[,total_dths_ub := sum(dths.up, na.rm = T),by=list(AgeGroup, AdminLevel, Name, year, sex)]

# Calculate GHE fractions
dat$frac_ghe <- dat$dths/dat$total_dths
dat$frac_lb_ghe <- dat$dths/dat$total_dths_lb
dat$frac_ub_ghe <- dat$dths/dat$total_dths_ub

# If total number of GHE deaths was 0, assign fractions as 0 instead of NaN
dat$frac_ghe[dat$total_dths == 0] <- 0
dat$frac_lb_ghe[dat$total_dths_lb == 0] <- 0
dat$frac_ub_ghe[dat$total_dths_ub == 0] <- 0

# Calculate rates
dat$rate_ghe <- dat$dths/dat$pop
dat$rate_lb_ghe <- dat$dths.low/dat$pop
dat$rate_ub_ghe <- dat$dths.up/dat$pop
dat$rate_cacode <- (dat$total_dths * dat$frac)/dat$pop
dat$rate_lb_cacode <- (dat$total_dths_lb * dat$frac_lb)/dat$pop
dat$rate_ub_cacode <- (dat$total_dths_ub * dat$frac_ub)/dat$pop
dat$dths_cacode <- (dat$total_dths * dat$frac)
dat$dths_lb_cacode <- (dat$total_dths_lb * dat$frac_lb)
dat$dths_ub_cacode <- (dat$total_dths_ub * dat$frac_ub)

# Merge on regions for national values
dat <- merge(dat, key[,c("iso3", "SDGregion", "wbinc13")], by.x = "Name", by.y = "iso3", all.x = TRUE)

# Rename columns
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
dat <- dat[,c("AgeGroup","AdminLevel","Name","SDGregion", "wbinc13","Sex","Year","COD",
             "total_dths", "total_dths_lb", "total_dths_ub",
             "frac_ghe", "frac_lb_ghe", "frac_ub_ghe",
             "dths_ghe","dths_lb_ghe","dths_ub_ghe",
             "rate_ghe","rate_lb_ghe","rate_ub_ghe",
             "frac_cacode", "frac_lb_cacode", "frac_ub_cacode",
             "dths_cacode","dths_lb_cacode","dths_ub_cacode",
             "rate_cacode","rate_lb_cacode","rate_ub_cacode")]
dat <- dat[order(dat$AgeGroup, dat$AdminLevel, dat$Name, dat$Sex, dat$Year, dat$COD),]

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/rates.rds")

