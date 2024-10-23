################################################################################
#' @description Set regional classifications for countries
#' @return Data frame with countries and different regional classifications.
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
require(readstata13)
#' Inputs
key_region_u20_WHO  <- read.dta13("./data/classification-keys/20190528-RegionClassSDG.dta", nonint.factors = T)
key_region_u20_IGME <- read.csv("./data/classification-keys/20210407-RegionClassIGME.csv")
################################################################################

## SDG region classification

dat1 <- key_region_u20_WHO
dat1 <- dat1[, names(dat1) %in% c("dimensionmembercode", "whoname", "sdg1")]
# Drop rows without an iso3 code
dat1 <- subset(dat1, dimensionmembercode != "")
# Drop rows without an SDG region
dat1 <- subset(dat1, sdg1 != "")
# Simplify SDG region labels
dat1$SDGregion <- as.character(dat1$sdg1)
dat1$SDGregion[dat1$SDGregion == "Australia and New Zealand (M49)"] <- "Australia and New Zealand"
dat1$SDGregion[dat1$SDGregion == "Central Asia (M49) and Southern Asia (MDG=M49)"] <- "Central and Southern Asia"
dat1$SDGregion[dat1$SDGregion == "Eastern Asia (M49) and South-eastern Asia (MDG=M49)"] <- "Eastern and South-eastern Asia"
dat1$SDGregion[dat1$SDGregion == "Latin America & the Caribbean (MDG=M49)"] <- "Latin America and the Caribbean"
dat1$SDGregion[dat1$SDGregion == "Northern America (M49) and Europe (M49)"] <- "Northern America and Europe"
dat1$SDGregion[dat1$SDGregion == "Oceania (MDG) / Oceania (M49) excluding Australia and New Zealand (M49)"] <- "Oceania"
dat1$SDGregion[dat1$SDGregion == "Sub-Saharan Africa (M49)"] <- "Sub-Saharan Africa"
dat1$SDGregion[grepl( "Western Asia", dat1$SDGregion, ignore.case = TRUE)] <- "Western Asia and Northern Africa"
# Delete old region columns
dat1 <- dat1[,-which(names(dat1) %in% c("sdg1"))]
# Re-label variables
names(dat1)[names(dat1) == "dimensionmembercode"] <- "iso3"
names(dat1)[names(dat1) == "whoname"] <- "WHOname"

## UNICEF region classification

dat2 <- key_region_u20_IGME
dat2 <- dat2[, names(dat2) %in% c("ISO3Code", "UNICEFReportRegion1", "UNICEFReportRegion2")]
# Combine UNICEFReportRegion1 and UNICEFReportRegion2
dat2$UNICEFregion <- dat2$UNICEFReportRegion1
# If report region 2 is provided, use it instead of report region 1
dat2$UNICEFregion[which(dat2$UNICEFReportRegion2 != "")] <- dat2$UNICEFReportRegion2[which(dat2$UNICEFReportRegion2 != "")]
# Delete old region columns
dat2 <- dat2[,-which(names(dat2) %in% c("UNICEFReportRegion1", "UNICEFReportRegion2"))]
# Re-label variables
names(dat2)[names(dat2) == "ISO3Code"] <- "iso3"

## Combine region reporting schemes

dat <- merge(dat1, dat2, by = c("iso3"), all = TRUE)

# Save output(s) ----------------------------------------------------------

write.csv(dat, "./gen/key_region.csv", row.names = FALSE)




