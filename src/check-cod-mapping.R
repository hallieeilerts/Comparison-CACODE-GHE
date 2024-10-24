################################################################################
#' @description Checking GHE data file for consistency with GHE-CACODE-cause-mapping. Veritfy that age-specific CA-CODE CODs are being matched to correct GHE COD and that children of parent categories added up to parent category. Made changes to spreadsheet by hand and re-checked using this script.
#' @return Three age-specific data frames with matched CA CODE and GHE CODs
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
#' Inputs
## GHE 2021 results
load("./data/ghe/GHE2021_deaths.RData")
## GHE causes mapped to COD categories
# Indicators included for...
# parent_category: CODs that are an umbrella category for other CODs
# excluded_from_total: CODs that were not listed in GHE2019 Methods pdf and are not included in sum for all deaths
key <- read.csv("./data/classification-keys/GHE-CACODE-cause-mapping.csv")
################################################################################

# Check cause-mapping for a few examples

# dat <- subset(a, age == 0.1 & iso3 == "NGA" & sex == 1 & year == 2010)
# dat <- merge(dat, key[,c("ghecause","parent_category", "excluded_from_total", "cacode_00to01m")], by = "ghecause", all.x = TRUE )
# 
# dat <- subset(a, age == 15.00 & iso3 == "USA" & sex == 1 & year == 2015)
# dat <- merge(dat, key[,c("ghecause","parent_category", "excluded_from_total", "cacode_15to19yM")], by = "ghecause", all.x = TRUE )

dat <- subset(a, age == 15.00 & iso3 == "IND" & sex == 1 & year == 2020)
dat <- merge(dat, key[,c("ghecause","parent_category", "excluded_from_total", "cacode_15to19yM")], by = "ghecause", all.x = TRUE )

rm(a)

# Check A
# Infectious and parasitic diseases
sum(subset(dat, ghecause == 40)$dths)
sum(subset(dat, ghecause %in% c(50:90))$dths)

# HIV should not be excluded a a parent cause because the two under it (101 and 102) aren't reported in the data
# This is reflected in the spreadsheet with 100-HIV/AIDS having an NA for "parent_category"
sum(subset(dat, ghecause == 100)$dths)
subset(dat, ghecause %in% c(101:102))

sum(subset(dat, ghecause == 120)$dths)
sum(subset(dat, ghecause %in% c(130:160))$dths)
sum(subset(dat, ghecause == 185)$dths)
sum(subset(dat, ghecause %in% c(186:205))$dths)
sum(subset(dat, ghecause == 210)$dths)
sum(subset(dat, ghecause %in% c(220:320))$dths)
sum(subset(dat, ghecause == 330)$dths)
sum(subset(dat, ghecause %in% c(340:362))$dths)
sum(subset(dat, ghecause == 20)$dths)
sum(subset(dat, ghecause %in% c(30, 50:90,100,110,130:160,170,180,186:205,220:320,340:362,365,370))$dths)
# all good for A. 
subset(key, ghecause %in% c(20,40,120,185,210,330) & parent_category == 1)

# Check B,C,D,E
sum(subset(dat, ghecause == 380)$dths) # Respiratory infections
sum(subset(dat, ghecause %in% c(390, 400, 410))$dths)
sum(subset(dat, ghecause %in% c(390, 395, 400, 410))$dths) # Now includes COVID

sum(subset(dat, ghecause == 490)$dths)
sum(subset(dat, ghecause %in% c(500,510,520,530))$dths)
sum(subset(dat, ghecause == 540)$dths)
sum(subset(dat, ghecause %in% c(550,560,570,580, 590))$dths)

# Check I: A-E
sum(subset(dat, ghecause == 10)$dths) # Communicable, maternal, perinatal and nutritional conditions
sum(subset(dat, ghecause %in% c(30, 50:90,100,110,130:160,170,180,186:205,220:320,340:362,365,370,
                                390,400,410,420,500,510,520,530,550,560,570,580,590))$dths)
sum(subset(dat, ghecause %in% c(30, 50:90,100,110,130:160,170,180,186:205,220:320,340:362,365,370,
                                390,395,400,410,420,500,510,520,530,550,560,570,580,590))$dths)  # Now includes COVID

# IIA. malignant neoplasms
sum(subset(dat, ghecause == 610)$dths)
sum(subset(dat, ghecause %in% c(621,622,623,630,640,650,660,670,680,691,692,
                                700,710,720,730,740,742,745,750,751,752,753,754,755,
                                761,762,763,770,780))$dths)
# IID
sum(subset(dat, ghecause == 810)$dths)
sum(subset(dat, ghecause %in% c(811,812,813,814))$dths)
# II E
sum(subset(dat, ghecause == 820)$dths)
sum(subset(dat, ghecause %in% c(831,832,840,850,860,871,872,873,874,875,880,890,900,911,912,920,930))$dths)
# II f
sum(subset(dat, ghecause == 940)$dths)
sum(subset(dat, ghecause %in% c(950,960,970,980,990,1000,1010))$dths)
# II g
sum(subset(dat, ghecause == 1020)$dths)
sum(subset(dat, ghecause %in% c(1030,1040,1050,1060,1070,1080,1090))$dths)
# II h
sum(subset(dat, ghecause == 1100)$dths)
sum(subset(dat, ghecause %in% c(1110,1120,1130,1140,1150,1160))$dths)
# II i
sum(subset(dat, ghecause == 1170)$dths)
sum(subset(dat, ghecause %in% c(1180,1190,1200))$dths)
# II j
sum(subset(dat, ghecause == 1210)$dths)
sum(subset(dat, ghecause %in% c(1220,1230,1240,1241,1242,1244,1246,1248,1250))$dths)
# II k
sum(subset(dat, ghecause == 1260)$dths)
sum(subset(dat, ghecause %in% c(1271,1272,1273,1280,1290,1300,1310,1320))$dths)
# II m
sum(subset(dat, ghecause == 1340)$dths)
sum(subset(dat, ghecause %in% c(1350,1360,1370,1380,1390))$dths)
# II n
sum(subset(dat, ghecause == 1400)$dths)
sum(subset(dat, ghecause %in% c(1410,1420,1430,1440,1450,1460))$dths)
# II o
sum(subset(dat, ghecause == 1470)$dths)
sum(subset(dat, ghecause %in% c(1480,1490,1500,1502))$dths)
# all II
sum(subset(dat, ghecause == 600)$dths)
sum(subset(dat, ghecause %in% c(621,622,623,630,640,650,660,670,680,691,692,
                                700,710,720,730,740,742,745,750,751,752,753,754,755,
                                761,762,763,770,780,811,812,813,814,831,832,840,850,860,871,872,873,874,875,880,890,900,911,912,920,930,
                                950,960,970,980,990,1000,1010,1030,1040,1050,1060,1070,1080,1090,1110,1120,1130,1140,1150,1160,
                                1180,1190,1200,1220,1230,1240,1241,1242,1244,1246,1248,1250,1271,1272,1273,1280,1290,1300,1310,1320,
                                1350,1360,1370,1380,1390,1410,1420,1430,1440,1450,1460,1480,1490,1500,1502,
                                790,800,1330,1505))$dths)

# II looks fine.

# III A
sum(subset(dat, ghecause == 1520)$dths)
sum(subset(dat, ghecause %in% c(1530,1540,1550,1560,1570,1575,1580,1590))$dths)
# III B
sum(subset(dat, ghecause == 1600)$dths)
sum(subset(dat, ghecause %in% c(1610, 1620, 1630))$dths)
sum(subset(dat, ghecause == 1510)$dths)
sum(subset(dat, ghecause %in% c(1530,1540,1550,1560,1570,1575,1580,1590,1610, 1620, 1630))$dths)

# Checking sum of deaths for all causes
sum(subset(dat, ghecause == 0)$dths)
v_causes_that_add_up_to_all <- c(30,50:90,100,110,130:160,170,180,186:205,220:320,340:362,365,370,
                                 390,400,410,420,500,510,520,530,550,560,570,580,590,
                                 621,622,623,630,640,650,660,670,680,691,692,
                                 700,710,720,730,740,742,745,750,751,752,753,754,755,
                                 761,762,763,770,780,811,812,813,814,831,832,840,850,860,871,872,873,874,875,880,890,900,911,912,920,930,
                                 950,960,970,980,990,1000,1010,1030,1040,1050,1060,1070,1080,1090,1110,1120,1130,1140,1150,1160,
                                 1180,1190,1200,1220,1230,1240,1241,1242,1244,1246,1248,1250,1271,1272,1273,1280,1290,1300,1310,1320,
                                 1350,1360,1370,1380,1390,1410,1420,1430,1440,1450,1460,1480,1490,1500,1502,
                                 790,800,1330,1505,
                                 1530,1540,1550,1560,1570,1575,1580,1590,1610, 1620, 1630,
                                 395, 1700) # Now includes covid and indirect covid
sum(subset(dat, ghecause %in% v_causes_that_add_up_to_all)$dths)

# Causes that are not included in sum
subset(dat, !(ghecause %in% v_causes_that_add_up_to_all))
# Exclude causes that are parent_categories
subset(dat, !(ghecause %in% v_causes_that_add_up_to_all) & is.na(parent_category))
# Exclude causes that are not included in the sum as per the GHE 2019 methods doc
subset(dat, !(ghecause %in% v_causes_that_add_up_to_all) & is.na(parent_category) & is.na(excluded_from_total))
# This should be zero now if all causes mapped correctly.


# Make cod_keys for each age group ----------------------------------------

#!!!!!!!!!!!!!!!!!! DO THIS IN A DIFFERENT SCRIPT

# Merge age-specific CA-CODE CODs with GHE cause names

# Adolescents
key_codADO <- merge(key_cod_cacode, key_cod_ghe[,c("cod_ghe","ghecause","adolescents", "delete")], by.x = "cod_cacode", by.y = "adolescents", all =TRUE)
nrow(subset(key_codADO, !is.na(cod_ghe) & is.na(cod_cacode))) # 0
# All GHE causes for adolescents have a matched CA-CODE cause
# Drop rows where CA CODE cause did not match to a GHE cause
df_codkey_ado <- subset(key_codADO, !is.na(cod_ghe))
# This is because the adolescent column did not contain the CA-CODE cause.
# Happens when the CA-CODE cause is not modeled for that age group. 
# For example, tetanus for adolescents is categorized as OtherCMPN.
# Thus, "cod_cacode" value of "tetanus" did not match to anything in "adolescents column.

# Neonates
key_codNEO <- merge(key_cod_cacode, key_cod_ghe[,c("cod_ghe","ghecause","neonates", "delete")], by.x = "cod_cacode", by.y = "neonates", all =TRUE)
nrow(subset(key_codNEO, !is.na(cod_ghe) & is.na(cod_cacode))) # All ghe causes for neonates have a matched CA-CODE cause
key_codNEO <- subset(key_codNEO, !is.na(cod_ghe))

# Postneonates
key_codPNEO <- merge(key_cod_cacode, key_cod_ghe[,c("cod_ghe","ghecause","postneonates", "delete")], by.x = "cod_cacode", by.y = "postneonates", all =TRUE)
nrow(subset(key_codPNEO, !is.na(cod_ghe) & is.na(cod_cacode))) # All ghe causes for postneonates have a matched CA-CODE cause
key_codPNEO <- subset(key_codPNEO, !is.na(cod_ghe))

if(finalized){
  # Save COD keys once GHE cause classification investigation finalized
  write.csv(key_codNEO, "./gen/key_codNEO.csv", row.names = FALSE)
  write.csv(key_codPNEO, "./gen/key_codPNEO.csv", row.names = FALSE)
  write.csv(key_codADO, "./gen/key_codADO.csv", row.names = FALSE)
}

