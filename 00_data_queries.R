# Forage fish report data pulls
# Matt Callahan
# 10/16/2026
# 2024 report pulled data from AKFIN Answers (online GUI)
# This will pull data using a direct database connection. 
library(dplyr)
library(dbplyr)
library(RJDBC)

# connect to AKFIN
# lot's of different connection options, this uses RJDBC because it's faster but most NOAA users use odbc
# it does require a downloaded java jar
options(java.parameters="-Xmx8g")
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../snippets/dbconnect/java/ojdbc8.jar")

con <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   keyring::key_list("akfin_oracle_db")$username, 
                   keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))


# NONTARGET
# get species from previous years
# bsai_nt <- read.csv("2024/data/BSAI/Nontarget Species Bycatch.csv")
# bsai_sp<- unique(bsai_nt$Species.Group.Name)
# 
# goa_nt <- read.csv("2024/data/GOA/Nontarget Species Bycatch.csv")
# goa_sp<- unique(goa_nt$Species.Group.Name)
# 
# nt_sp<-unique(c(bsai_sp, goa_sp))

# for future reference
nt_sp<-c("Squid",
         "Pandalid shrimp" ,              
         "Pacific Sandfish" ,   
         "Smelt (Family Osmeridae)",
         "Lanternfishes (myctophidae)",
        "Pacific Sand lance",             
        "Other osmerids"  ,              
        "Bristlemouths",
        "Gunnels",                        
        "Eulachon",                       
        "Capelin",                        
        "Surf smelt",                    
        "Deep sea smelts (bathylagidae)")

# download
# Note no separation of BS and AI...

nontarget <- tbl(con, in_schema("COUNCIL", "COMPREHENSIVE_NONTARGET")) %>%
  rename_with(tolower) %>%
  filter(nontarget_group_name %in% nt_sp) %>%
  group_by(fmp_area, year, nontarget_group_name, retained_or_discarded) %>%
  summarise(
    nontarget_estimate_mt = sum(nontarget_estimate_weight, na.rm = TRUE)  ) %>%
  arrange(fmp_area, year, nontarget_group_name, retained_or_discarded) %>% 
    collect()%>%
  mutate(retained_or_discarded=ifelse(retained_or_discarded=="Y", "Retained", "Discarded"))

# sanity check 
# sum((bsai_nt%>%filter(Year<2021))$Nontarget.Estimate..mt.)-sum((nontarget %>% filter(year<2021 & fmp_area=="BSAI"))$nontarget_estimate_mt) # 0
# sum((goa_nt%>%filter(Year<2021))$Nontarget.Estimate..mt.)-sum((nontarget %>% filter(year<2021 & fmp_area=="GOA"))$nontarget_estimate_mt) # 0

# write separate csvs
write.csv(nontarget%>%filter(fmp_area=="GOA"), "data/GOA/goa_nontarget.csv", row.names=FALSE)
write.csv(nontarget%>%filter(fmp_area=="BSAI"), "data/BSAI/bsai_nontarget.csv", row.names=FALSE)

# PSC


psc <- tbl(con, in_schema("COUNCIL", "COMPREHENSIVE_PSC")) %>%
  rename_with(tolower) %>%
  filter(species_group_code =="HERR") %>%
  group_by(fmp_area, year, trip_target_name) %>%
  summarise(
    psc_estimate_mt = sum(pscnq_estimate, na.rm = TRUE)  ) %>%
  arrange(fmp_area, year, trip_target_name) %>% 
  collect()

# check
# goa_psc_24<-read.csv("2024/data/GOA/Prohibited Species Catch.csv")
# sum((goa_psc_24%>%filter(Year<2023))$PSCNQ.Estimate)-sum((psc %>% filter(year<2023 & fmp_area=="GOA"))$psc_estimate_mt) # 0

# write separate csvs
write.csv(psc%>%filter(fmp_area=="GOA"), "data/GOA/goa_herring_psc.csv", row.names=FALSE)
write.csv(psc%>%filter(fmp_area=="BSAI"), "data/BSAI/bsai_herring_psc.csv", row.names=FALSE)

# CAS
# This is just squid
sqid <- tbl(con, in_schema("COUNCIL", "COMPREHENSIVE_BLEND_CA")) %>%
  rename_with(tolower) %>%
  filter(species_group_code =="SQID" | species_name == "squid, majestic") %>%
  group_by(fmp_area, year, species_group_code, retained_or_discarded) %>%
  summarise(
    catch_mt = sum(weight_posted, na.rm = TRUE)  ) %>%
  arrange(fmp_area, year, species_group_code, retained_or_discarded) %>% 
  collect()

# check
# cas24_1<-read.csv("2024/data/GOA/FMP Other Species Bycatch CAS2.csv")
# cas24_2<-read.csv("2024/data/BSAI/Groundfish Total Catch.csv")
# cas24_3<-read.csv("2024/data/BSAI/FMP Other Species Bycatch CAS2.csv")
# #
# newcas21g<- sum((sqid %>% filter(year<2021 & fmp_area == "GOA"))$catch_mt, na.rm=T)
# newcas21b<- sum((sqid %>% filter(year<2021 & fmp_area == "BSAI"))$catch_mt, na.rm=T)
# 
# oldgoa21 <-  sum((cas24_1%>%filter(Year<2021))$Weight.Posted..mt., na.rm=T)
# oldgoa21-newcas21g # 0
# 
# oldbs21a <- sum((cas24_2%>%filter(Year<2021))$Catch..mt., na.rm=T)
# oldbs21b <- sum((cas24_3%>%filter(Year<2021))$Weight.Posted..mt., na.rm=T)
# oldbs21a-newcas21b # -463 ? whatever, moving on.
# oldbs21b-newcas21b # -0


# write separate csvs
write.csv(sqid%>%filter(fmp_area=="GOA"), "data/GOA/squid_catch.csv", row.names=FALSE)
write.csv(sqid%>%filter(fmp_area=="BSAI"), "data/BSAI/squid_catch.csv", row.names=FALSE)

# GAP_PRODUCTS
# There are these species lists but I am just going to query the species codes that were in the reports before
# goa_sp<- read.csv("2024/data/GOA/list_of_species.csv")
# bsai_sp <- read.csv("2024/data/BSAI/list of species.csv")

goa_biom<-read.csv("2024/data/GOA/Biomass by Stratum GOA.csv")
goa_cpue<-read.csv("2024/data/GOA/race_cpue_by_haul.csv")
goa_sp<-unique(c(unique(goa_biom$Species.Code), unique(goa_cpue$Species.Code)))
# 20202 20510 21110 21592 21740 23000 23010 23041 23220 23235 23240 23800 66000 66031 66120 79000 79210
bsai1 <- read.csv("2024/data/BSAI/race_cpue_by_haul.csv", skip=7)
bsai2 <- read.csv("2024/data/BSAI/EBS Shelf - Biomass by Stratum Standard.csv")
bsai3 <- read.csv("2024/data/BSAI/ebs_biomass.csv")
79000 #EBS Shelf - Biomass by Stratum Standard Grouped
bsai_sp<-unique(c(unique(bsai1$Species.Code), unique(bsai2$Species.Code), unique(bsai3$Species.Code)))
bsai_sp
#79210 23000 66045 21592 23808 66030 60145 23010 66302 23055 66179 23041 23850 23807 20202 66770 66031 66000 21110 79000 66601 23809 21725 66150 23806 23800 23836 66163 66161 20203 20204 66019 66500 66548 66050 66120 64000 22600 66003 79120 66512 66032 22623 66750
gap_sp <- unique(c(goa_sp, bsai_sp))
# select only biomass regions
gap_regions<-c(99900, 99902, 99903, 99904, 99905)

# biomass
gap_biomass <- tbl(con, in_schema("GAP_PRODUCTS", "AKFIN_BIOMASS_V")) %>%
  rename_with(tolower) %>%
  filter(species_code %in% gap_sp & area_id %in% gap_regions) %>%
  collect()
# write csvs
write.csv(gap_biomass%>%filter(survey_definition_id==98),  "data/BSAI/ebs_biomass.csv",  row.names=FALSE)
write.csv(gap_biomass%>%filter(survey_definition_id==143),  "data/BSAI/nbs_biomass.csv",  row.names=FALSE)
write.csv(gap_biomass%>%filter(survey_definition_id==51),  "data/BSAI/ai_biomass.csv",  row.names=FALSE)
write.csv(gap_biomass%>%filter(survey_definition_id==47),  "data/GOA/goa_biomass.csv",  row.names=FALSE)

# goa cpue
goa_cpue <-  tbl(con, in_schema("GAP_PRODUCTS", "AKFIN_CPUE_V")) %>%
  rename_with(tolower) %>%
  filter(species_code %in% gap_sp & survey_definition_id==47) %>%
  collect()
# save
write.csv(goa_cpue,  "data/GOA/goa_cpue.csv",  row.names=FALSE)

# bsai cpue
# Note that I combined AI, EBS slope, SEBS and NBS into one csv
bsai_cpue <-  tbl(con, in_schema("GAP_PRODUCTS", "AKFIN_CPUE_V")) %>%
  rename_with(tolower) %>%
  filter(species_code %in% gap_sp & survey_definition_id%in%c(51, 78, 98, 143)) %>%
  collect()

# save
# half a million rows
write.csv(bsai_cpue,  "data/BSAI/bsai_cpue.csv",  row.names=FALSE)


