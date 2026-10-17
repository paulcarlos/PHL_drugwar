# set working directory / folder of data and codes
setwd("") 

# upload geocode table
ref <- read.csv("geocode_prov-muni_psa-mort-births.csv")
tab1 <- readxl::read_excel("vsr_2016_death_metadata(dictionary).xlsx",sheet="geocodes")

# remove foreigners
tab1 <- tab1[tab1$`Region \r\nCode`!=0,]

# create Phil Standard Geographic Codes (PSGC)
tab1$pcode_adm2 <- ref$ADM2_PCODE[match(paste0(toupper(tab1$`Province Name`),"_",toupper(tab1$`Municipality Name`)),
                                        paste0(toupper(ref$prov.nam1),"_",toupper(ref$muni.nam1)))]
tab1$pcode_adm3 <- ref$ADM3_PCODE[match(paste0(toupper(tab1$`Province Name`),"_",toupper(tab1$`Municipality Name`)),
                                        paste0(toupper(ref$prov.nam1),"_",toupper(ref$muni.nam1)))]
rn1 <- which(is.na(tab1$pcode_adm2))
tab1$pcode_adm2[rn1] <- ref$ADM2_PCODE[match(toupper(tab1$`Province Name`[rn1]),toupper(ref$prov.nam1))]
write.csv(tab1,"geocodes_2016.csv",row.names=FALSE)

# upload individual table
rdat <- read.csv("raw_2016_deaths_jun-aug.csv") # THIS IS A SUBSET DATA OF A LARGER 2016 EXCEL FILE

# remove foreigners
tab2 <- rdat[rdat$residence_region!=0,]

# add dates
tab2$date <- as.Date(paste0(tab2$date_of_death_year,"-",tab2$date_of_death_month,"-",tab2$date_of_death_day))

# fix age and sex
tab2$sex2 <- ifelse(as.integer(tab2$sex)==1,"M","F")
tab2$age2 <- NA
tab2$age2[as.integer(tab2$age)%in%0:28] <- 0
tab2$age2[as.integer(tab2$age)%in%101:111] <- 0
tab2$age2[as.integer(tab2$age)%in%201:298] <- as.integer(tab2$age[as.integer(tab2$age)%in%201:298])-200
tab2$age2[as.integer(tab2$age)>298 & as.integer(tab2$age)<999] <- 98

# add geocodes
refmun <- paste0(sprintf("%02d",as.integer(tab1$`Region \r\nCode`)),"_", # REFERENCE MUNICPALITIES
                 sprintf("%03d",as.integer(tab1$`Province Code`)),"_",
                 sprintf("%02d",as.integer(tab1$`Municipality Code`)))

place <- paste0(sprintf("%02d",as.integer(tab2$place_of_death_region)),"_", # PLACE OF DEATHS
                sprintf("%03d",as.integer(tab2$place_of_death_province)),"_",
                sprintf("%02d",as.integer(tab2$place_of_death_mun)))
setdiff(place,refmun)
place[grep("_039_",place)] <- "01_039_NA" # Manila
place[grep("02_011_14",place)] <- "02_012_14" # Tublay
place[grep("02_027_10",place)] <- "02_028_10" # Tinoc
setdiff(place,refmun)

tab2$place_adm2_pcode <- tab1$pcode_adm2[match(place,refmun)]
tab2$place_adm3_pcode <- tab1$pcode_adm3[match(place,refmun)]
rn1 <- which(is.na(tab2$place_adm2_pcode))

resid <- paste0(sprintf("%02d",as.integer(tab2$residence_region)),"_", # RESIDENCE 
                sprintf("%03d",as.integer(tab2$residence_province)),"_",
                sprintf("%02d",as.integer(tab2$residence_mun)))
setdiff(resid,refmun)
resid[grep("_039_",resid)] <- "01_039_NA" # Manila
resid[grep("02_011_14",resid)] <- "02_012_14" # Tublay
resid[grep("02_027_10",resid)] <- "02_028_10" # Tinoc
setdiff(resid,refmun)

tab2$resid_adm2_pcode <- tab1$pcode_adm2[match(resid,refmun)]
tab2$resid_adm3_pcode <- tab1$pcode_adm3[match(resid,refmun)]
rn1 <- which(is.na(tab2$resid_adm2_pcode))

sort(unique(tab2$place_of_death_mun))
sort(unique(tab2$residence_mun))
apply(tab2,2,function(z)sum(is.na(z)))

# save
write.csv(tab2,"deaths_2016_jun-aug.csv", row.names=FALSE)

rm(list=ls());gc()
