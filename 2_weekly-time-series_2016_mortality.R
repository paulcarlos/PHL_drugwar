library(lubridate)

# set working directory / folder of data and codes
setwd("") 

# load processed mortality data
dat <- read.csv("deaths_2016_jun-aug.csv")

# convert to DATE variable, add ISO Week
dat$date <- as.Date(dat$date)
dat$yrwk <- paste0(isoyear(dat$date),"-",sprintf("%02d",isoweek(dat$date)))

# create counts variable
dat$mort <- 1

# ICD 10 codes compilation / causes of death
icd <- unique(dat$cause_of_death_3digit) # all available deaths
fira <- c("W32","W33","W34","X72","X73","X74","X93","X94","X95","Y22","Y23","Y24","Y35") # firearm codes
infc <- c(grep("A|B",icd,value=TRUE),paste0("G0",0:7),paste0("J",sprintf("%02d",0:22)),paste0("L0",0:8)) # infectious diseases, with influenza and pneumonia
circ <- grep("I",icd,value=TRUE) # circulatory
other <- sort(icd[!icd %in% c(fira,infc,circ)]) # all other natural causes
grp <- list(fira = fira, infc = infc, circ = circ, others = other) # compile in a list

# create blank dataframe
prv <- sort(unique(dat$place_adm2_pcode)) # province PSGC codes
wk <- sort(unique(dat$yrwk)) # ISO weeks
aggrp <- c("","_under18") # age groups
cnm <- c("provcode","yrwk", # column names or variables
         paste0("fira",aggrp), paste0("infc",aggrp),
         paste0("circ",aggrp), paste0("others",aggrp)) 
df <- data.frame(matrix(NA,nrow=length(prv)*length(wk),ncol=length(cnm),dimnames=list(NULL,cnm)))
df$provcode <- rep(prv,each=length(wk))
df$yrwk <- rep(wk,times=length(prv))

# loop weekly time series per cause of death
for (i in names(grp)) {
  
  cat(i," ")
  icd_scod <- grp[[i]]
  d1 <- dat[dat$cause_of_death_3digit %in% icd_scod,]
  u18 <- d1[d1$age2 < 18 & !is.na(d1$age2),]
  
  ag1 <- aggregate(mort~yrwk+place_adm2_pcode, data=d1, FUN="sum")
  ag_u18 <- aggregate(mort~yrwk+place_adm2_pcode, data=u18, FUN="sum")
  
  df[,i] <- ag1$mort[match(paste0(df$provcode,"_",df$yrwk), paste0(ag1$place_adm2_pcode,"_",ag1$yrwk))]
  df[,paste0(i,"_under18")] <- ag_u18$mort[match(paste0(df$provcode,"_",df$yrwk), paste0(ag_u18$place_adm2_pcode,"_",ag_u18$yrwk))]
  
}

# replace NAs with zeroes
df[is.na(df)] <- 0


#rm(list=ls());gc()