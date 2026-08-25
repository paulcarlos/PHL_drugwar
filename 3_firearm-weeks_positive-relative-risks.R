
######### SELECT FIREARM WEEKs WITH POSITIVE RRs ##########

Sys.setLanguage("en")

# load data for ALL AGES
rrdf <- read.csv("relativerisks_allage.csv")

# subset rows for Firearms and with RR values
d1 <- rrdf[!is.na(rrdf$rr) & rrdf$type == "Firearms",]

# dataframe to save information
prov <- unique(d1$prov)
coln <- c("prov","signif_week")
sdt1 <- data.frame(matrix(NA, nrow=0, ncol=length(coln), dimnames=list(NULL,coln))) 

# loop search of significantly positive relative risks
for (i in prov) {
  #i=prov[1]
  d2 <- d1[d1$prov == i,]
  
  dt <- d2$dwar[d2$lci>1] # select weeks that are significantly positive
  
  if (length(dt)>0) {
    s1 <- data.frame("prov"=i,"signif_week"=dt)
    sdt1 <- rbind(sdt1,s1)
  }
  
}
rm(d2,dt,s1)

#save
write.csv(sdt1, "fira_signifweeks.csv", row.names=FALSE)

rm(list=ls()); gc()




######### POSITIVE WEEKS FOR OTHER CAUSES, CONINCIDING FIREARM WEEKS OR ACLED VIOLENCE ##########

Sys.setLanguage("en")

# load data for ALL AGES
rrdf <- read.csv("relativerisks_allage.csv")
wpat <- read.csv("fira_signifweeks.csv")
acled <- read.csv("acled_weekly_2026-07-20.csv")

# subset rows with RRs
d1 <- rrdf[!is.na(rrdf$rr),]

# natural causes of death
nam <- c("Pneumonia","Sepsis","Other Infectious",
         "Hypertension","Myocardial Infarction","Cerebrovascular Accident",
         "Unknown Cause","Other Causes")

# loop search
d2 <- d1[d1$type %in% nam,]
gd1 <- d2[!duplicated(d2[,c("prov","type")]),c("prov","type")]
gd1$pos <- 0
coln <- c("prov","type","signif_week")
sdt1 <- data.frame(matrix(NA, nrow=0, ncol=length(coln), dimnames=list(NULL,coln))) 

for (i in 1:nrow(gd1)) {
  prv <- gd1$prov[i]
  typ <- gd1$type[i]
  
  d3 <- d2[d2$prov==prv & d2$type==typ,] # subset data of selected province and cause of death
  
  if (sum(d3$lci>1)>0) {
    
    if (prv %in% wpat$prov) {
      a1 <- wpat[wpat$prov == prv,]
      
      # check significant weeks
      if (nrow(a1)>0) {
        dt <- d3$dwar[d3$lci > 1]
        swk <- intersect(dt,a1$signif_week)
        
        if (length(swk)>0) {
          s1 <- data.frame("prov"=prv,"type"=typ,"signif_week"=swk)
          sdt1 <- rbind(sdt1,s1)
          gd1$pos[i] <- 1
        }
        
      }
      
    } else {
      
      # create reference weeks
      a1 <- acled[acled$provname == prv & acled$yrwk %in% d3$dwar & acled$fatalities > 1,]
      
      # check significant weeks
      if (nrow(a1)>0) {
        dt <- d3$dwar[d3$lci > 1]
        swk <- intersect(dt,a1$yrwk)
        
        if (length(swk)>0) {
          s1 <- data.frame("prov"=prv,"type"=typ,"signif_week"=swk)
          sdt1 <- rbind(sdt1,s1)
          gd1$pos[i] <- 1
        }
      }
      
    }
    
  }
  
}


# SAVE
write.csv(sdt1,"others_signifweeks.csv", row.names= FALSE)


rm(list=ls());gc()


