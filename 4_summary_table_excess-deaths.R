Sys.setLanguage("en")

library(lubridate)

# drug war weeks
dut <- seq(ymd(20160630),ymd(20200126),1) # Duterte presidency without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))

# upload data
rrdf <- read.csv("relativerisks_allage.csv")
all <- rrdf[!is.na(rrdf$rr) & rrdf$dwar %in% dutwk,]
rrdf <- read.csv("relativerisks_allage.csv")
u18 <- rrdf[!is.na(rrdf$rr) & rrdf$dwar %in% dutwk,]
rm(rrdf)

# get significant week table
s1 <- read.csv("fira_signifweeks.csv")
s2 <- read.csv("others_signifweeks.csv")
s1$type <- "Firearms"
s1 <- s1[,colnames(s2)]
swk <- rbind(s1,s2)
swk <- swk[swk$signif_week %in% dutwk,]
rm(s1,s2)

# causes of death 
nam <- c("Firearms","Assault","Undetermined Intent",
         "Pneumonia","Sepsis","Other Infectious",
         "Hypertension","Myocardial Infarction","Cerebrovascular Accident",
         "Unknown Cause","Other Causes")
extl <- nam[1:3] # external causes
ntrl <- nam[4:length(nam)] # natural causes

# create guide for loop
cons <- swk[!duplicated(swk[,c("prov","type")]), c("prov","type")]
gtab <- all[!duplicated(all[,c("prov","type")]), c("prov","type")]
rownames(gtab) <- 1:nrow(gtab)
gtab$cons <- 0
gtab$cons[match(paste0(cons$prov,"-",cons$type), paste0(gtab$prov,"-",gtab$type))] <- 1

# loop total numbers
coln <- c("type","prov","age","cause","kill","lci","uci")
sdt <- data.frame(matrix(NA, nrow=0, ncol=length(coln), dimnames=list(NULL, coln)))

for (i in 1:nrow(gtab)) {
  #i=54
  prv <- gtab$prov[i]
  typ <- gtab$type[i]
  
  if (typ %in% extl) {
    
    if (nrow(all[all$prov == prv & all$type == typ & all$lci>1, ])>0) {
      
      a0 <- all[all$prov == prv & all$type == typ & all$lci>1, ]
      a1 <- a0
      s0 <- data.frame("type"="uncons","prov"=prv, "age"="all", "cause"=typ, "kill"=sum(a0$attr), "lci"=sum(a0$attr_low), "uci"=sum(a0$attr_hi))
      s1 <- data.frame("type"="cons","prov"=prv, "age"="all", "cause"=typ, "kill"=sum(a1$attr), "lci"=sum(a1$attr_low), "uci"=sum(a1$attr_hi))
      sdt <- rbind(sdt,s0,s1)
      
      if (nrow(u18[u18$prov == prv & u18$type == typ, ])>0) {
        u0 <- u18[u18$prov == prv & u18$type == typ & u18$lci > 1, ]
        u1 <- u0
        s2 <- data.frame("type"="uncons","prov"=prv, "age"="u18", "cause"=typ, "kill"=sum(u0$attr), "lci"=sum(u0$attr_low), "uci"=sum(u0$attr_hi))
        s3 <- data.frame("type"="cons","prov"=prv, "age"="u18", "cause"=typ, "kill"=sum(u1$attr), "lci"=sum(u1$attr_low), "uci"=sum(u1$attr_hi))
        sdt <- rbind(sdt,s2,s3)
      } 
      
    } else {
      s1 <- data.frame("type"="uncons","prov"=prv, "age"=c("all","u18"), "cause"=typ, "kill"=0, "lci"=0, "uci"=0)
      s2 <- data.frame("type"="cons","prov"=prv, "age"=c("all","u18"), "cause"=typ, "kill"=0, "lci"=0, "uci"=0)
      sdt <- rbind(sdt,s1,s2)
    }
    
  } else {
    
    d1 <- swk[swk$prov == prv & swk$type == typ, ] # significantly positive weeks
    
    if (nrow(d1)>0) {
      
      a0 <- all[all$prov == prv & all$type == typ & all$lci>1, ]
      a1 <- all[all$prov == prv & all$type == typ & all$lci>1 & all$dwar %in% d1$signif_week, ]
      s0 <- data.frame("type"="uncons","prov"=prv, "age"="all", "cause"=typ, "kill"=sum(a0$attr), "lci"=sum(a0$attr_low), "uci"=sum(a0$attr_hi))
      s1 <- data.frame("type"="cons","prov"=prv, "age"="all", "cause"=typ, "kill"=sum(a1$attr), "lci"=sum(a1$attr_low), "uci"=sum(a1$attr_hi))
      sdt <- rbind(sdt,s0,s1)
      
      if (nrow(u18[u18$prov == prv & u18$type == typ & u18$dwar %in% d1$signif_week, ])>0) {
        u0 <- u18[u18$prov == prv & u18$type == typ & u18$lci > 1, ]
        u1 <- u18[u18$prov == prv & u18$type == typ & u18$lci > 1 & u18$dwar %in% d1$signif_week , ]
        s2 <- data.frame("type"="uncons","prov"=prv, "age"="u18", "cause"=typ, "kill"=sum(u0$attr), "lci"=sum(u0$attr_low), "uci"=sum(u0$attr_hi))
        s3 <- data.frame("type"="cons","prov"=prv, "age"="u18", "cause"=typ, "kill"=sum(u1$attr), "lci"=sum(u1$attr_low), "uci"=sum(u1$attr_hi))
        sdt <- rbind(sdt,s2,s3)
      } 
      
    } else {
      s1 <- data.frame("type"="uncons","prov"=prv, "age"=c("all","u18"), "cause"=typ, "kill"=0, "lci"=0, "uci"=0)
      s2 <- data.frame("type"="cons","prov"=prv, "age"=c("all","u18"), "cause"=typ, "kill"=0, "lci"=0, "uci"=0)
      sdt <- rbind(sdt,s1,s2)
    }
  }
  
  
  
}
rm(prv,typ,d1,a0,a1,s0,s1,s2,s3,i)

# SAVE
write.csv(sdt,"summary_kills_prov-type.csv",row.names = FALSE)


#rm(list=ls());gc()
