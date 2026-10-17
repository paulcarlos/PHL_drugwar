
# SET WORKING DIRECTORY
setwd("")

# load libraries
library(splines)
library(dlnm)
source("qaicbic.R") # function for qAIC and qBIC

# Weeks, pre and post
dut <- seq(ymd(20160509),ymd(20200126),1) # Duterte election win as president without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))
noy <- seq(ymd(20100630),ymd(20160509),1) # PNoy presidency, minus weeks prior to Duterte election win
noywk <- unique(paste0(isoyear(noy),"-",sprintf("%02d",isoweek(noy))))
noywk <- noywk[noywk!=dutwk[1]]
twk <- c(noywk,dutwk) # full weeks

# load weekly dataset
dat <- read.csv("phl_weekly_mort_prov_2010-2020.csv")

# categories
grp <- c("fira", "infc", "circ", "others") # short names for causes of death
agegrp <- "under18" # age groups, blank is all ages

# adjust TC wind
dat$tcwind2 <- ifelse(dat$tcwind>=33,1,0)

# add variables 
dat$dwar <- 0 
dat$dwar[dat$yrwk%in%dutwk] <- sapply(dat$yrwk[dat$yrwk%in%dutwk],function(z) grep(z,dutwk)) # War on Drugs weeks
dat$week <- sapply(dat$yrwk, function(z) grep(z,twk)) # weeks for linear term trend
dat$isoweek <- as.numeric(substr(dat$yrwk,6,8)) # ISO weeks for seasonality

# specifications to be tested
nkpost <- seq(5,13,1) # knots for War on Drugs week
dfseas <- 2:6 # seasonality

# blank dataframe to save qAIC values
prv <- unique(dat$provname)
mspec <- data.frame("prov"=rep(prv,each=length(nkpost)*length(dfseas)),
                    "nkpost"=rep(nkpost,times=length(prv),each=length(dfseas)),
                    "dfseas"=rep(dfseas,times=length(prv)*length(nkpost)),"tcwind"=NA)
cnm <- do.call(paste0,expand.grid(grp,paste0("_",agegrp)))
mspec1 <- data.frame(matrix(NA,nrow=nrow(mspec),ncol=length(c(grp,cnm)), dimnames=list(NULL,c(grp,cnm))))
mspec <- cbind(mspec,mspec1)

# FOR LOOP for eac combination of specs
for (i in 1:nrow(mspec)) {
  # get province name
  p1 <- mspec$prov[i] 

  # subset data at province level
  d1 <- dat[dat$provname==p1,]

  # get the specifications
  nkp <- mspec$nkpost[i] # knots for post event
  kpost <- equalknots(d1$dwar,nkp) # equalknots from number of knots
  dfs <- mspec$dfseas[i] # seasonality degrees of freedom
  tkn <- quantile(d1$t2m,c(0.2,0.8)) # temperature knots at 20th and 80th percentiles
  
  # spline for post event or War on Drugs weeks
  bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
  
  # FOR LOOP by cause of death
  for (j in grp) {
    d1$out <- d1[,j] # outcome for all ages
    d1$under18 <- d1[,paste0(j,"_under18")] # outcome for under 18
     
    if (sum(d1$tcwind2)>1) { # check if there are TC winds
      
      mspec$tcwind[i] <- 1
      
      # model test fo all ages
      trc1 <- tryCatch(glm(out ~ bpost + week + ns(isoweek,df=dfs) + ns(t2m,knots=tkn) + tcwind2 + offset(log(pop)),
                          family=quasipoisson,data=d1,na.action="na.exclude"),
                      error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc1)=="glm")) {
        mspec[i,paste0(j)] <- qaicbic(trc1)[1]
      }
      
      # model test for under 18
      trc2 <- tryCatch(glm(under18 ~ bpost + week + ns(isoweek,df=dfs) + ns(t2m,knots=tkn) + tcwind2 + offset(log(pop)),
                          family=quasipoisson,data=d1,na.action="na.exclude"),
                      error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc2)=="glm")) {
        mspec[i,paste0(j,"_under18")] <- qaicbic(trc2)[1]
      }
                
    } else { # models without TCWIND variable
      
      mspec$tcwind[i] <- 0
      
      # model for all ages
      trc1 <- tryCatch(glm(out ~ bpost + week + ns(isoweek,df=dfs) + ns(t2m,knots=tkn) + offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc1)=="glm")) {
        mspec[i,paste0(j)] <- qaicbic(trc1)[1]
      }
      
      # model for under 18
      trc2 <- tryCatch(glm(under18 ~ bpost + week + ns(isoweek,df=dfs) + ns(t2m,knots=tkn) + offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc2)=="glm")) {
        mspec[i,paste0(j,"_under18")] <- qaicbic(trc2)[1]
      }
         
    }
    
  }
  
  cat(i," ")
}
rm(i,p1,d1,nkp,dfs,kpost,bpost,tkn,trc1,trc2)

# create blank dataframe to save final model specifications
cnm2 <- do.call(paste0,expand.grid(c("nkpost_","dfseas_"),c(grp,cnm)))
lwqaic <- data.frame(matrix(NA,nrow=length(prv), ncol=length(cnm2)+2, dimnames=list(NULL,c("prov","tcwind",cnm2))))
lwqaic$prov <- prv

# FOR LOOP to find lowest qAIC
for (i in 1:nrow(lwqaic)) {
  
  # subset the province and specifications
  p1 <- lwqaic$prov[i]
  d1 <- mspec[mspec$prov==p1,]
  
  # loop by cause of death
  for (j in grp) {
    
    # get lowest qAIC for all ages
    pq <- d1[,j] # get outcome
    if (!sum(is.na(pq))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j)] <- d1$nkpost[which.min(pq)][1]
      lwqaic[i,paste0("dfseas_",j)] <- d1$dfseas[which.min(pq)][1]
      lwqaic$tcwind[i] <- d1$tcwind[which.min(pq)]
    }
    
    # get lowest qAIC for under 18
    pq_under18 <- d1[,paste0(j,"_under18")]
    if (!sum(is.na(pq_under18))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j,"_under18")] <- d1$nkpost[which.min(pq_under18)][1]
      lwqaic[i,paste0("dfseas_",j,"_under18")] <- d1$dfseas[which.min(pq_under18)][1]
      lwqaic$tcwind[i] <- d1$tcwind[which.min(pq_under18)]
    }
    
  }
  
}


# SAVE DATA
write.csv(lwqaic,"qaic_lowest_specs_prov.csv",row.names=FALSE)

rm(list=ls());gc()

