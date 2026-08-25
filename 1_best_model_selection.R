Sys.setLanguage("en")
library(lubridate)
library(splines)
library(dlnm)
library(MASS)
library(pbs)
library(zoo)
library(scales)

source("qaicbic.R") # Function to calculate quasi-Akaike Information Criterion

# create pre-post weeks
dut <- seq(ymd(20160509),ymd(20200126),1) # Duterte election win as president excluding COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))
noy <- seq(ymd(20100630),ymd(20160509),1) # Aquino presidency, minus weeks prior to Duterte election win
noywk <- unique(paste0(isoyear(noy),"-",sprintf("%02d",isoweek(noy))))
noywk <- noywk[noywk!=dutwk[1]]
twk <- c(noywk,dutwk)

# upload data
rdat <- readRDS("phl_weekly_mort_prov_2006-2024.rds") # NOT PUBLICLY SHAREABLE

# subset data
dat <- rdat[rdat$yrwk%in%twk,]
grp <- c("fira","aslt","unde","pneu","sep","infc","hpn","mi","cva","unk","others") # causes of deaths
agegrp <- c("under18","over18")

# add Drug War Period
dat$dwar <- 0
dat$dwar[dat$yrwk%in%dutwk] <- sapply(dat$yrwk[dat$yrwk%in%dutwk],function(z) grep(z,dutwk))
dat$week <- sapply(dat$yrwk, function(z) grep(z,twk))
dat$isoweek <- as.numeric(substr(dat$yrwk,6,8))

# typhoon categorical, >=33 meter per second
summary(dat$tcwind)
dat$tcwind2 <- ifelse(dat$tcwind>=33,1,0)

# model specifications
nkpost <- seq(5,13,1) # number of knots for post weeks or drug war weeks
dfseas <- 2:7 # number of degrees of freedom for seasonality

# blank dataframe to save qAIC values
prv <- unique(dat$provname)
mspec <- data.frame("prov"=rep(prv,each=length(nkpost)*length(dfseas)),
                    "nkpost"=rep(nkpost,times=length(prv),each=length(dfseas)),
                    "dfseas"=rep(dfseas,times=length(prv)*length(nkpost)),"tcwind"=NA)
cnm <- do.call(paste0,expand.grid(grp,paste0("_",agegrp)))
mspec1 <- data.frame(matrix(NA,nrow=nrow(mspec),ncol=length(c(grp,cnm)), dimnames=list(NULL,c(grp,cnm))))
mspec <- cbind(mspec,mspec1)
#sum(duplicated(mspec))

# loop models and qAIC extraction
for (i in 1:nrow(mspec)) {
  p1 <- mspec$prov[i] # name of province
  
  # subset weekly deaths 
  d1 <- dat[dat$provname==p1,]
  
  # model specifications
  nkp <- mspec$nkpost[i] # knots for drug war weeks
  dfs <- mspec$dfseas[i] # degrees of freedom for seasonality
  kpost <- equalknots(d1$dwar,nkp) 
  tkn <- quantile(d1$t2m,c(0.2,0.8)) # knots for temperature
  
  # B-spline for drug war weeks
  bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
  
  # loop by cause of death
  for (j in grp) {
    d1$out <- d1[,j] # deaths for all ages
    d1$under18 <- d1[,paste0(j,"_under18")] # deaths of <18 years old
    d1$over18 <- d1[,paste0(j,"_over18")] # deaths over 18 years old
    
    if (sum(d1$tcwind2)>1) { # if condition for provinces with typhoons
      
      mspec$tcwind[i] <- 1
      
      # model for all-ages
      trc1 <- tryCatch(glm(out~bpost+ns(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc1)=="glm")) {
        mspec[i,paste0(j)] <- qaicbic(trc1)[1]
      }
      
      # model for <18 yrs old
      trc2 <- tryCatch(glm(under18~bpost+ns(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc2)=="glm")) {
        mspec[i,paste0(j,"_under18")] <- qaicbic(trc2)[1]
      }
      
      # model for >18 yrs old
      trc3 <- tryCatch(glm(over18~bpost+ns(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc3)=="glm")) {
        mspec[i,paste0(j,"_over18")] <- qaicbic(trc3)[1]
      }
      
    } else { # provinces without typhoons
      
      mspec$tcwind[i] <- 0
      
      # model for all-ages
      trc1 <- tryCatch(glm(out~bpost+ns(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc1)=="glm")) {
        mspec[i,paste0(j)] <- qaicbic(trc1)[1]
      }
      
      # model for <18 yrs old
      trc2 <- tryCatch(glm(under18~bpost+ns(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc2)=="glm")) {
        mspec[i,paste0(j,"_under18")] <- qaicbic(trc2)[1]
      }
      
      # model for >18 yrs old
      trc3 <- tryCatch(glm(over18~bpost+ns(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc3)=="glm")) {
        mspec[i,paste0(j,"_over18")] <- qaicbic(trc3)[1]
      }
      
    }
    
  }
  
  cat(i," ")
}
rm(i,p1,d1,nkp,dfs,kpost,bpost,tkn,trc1,trc2,trc3)
saveRDS(mspec,"qaic_allspecs_prov.rds")

# find lowest qAIC values
cnm2 <- do.call(paste0,expand.grid(c("nkpost_","dfseas_"),c(grp,cnm)))
lwqaic <- data.frame(matrix(NA,nrow=length(prv), ncol=length(cnm2)+2, dimnames=list(NULL,c("prov","tcwind",cnm2))))
lwqaic$prov <- prv

for (i in 1:nrow(lwqaic)) {
  #i=1
  p1 <- lwqaic$prov[i]
  d1 <- mspec[mspec$prov==p1,]
  
  for (j in grp) {
    
    pq <- d1[,paste0(j)]
    if (!sum(is.na(pq))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j)] <- d1$nkpost[which.min(pq)][1]
      lwqaic[i,paste0("dfseas_",j)] <- d1$dfseas[which.min(pq)][1]
      lwqaic$tcwind[i] <- d1$tcwind[which.min(pq)]
    }
    
    pq_under18 <- d1[,paste0(j,"_under18")]
    if (!sum(is.na(pq_under18))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j,"_under18")] <- d1$nkpost[which.min(pq_under18)][1]
      lwqaic[i,paste0("dfseas_",j,"_under18")] <- d1$dfseas[which.min(pq_under18)][1]
      lwqaic$tcwind[i] <- d1$tcwind[which.min(pq_under18)]
    }
    
    pq_over18 <- d1[,paste0(j,"_over18")]
    if (!sum(is.na(pq_over18))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j,"_under18")] <- d1$nkpost[which.min(pq_under18)][1]
      lwqaic[i,paste0("dfseas_",j,"_over18")] <- d1$dfseas[which.min(pq_over18)][1]
      lwqaic$tcwind[i] <- d1$tcwind[which.min(pq_over18)]
    }
  }
  
}
rm(i,p1,d1)
saveRDS(lwqaic,"qaic_lowest_specs_prov.rds")


#rm(list=ls());gc()