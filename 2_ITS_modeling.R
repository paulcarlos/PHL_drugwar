Sys.setLanguage("en")
library(lubridate)
library(splines)
library(dlnm)
library(MASS)
library(pbs)
library(zoo)
library(scales)

# Pre & post weeks
dut <- seq(ymd(20160509),ymd(20200126),1) # Duterte election win as president without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))
noy <- seq(ymd(20100630),ymd(20160509),1) # PNoy presidency, minus weeks prior to Duterte election win
noywk <- unique(paste0(isoyear(noy),"-",sprintf("%02d",isoweek(noy))))
noywk <- noywk[noywk!=dutwk[1]]
twk <- c(noywk,dutwk)

# upload data
rdat0 <- readRDS("0_data/1_combined_data/phl_weekly_mort_prov_2006-2024.rds") # CANNOT PUBLICLY SHARE

# remove southern provinces with poor data
prvexcl <- c("Tawi-Tawi","Sulu","Basilan & City of Isabela","Maguindanao & Cotabato City","Lanao del Sur")
rdat <- rdat0[!rdat0$provname %in% prvexcl,]

# retain provinces with ACLED data
acled <- read.csv("acled_daily_2026-07-20.csv")
rdat <- rdat[rdat$provname %in% acled$provname,]
length(unique(rdat$provname))

# subset to pre & post weeks
dat <- rdat[rdat$yrwk%in%twk,]

# add Drug War Period, post week
dat$dwar <- 0
dat$dwar[dat$yrwk%in%dutwk] <- sapply(dat$yrwk[dat$yrwk%in%dutwk],function(z) grep(z,dutwk))
dat$week <- sapply(dat$yrwk, function(z) grep(z,twk))
dat$isoweek <- as.numeric(substr(dat$yrwk,6,8))

# typhoon weeks, with >=33 m/s max wind
dat$tcwind2 <- ifelse(dat$tcwind>=33,1,0)

# model specifications
lwqaic <- readRDS("qaic_lowest_specs_prov.rds")
tempknots <- c(0.2,0.8)
prv <- unique(dat$provname)
grp <- c("fira","aslt","unde","pneu","sep","infc","hpn","mi","cva","unk","others")
nam <- c("Firearms","Assault","Undetermined Intent",
         "Pneumonia","Sepsis","Other Infectious",
         "Hypertension","Myocardial Infarction","Cerebrovascular Accident",
         "Unknown Cause","Other Causes")
nsim <- 1000


############## LOOP MODEL PER PROVINCE, ALL AGE ################

# list and dataframe to save
modlist <- list()
dutwk0 <- c("2016-18",dutwk)
rrdf <- data.frame("type"=rep(nam,each=length(prv)*length(dutwk0)),
                   "prov"=rep(prv,each=length(dutwk0),times=length(nam)),
                   "dwar"=rep(dutwk0,times=length(nam)*length(prv)),
                   "week"=rep(1:length(dutwk0),times=length(nam)*length(prv)),
                   "rr"=NA, "lci"=NA, "uci"=NA, "deaths"=NA, "attr"=NA, "attr_low"=NA, "attr_hi"=NA)
#sum(duplicated(rrdf))

# weeks in X-axis
wks <- seq(1,length(dutwk0),20)

# loop model and plots
avcas <- 0.5 # average weekly cases cutoff
for (i in 1:length(grp)) {
  g1 <- grp[i]
  g2 <- nam[i]
  cat("\n",g1,"\n")
  
  pdf(paste0("rrplots/suppl_rrplot_",g1,"_prov.pdf"), width=14, height=7)
  par(mfrow=c(3,3), mar=c(4,4,3,1), oma=c(1,1,1,1))
  
  for (j in 1:length(prv)) {
    cat(j," ")
    p1 <- prv[j]
    nkpost <- lwqaic[lwqaic$prov==p1,paste0("nkpost_",g1)]
    dfseas <- lwqaic[lwqaic$prov==p1,paste0("dfseas_",g1)]
    tcw <- lwqaic$tcwind[lwqaic$prov==p1]
    
    d1 <- dat[dat$provname==p1,]
    d1$out <- d1[,paste0(g1)]
    
    # data check
    avgpre <- mean(d1$out[d1$dwar==0],na.rm=TRUE)
    avgpost <- mean(d1$out[d1$dwar>0],na.rm=TRUE)
    
    if (avgpre>avcas & avgpost>avcas) {
      
      kpost <- equalknots(d1$dwar,nkpost)
      bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
      
      if (sum(d1$tcwind2>0)>1) {
        mod <- glm(out ~ bpost + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude")
      } else {
        mod <- glm(out ~ bpost + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude")
      }
      
      modlist[[paste0(g1,"_",p1)]] <- mod
      rrdf$deaths[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- d1[d1$yrwk %in% dutwk,g1]
      
      cp <- crosspred(bpost,mod,cen=0,by=1)
      if (sum(is.infinite(cp$allRRhigh))>0) {
        
        plot(NA, main=paste0(p1," - ",g2," (NULL)"), ylab="RR (95%CI)", ylim=c(0.8,7), xlab="Drug War Weeks", xlim=c(0,length(dutwk)))
        
      } else {
        
        plot(cp, main=paste0(p1," - ",g2), ylab="RR (95%CI)", xlab="Drug War Weeks",xaxt="n")
        axis(1, at=wks, labels=dutwk0[wks])
        
        rrdf$rr[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRfit
        rrdf$lci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRlow
        rrdf$uci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRhigh
        
        rrdf$attr[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- ((1-exp(-bpost%*%cp$coefficients))*d1$out)[which(d1$dwar>0)]
        
        set.seed(20260723)
        cfsim <- mvrnorm(nsim,cp$coefficients,cp$vcov)
        
        ansim <- matrix(NA,nrow=length(dutwk),ncol=nsim)
        for (s in 1:nsim) {
          #s=1
          ansim[,s] <- ((1-exp(-bpost %*% cfsim[s,]))*d1$out)[which(d1$dwar>0)]
        }
        rrdf$attr_low[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.025,na.rm=TRUE)
        rrdf$attr_hi[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.975,na.rm=TRUE)
      }
      
    } else {
      
      plot(NA, main=paste0(p1," - ",g2," (NULL)"), ylab="RR (95%CI)", ylim=c(0.8,7), xlab="Drug War Weeks", xlim=c(0,length(dutwk)))
      
    }
    
  }
  dev.off()
}

# save
save(modlist,rrdf, file="modeloutputs_allage.rda")




############## LOOP MODEL PER PROVINCE, UNDER 18 ################

# list and dataframe to save
modlist <- list()
dutwk0 <- c("2016-18",dutwk)
rrdf <- data.frame("type"=rep(nam,each=length(prv)*length(dutwk0)),
                   "prov"=rep(prv,each=length(dutwk0),times=length(nam)),
                   "dwar"=rep(dutwk0,times=length(nam)*length(prv)),
                   "week"=rep(1:length(dutwk0),times=length(nam)*length(prv)),
                   "rr"=NA, "lci"=NA, "uci"=NA, "deaths"=NA, "attr"=NA, "attr_low"=NA, "attr_hi"=NA)
#sum(duplicated(rrdf))

# weeks in X-axis
wks <- seq(1,length(dutwk0),20)

# loop model and plots
avcas <- 0.5 # average number of cases per week, as cutoff

for (i in 1:length(grp)) { # by cause of death
  g1 <- grp[i]
  g2 <- nam[i]
  cat("\n",g1,"\n")
  
  pdf(paste0("rrplots/suppl_rrplot_",g1,"_prov_under18.pdf"), width=14, height=7)
  par(mfrow=c(3,3), mar=c(4,4,3,1), oma=c(1,1,1,1))
  
  for (j in 1:length(prv)) { # by province
    cat(j," ")
    p1 <- prv[j]
    nkpost <- lwqaic[lwqaic$prov==p1,paste0("nkpost_",g1,"_under18")]
    dfseas <- lwqaic[lwqaic$prov==p1,paste0("dfseas_",g1,"_under18")]
    tcw <- lwqaic$tcwind[lwqaic$prov==p1]
    
    if (sum(is.na(c(nkpost,dfseas)))==0) {
      
      d1 <- dat[dat$provname==p1,]
      d1$out <- d1[,paste0(g1,"_under18")]
      
      # data check
      avgpre <- mean(d1$out[d1$dwar==0],na.rm=TRUE)
      avgpost <- mean(d1$out[d1$dwar>0],na.rm=TRUE)
      
      if (avgpre>avcas & avgpost>avcas) {
        
        kpost <- equalknots(d1$dwar,nkpost)
        bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
        
        if (tcw==1) {
          mod <- glm(out ~ bpost + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                     family=quasipoisson, data=d1, na.action="na.exclude")
        } else {
          mod <- glm(out ~ bpost + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + offset(log(pop)),
                     family=quasipoisson, data=d1, na.action="na.exclude")
        }
        
        modlist[[paste0(g1,"_",p1)]] <- mod
        rrdf$deaths[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- d1[d1$yrwk %in% dutwk,g1]
        
        cp <- crosspred(bpost,mod,cen=0,by=1)
        
        if (sum(is.infinite(cp$allRRhigh))>0) {
          
          plot(NA, main=paste0("u18 ",p1," - ",g2," (NULL)"), ylab="RR (95%CI)", ylim=c(0.8,7), xlab="Drug War Weeks", xlim=c(0,length(dutwk)))
          
        } else {
          
          plot(cp, main=paste0("u18 ",p1," - ",g2), ylab="RR (95%CI)", xlab="Drug War Weeks",xaxt="n")
          axis(1, at=wks, labels=dutwk0[wks])
          
          rrdf$rr[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRfit
          rrdf$lci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRlow
          rrdf$uci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRhigh
          
          rrdf$attr[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- ((1-exp(-bpost%*%cp$coefficients))*d1$out)[which(d1$dwar>0)]
          
          set.seed(20260723)
          cfsim <- mvrnorm(nsim,cp$coefficients,cp$vcov)
          
          ansim <- matrix(NA,nrow=length(dutwk),ncol=nsim)
          for (s in 1:nsim) {
            #s=1
            ansim[,s] <- ((1-exp(-bpost %*% cfsim[s,]))*d1$out)[which(d1$dwar>0)]
          }
          rrdf$attr_low[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.025,na.rm=TRUE)
          rrdf$attr_hi[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.975,na.rm=TRUE)
        }
      } else {
        plot(NA, main=paste0("u18 ",p1," - ",g2," (NULL)"), ylab="RR (95%CI)", ylim=c(0.8,7), xlab="Drug War Weeks", xlim=c(0,length(dutwk)))
      }
      
    } else {
      
      plot(NA, main=paste0("u18 ",p1," - ",g2," (NULL)"), ylab="RR (95%CI)", ylim=c(0.8,7), xlab="Drug War Weeks", xlim=c(0,length(dutwk)))
      
    }
  }
  dev.off()
}
sum(rrdf$attr[rrdf$type=="Firearms" & rrdf$lci>1 & !is.na(rrdf$lci)])

# save
save(modlist,rrdf, file="modeloutputs_under18.rda")

#rm(list=ls());gc()
