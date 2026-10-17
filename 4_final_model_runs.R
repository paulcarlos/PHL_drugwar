# SET WORKING DIRECTORY
setwd("")

# load libraries
library(splines)
library(dlnm)
library(MASS)

# load data
dat <- read.csv("phl_weekly_mort_prov_2010-2020.csv") # weekly data
lwqaic <- read.csv("qaic_lowest_specs_prov.csv") # model specifications, lowest qAIC

# create weeks
dut <- seq(ymd(20160509),ymd(20200126),1) # Duterte election win as president without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))
noy <- seq(ymd(20100630),ymd(20160509),1) # PNoy presidency, minus weeks prior to Duterte election win
noywk <- unique(paste0(isoyear(noy),"-",sprintf("%02d",isoweek(noy))))
noywk <- noywk[noywk!=dutwk[1]]
twk <- c(noywk,dutwk) # full weeks

# add variables 
dat$dwar <- 0 
dat$dwar[dat$yrwk%in%dutwk] <- sapply(dat$yrwk[dat$yrwk%in%dutwk],function(z) grep(z,dutwk)) # War on Drugs weeks
dat$week <- sapply(dat$yrwk, function(z) grep(z,twk)) # weeks for linear term trend
dat$isoweek <- as.numeric(substr(dat$yrwk,6,8)) # ISO weeks for seasonality

# create TC wind indicator
dat$tcwind2 <- ifelse(dat$tcwind>=33,1,0)

# other model specifications
tkn <- c(0.2,0.8) # knots for temperature
prv <- unique(dat$provname) # provinces 
grp <- c("fira","infc","circ","others") # shortnames for causes
nam <- c("Firearm","Infectious","Circulatory","Others") # longnames for causes
nsim <- 1000 # number of simulations



######### LOOP FOR ALL AGES #############

# create blank dataframe and list
dutwk0 <- c("2016-18",dutwk)
rrdf <- data.frame("type"=rep(nam,each=length(prv)*length(dutwk0)), # save relative risks
                   "prov"=rep(prv,each=length(dutwk0),times=length(nam)),
                   "dwar"=rep(dutwk0,times=length(nam)*length(prv)),
                   "week"=rep(1:length(dutwk0),times=length(nam)*length(prv)),
                   "rr"=NA, "lci"=NA, "uci"=NA, "deaths"=NA, "attr"=NA, "attr_low"=NA, "attr_hi"=NA)
sum(duplicated(rrdf))
dlist <- list()

# cutoff for average data pre and post
avcas <- 0.5 # 0.5 per week

# FOR LOOP
for (i in 1:length(grp)) {
  # get cause of death grouping
  g1 <- grp[i] # shortname
  g2 <- nam[i] # longname
  
  cat("\n",g1,"\n")
  
  # loop by province
  for (j in 1:length(prv)) {
    cat(j," ")
    # get province
    p1 <- prv[j]
    
    # get model specifications
    nkpost <- lwqaic[lwqaic$prov==p1,paste0("nkpost_",g1)] # number of knots 
    dfseas <- lwqaic[lwqaic$prov==p1,paste0("dfseas_",g1)] # seasonality degrees of freedom
    tcw <- lwqaic$tcwind[lwqaic$prov==p1] # with or without typhoon
    
    # subset data & add variables
    d1 <- dat[dat$provname==p1,]
    d1$year <- as.integer(substr(d1$yrwk,1,4))
    d1$out <- d1[,g1]
    tempknots <- quantile(d1$t2m,tkn,na.rm=TRUE)

    # small data conditions
    avgpre <- mean(d1$out[d1$dwar==0],na.rm=TRUE)
    avgpost <- mean(d1$out[d1$dwar>0],na.rm=TRUE)
    
    if (avgpre>avcas & avgpost>avcas) {
      
      kpost <- equalknots(d1$dwar,nkpost)
      bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
      
      # run models 
      if (sum(d1$tcwind2>0)>1 & paste0(g1,"_",p1)!="fira_Albay") {
        mod <- glm(out ~ bpost + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to create relative risks
        mod1 <- glm(out ~ bs(dwar, degree=2, knots=kpost) + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to produce the counterfactuals
      
      } else if (paste0(g1,"_",p1)!="fira_Albay") { # no typhoon model
        mod <- glm(out ~ bpost + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to create relative risks
        mod1 <- glm(out ~ bs(dwar, degree=2, knots=kpost) + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to produce the counterfactuals
      
      } else if (paste0(g1,"_",p1)=="fira_Albay") { # remove linear term for week for ALBAY firearms
        mod <- glm(out ~ bpost + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to create relative risks
        mod1 <- glm(out ~ bs(dwar, degree=2, knots=kpost) + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to produce the counterfactuals
      }
      
      # extract actual deaths
      rrdf$deaths[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk0] <- d1[d1$yrwk %in% dutwk0,g1] 
      
      # create counterfactual dataframe
      d0 <- d1[,c("dwar","week","isoweek","t2m","tcwind2","pop")]
      d0$dwar <- 0 # no drug war
      
      # predict with and without drug war
      prd1 <- predict(mod1,type="response",se.fit = TRUE) # predicted
      prd2 <- predict(mod1,newdata=d0,type="response",se.fit=TRUE) # counterfactual
      d1$fit <- prd1$fit
      d1$cfact <- prd2$fit
      
      # save predicted and counterfactuals
      d2 <- d1[,c("provcode","provname","yrwk","year","week","isoweek","dwar","pop","t2m","tcwind2",
                  "out","fit","cfact")]
      d2$type <- g2
      dlist[[paste0(g1,"_",p1)]] <- d2

      # predict Relative Risks
      cp <- crosspred(bpost,mod,cen=0,by=1)
      
      if (!sum(is.infinite(cp$allRRhigh))>0) { # check if coefficients are produced, no Inf or infinites
        
        # insert relative risks, lower and upper CIs
        rrdf$rr[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRfit
        rrdf$lci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRlow
        rrdf$uci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRhigh
          
        # insert attributable deaths
        rrdf$attr[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- ((1-exp(-bpost%*%cp$coefficients))*d1$out)[which(d1$dwar>0)]
        
        # create 1000 simulations
        set.seed(20260723)
        cfsim <- mvrnorm(nsim,cp$coefficients,cp$vcov) # multivariate normal of coefficients
        ansim <- matrix(NA,nrow=length(dutwk),ncol=nsim) # matrix of all simulations
        
        for (s in 1:nsim) {
          ansim[,s] <- ((1-exp(-bpost %*% cfsim[s,]))*d1$out)[which(d1$dwar>0)]
        }
        
        # create 95%eCI of attributable deaths
        rrdf$attr_low[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.025,na.rm=TRUE) # lower bounds
        rrdf$attr_hi[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.975,na.rm=TRUE) # upper bounds

      }
    }  
  }
}

# SAVE 
save(dlist,rrdf,file="modeloutputs_allage.rda")



######### LOOP FOR ALL AGES #############

# create blank dataframe
dutwk0 <- c("2016-18",dutwk)
rrdf <- data.frame("type"=rep(nam,each=length(prv)*length(dutwk0)),
                   "prov"=rep(prv,each=length(dutwk0),times=length(nam)),
                   "dwar"=rep(dutwk0,times=length(nam)*length(prv)),
                   "week"=rep(1:length(dutwk0),times=length(nam)*length(prv)),
                   "rr"=NA, "lci"=NA, "uci"=NA, "deaths"=NA, "attr"=NA, "attr_low"=NA, "attr_hi"=NA)
dlist <- list()

# cutoff for average data pre and post
avcas <- 0.5 # 0.5 per week

# FOR LOOP
for (i in 1:length(grp)) {
  # get cause of death grouping
  g1 <- grp[i] # shortname
  g2 <- nam[i] # longname
  
  cat("\n",g1,"\n")
  
  # loop by province
  for (j in 1:length(prv)) {
    cat(j," ")
    # get province
    p1 <- prv[j]
    
    # get model specifications
    nkpost <- lwqaic[lwqaic$prov==p1,paste0("nkpost_",g1,"_under18")] # number of knots 
    dfseas <- lwqaic[lwqaic$prov==p1,paste0("dfseas_",g1,"_under18")] # seasonality degrees of freedom
    tcw <- lwqaic$tcwind[lwqaic$prov==p1] # with or without typhoon
    
    # subset data & add variables
    d1 <- dat[dat$provname==p1,]
    d1$year <- as.integer(substr(d1$yrwk,1,4))
    d1$out <- d1[,paste0(g1,"_under18")]
    tempknots <- quantile(d1$t2m,tkn,na.rm=TRUE)

    # small data conditions
    avgpre <- mean(d1$out[d1$dwar==0],na.rm=TRUE)
    avgpost <- mean(d1$out[d1$dwar>0],na.rm=TRUE)
    
    if (avgpre>avcas & avgpost>avcas) {
      
      kpost <- equalknots(d1$dwar,nkpost)
      bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
      
      # run models 
      if (sum(d1$tcwind2>0)>1 & paste0(g1,"_",p1)!="fira_Albay") {
        mod <- glm(out ~ bpost + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to create relative risks
        mod1 <- glm(out ~ bs(dwar, degree=2, knots=kpost) + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to produce the counterfactuals
      
      } else if (paste0(g1,"_",p1)!="fira_Albay") { # no typhoon model
        mod <- glm(out ~ bpost + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to create relative risks
        mod1 <- glm(out ~ bs(dwar, degree=2, knots=kpost) + week + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to produce the counterfactuals
      
      } else if (paste0(g1,"_",p1)=="fira_Albay") { # remove linear term for week for ALBAY firearms
        mod <- glm(out ~ bpost + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to create relative risks
        mod1 <- glm(out ~ bs(dwar, degree=2, knots=kpost) + ns(isoweek,df=dfseas) + ns(t2m,knots=tempknots) + tcwind2 + offset(log(pop)),
                   family=quasipoisson, data=d1, na.action="na.exclude") # model to produce the counterfactuals
      }
      
      # extract actual deaths
      rrdf$deaths[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- d1[d1$yrwk %in% dutwk,g1] 
      
      # create counterfactual dataframe
      d0 <- d1[,c("dwar","week","isoweek","t2m","tcwind2","pop")]
      d0$dwar <- 0 # no drug war
      
      # predict with and without drug war
      prd1 <- predict(mod1,type="response",se.fit = TRUE) # predicted
      prd2 <- predict(mod1,newdata=d0,type="response",se.fit=TRUE) # counterfactual
      d1$fit <- prd1$fit
      d1$cfact <- prd2$fit
      
      # save predicted and counterfactuals
      d2 <- d1[,c("provcode","provname","yrwk","year","week","isoweek","dwar","pop","t2m","tcwind2",
                  "out","fit","cfact")]
      d2$type <- g2
      dlist[[paste0(g1,"_",p1)]] <- d2
      
      # predict Relative Risks
      cp <- crosspred(bpost,mod,cen=0,by=1)
      
      if (!sum(is.infinite(cp$allRRhigh))>0) { # check if coefficients are produced, no Inf or infinites
        
        # insert relative risks, lower and upper CIs
        rrdf$rr[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRfit
        rrdf$lci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRlow
        rrdf$uci[rrdf$type==g2 & rrdf$prov==p1] <- cp$allRRhigh
          
        # insert attributable deaths
        rrdf$attr[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- ((1-exp(-bpost%*%cp$coefficients))*d1$out)[which(d1$dwar>0)]
        
        # create 1000 simulations
        set.seed(20260723)
        cfsim <- mvrnorm(nsim,cp$coefficients,cp$vcov) # multivariate normal of coefficients
        ansim <- matrix(NA,nrow=length(dutwk),ncol=nsim) # matrix of all simulations
        
        for (s in 1:nsim) {
          ansim[,s] <- ((1-exp(-bpost %*% cfsim[s,]))*d1$out)[which(d1$dwar>0)]
        }
        
        # create 95%eCI of attributable deaths
        rrdf$attr_low[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.025,na.rm=TRUE) # lower bounds
        rrdf$attr_hi[rrdf$type==g2 & rrdf$prov==p1 & rrdf$dwar%in% dutwk] <- apply(ansim,1,quantile,0.975,na.rm=TRUE) # upper bounds

      }
    }  
  }
}

# SAVE 
save(dlist,rrdf,file="modeloutputs_under18.rda")


rm(list=ls());gc()
