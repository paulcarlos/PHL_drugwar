# SET WORKING DIRECTORY
setwd("")
library(ggplot2)

# week values
dut <- seq(ymd(20160630),ymd(20200126),1) # Duterte presidency without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))

# names
nam <- c("Firearm","Infectious","Circulatory","Others")

# get ACLED sum
acled0 <- read.csv("acled_weekly_2026-07-20.csv")
prvexcl <- c("Tawi-Tawi","Sulu","Basilan & City of Isabela","Maguindanao & Cotabato City","Lanao del Sur")
acled <- sum(acled0$fatalities[!acled0$prov %in% prvexcl & acled0$yrwk %in% dutwk])

# summarize data into a table - ALL AGES
swk <- read.csv("signif_weeks_fira-acled.csv")
swk$prov[swk$prov=="National Capital Region"] <- "Metro Manila"
load("modeloutputs_allage.rda")
d1 <- rrdf[rrdf$dwar %in% dutwk & !is.na(rrdf$lci) & rrdf$lci>1, ]
tab1 <- aggregate(attr ~ type, data=d1, FUN="sum") # full numbers, without filter
tab1$filter <- "none"
d2 <- d1[paste0(d1$prov,"_",d1$dwar) %in% paste0(swk$prov,"_",swk$signif_week), ]
tab2 <- aggregate(attr ~ type, data=d2, FUN="sum")
tab2$filter <- "fira"
tab <- rbind(tab1,tab2)

# filtering methods
fira <- "Filtered Attributable Deaths"
none <- "Overall Attributable Deaths"

# create dataframe
dfall <- data.frame("cause"=rep(c("Media Reports (ACLED)",nam), each=2),
                    "type"=rep(c("none","fira"), times=length(nam)+1),
                    "total"=NA)
dfall$total <- tab$attr[match(paste0(dfall$cause,"-",dfall$type),
                              paste0(tab$type,"-",tab$filter))]
dfall$total[dfall$cause=="Media Reports (ACLED)"] <- acled
dfall$total[dfall$cause %in% c("Media Reports (ACLED)","Firearm") & dfall$type=="fira"] <- NA
dfall$cause <- factor(dfall$cause,levels=c("Media Reports (ACLED)",nam))
dfall$type2 <- ifelse(dfall$type=="fira",fira, none)
dfall$type2 <- factor(dfall$type2,levels=c(fira, none))
dfall$label <- round(dfall$total)


# PLOT
wid <- 1
dodwid <- 0.8
txtsiz <- 10
txtsiz2 <- 3

maxv1 <- round(max(dfall$total,na.rm=TRUE),-3)

ggplot(data=dfall, aes(y=cause, x=total, fill=type2)) +
  geom_col(position=position_dodge(width=dodwid),width=wid) +
  scale_fill_manual(name="",values=c("#A50F15","#076FA2")) +
  scale_y_discrete(name="",limits=rev) +
  geom_text(aes(x=label,label=formatC(round(label),digits=5,big.mark=",")),size=txtsiz2,color="black",fontface="bold",hjust=-0.01,
            position=position_dodge(width=wid)) +
  scale_x_continuous(name="",breaks=seq(0,maxv1+10000,10000),limits=c(0,maxv1+10000),expand=c(0,0)) +
  labs(title="A. Excess Deaths in All Ages",x="Excess Deaths") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(size=txtsiz,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


rm(list=ls());gc()

