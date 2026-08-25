############# FIGURE 5 BAR PLOT #############

library(lubridate)
library(ggplot2)

# drug war weeks
dut <- seq(ymd(20160630),ymd(20200126),1) # Duterte presidency without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))

# causes of death
nam <- c("Firearms","Assault","Undetermined Intent",
         "Pneumonia","Sepsis","Other Infectious",
         "Hypertension","Myocardial Infarction","Cerebrovascular Accident",
         "Unknown Cause","Other Causes")
extl <- nam[1:3] # external causes
ntrl <- nam[4:length(nam)] # natural causes

# upload data
sdt <- read.csv("summary_kills_prov-type.csv")

# aggregate country level
ag1 <- aggregate(kill ~ type + age + cause, data=sdt, FUN="sum")
ag2 <- aggregate(lci ~ type + age + cause, data=sdt, FUN="sum")
ag1$lci <- ag2$lci[match(paste0(ag1$type,"-",ag1$age,"-",ag1$cause),
                         paste0(ag2$type,"-",ag2$age,"-",ag2$cause))]
ag2 <- aggregate(uci ~ type + age + cause, data=sdt, FUN="sum")
ag1$uci <- ag2$uci[match(paste0(ag1$type,"-",ag1$age,"-",ag1$cause),
                         paste0(ag2$type,"-",ag2$age,"-",ag2$cause))]
agall <- ag1[ag1$age=="all", ]
agu18 <- ag1[ag1$age=="u18", ]
sum(agall$kill[agall$type=="uncons"])
sum(agall$kill[agall$type=="cons"])

# get ACLED and OMT
acled0 <- read.csv("acled_weekly_2026-07-20.csv")
acled <- sum(acled0$fatalities[acled0$provname %in% sdt$prov & acled0$yrwk %in% dutwk])
omt <- 122

# create tables
consterm <- "Filtered Attributable Deaths"
uncons <- "Overall Attributable Deaths"

dfall <- data.frame("cause"=rep(c("Media Reports (ACLED)",nam), each=2), # FOR ALL AGES
                    "type"=rep(c("uncons","cons"), times=length(nam)+1),
                    "total"=NA, "upper"=NA)
dfall$total <- agall$kill[match(paste0(dfall$type,"-",dfall$cause),
                                paste0(agall$type,"-",agall$cause))]
dfall$total[dfall$cause=="Media Reports (ACLED)"] <- acled
dfall$total[dfall$cause %in% c("Media Reports (ACLED)",extl) & dfall$type=="cons"] <- NA
dfall$upper <- agall$uci[match(paste0(dfall$type,"-",dfall$cause),
                               paste0(agall$type,"-",agall$cause))]
dfall$upper[dfall$cause %in% c("Media Reports (ACLED)",extl) & dfall$type=="cons"] <- NA
dfall$cause <- factor(dfall$cause,levels=c("Media Reports (ACLED)",nam))
dfall$type2 <- ifelse(dfall$type=="cons",consterm, uncons)
dfall$type2 <- factor(dfall$type2,levels=c(consterm, uncons))
dfall$label <- round(dfall$total)

dfu18 <- data.frame("cause"=rep(c("Field Reports (OMCT)",nam), each=2), # FOR <18 years old
                    "type"=rep(c("uncons","cons"), times=length(nam)+1),
                    "total"=NA, "upper"=NA)
dfu18$total <- agu18$kill[match(paste0(dfall$type,"-",dfall$cause),
                                paste0(agu18$type,"-",agu18$cause))]
dfu18$total[dfu18$cause=="Field Reports (OMCT)"] <- omt
dfu18$total[dfu18$cause %in% c("Field Reports (OMCT)",extl) & dfu18$type=="cons"] <- NA
dfu18$total[dfu18$total==0 & !is.na(dfu18$total)] <- NA
dfu18$upper <- agu18$uci[match(paste0(dfu18$type,"-",dfu18$cause),
                               paste0(agu18$type,"-",agu18$cause))]
dfu18$upper[dfu18$cause %in% c("Media Reports (ACLED)",extl) & dfu18$type=="cons"] <- NA
dfu18$cause <- factor(dfu18$cause,levels=c("Field Reports (OMCT)",nam))
dfu18$type2 <- ifelse(dfu18$type=="cons",consterm, uncons)
dfu18$type2 <- factor(dfu18$type2,levels=c(consterm, uncons))
dfu18$label <- round(dfu18$total)


# PLOTS
wid <- 1
dodwid <- 0.8
txtsiz <- 10
txtsiz2 <- 3

maxv1 <- round(max(dfall$total,na.rm=TRUE),-3)

p1 <- ggplot(data=dfall, aes(y=cause, x=total, xmin=total, xmax=upper, fill=type2)) +
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
p1


maxv2 <- round(max(dfu18$total,na.rm=TRUE),-2)

p2 <- ggplot(data=dfu18, aes(y=cause, x=total, xmin=total, xmax=upper, fill=type2)) +
  geom_col(position=position_dodge(width=dodwid),width=wid) +
  scale_fill_manual(name="",values=c("#A50F15","#076FA2")) +
  scale_y_discrete(name="",limits=rev) +
  geom_text(aes(x=label,label=formatC(round(label),digits=5,big.mark=",")),size=txtsiz2,color="black",fontface="bold",hjust=-0.01,
            position=position_dodge(width=wid)) +
  scale_x_continuous(name="",breaks=seq(0,maxv2+500,500),limits=c(0,maxv2+200),expand=c(0,0)) +
  labs(title="B. Excess Deaths in Under 18 Years Old",x="Excess Deaths") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(size=txtsiz,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p2


# COMBINED PLOT
legplt <- cowplot::get_legend(p1 + theme(legend.position = "bottom"))

plt1 <- cowplot::plot_grid(p1,p2,nrow=1)
plt2 <- cowplot::plot_grid(plt1,legplt,nrow=2,rel_heights =c(1,0.1))
sjPlot::save_plot(paste0("fig5_barplots_bycause.png"),plt2,height=18,width=28,dpi=2000)



rm(list=ls());gc()




############# FIGURE 6 MAP #############
# NOTE THIS INCLUDES OTHER SUPPLEMENTARY TABLES

Sys.setLanguage("en")

library(terra)
library(ggplot2)
library(tidyterra)
library(classInt)
library(RColorBrewer)
library(dichromat)

# drug war weeks
dut <- seq(ymd(20160630),ymd(20200126),1) # Duterte presidency without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))

# causes of death
nam <- c("Firearms","Assault","Undetermined Intent",
         "Pneumonia","Sepsis","Other Infectious",
         "Hypertension","Myocardial Infarction","Cerebrovascular Accident",
         "Unknown Cause","Other Causes")
extl <- nam[1:3] # external causes
ntrl <- nam[4:length(nam)] # natural causes

# upload data, summarize data
sdt <- read.csv("summary_kills_prov-type.csv")
sdt2 <- aggregate(kill ~ prov, data=sdt[sdt$type =="cons" & sdt$age=="all",] , FUN="sum")
sum(sdt2$kill)
ag1 <- aggregate(lci ~ prov, data=sdt[sdt$type =="cons" & sdt$age=="all",] , FUN="sum")
sdt2$lci <- ag1$lci[match(sdt2$prov, ag1$prov)]
ag1 <- aggregate(uci ~ prov, data=sdt[sdt$type =="cons" & sdt$age=="all",] , FUN="sum")
sdt2$uci <- ag1$uci[match(sdt2$prov, ag1$prov)]

# get ACLED data
acled0 <- read.csv("acled_weekly_2026-07-20.csv")
acled <- aggregate(fatalities~provname, data=acled0[acled0$yrwk %in% dutwk & acled0$provname %in% sdt2$prov,], FUN="sum")

# combine dataframe
d2 <- acled
d2$attr <- round(sdt2$kill[match(d2$provname,sdt2$prov)])
d2[is.na(d2)] <- 0
d2$diff <- round(d2$attr - d2$fatalities)
d2$pct <- d2$diff/d2$fatalities * 100
write.csv(d2,"suppl_table_excessdeaths.csv",row.names=FALSE)


# firearms deaths only
fira <- acled
ag1 <- aggregate(kill ~ prov, data=sdt[sdt$type =="cons" & sdt$cause=="Firearms" & sdt$age=="all",] , FUN="sum")
fira$attr <- round(ag1$kill[match(fira$provname, ag1$prov)])
fira$attr[is.na(fira$attr)] <- 0
fira$diff <- round(fira$attr - fira$fatalities)
fira$pct <- fira$diff/fira$fatalities * 100
write.csv(fira,"suppl_table_excessdeaths_firearms.csv",row.names=FALSE)


# get shapefile
shp <- vect("phl_prov_v1_simplify.gpkg") # MAP FILE TOO LARGE

# get manila extent
mla <- shp[shp$ADM2_EN=="National Capital Region",]
extnt <- ext(mla)
extvec <- vect(extnt,crs=mla)
mmlin <- vect(rbind(c(extnt$xmax,extnt$ymax),c(127,extnt$ymax)),type="lines",crs=crs(shp))

# INPUT VALUES
#summary(d2$attr)
#classIntervals(d2$attr,n=4,style="pretty")$brks
#classIntervals(d2$attr,n=4,style="fisher")$brks
val1 <- c(0,1,500,1000,3000,11000)
lab1 <- c("0",paste0(val1[2]," to ",val1[3]),paste0(val1[3]," to ",val1[4]),
          paste0(val1[4]," to ",val1[5]),">3000")
d2$cat1 <- as.character(cut(d2$attr,breaks=c(val1),include.lowest=TRUE,labels=c(lab1)))
shp$attr <- factor(d2$cat1[match(shp$ADM2_EN,d2$provname)],levels=lab1)

#classIntervals(d2$diff,n=4,style="pretty")$brks
#classIntervals(d2$diff,n=4,style="fisher")$brks
val2 <- c(-50,-1,500,1000,4000,8000)
lab2 <- c(paste0(val2[1]," to ",val2[2]),paste0("1"," to ",val2[3]),
          paste0(val2[3]," to ",val2[4]),paste0(val2[4]," to ",val2[5]),paste0(val2[5]," to ",val2[6]))
d2$cat2 <- as.character(cut(d2$diff,breaks=c(val2),include.lowest=FALSE,labels=c(lab2)))
shp$diff <- factor(d2$cat2[match(shp$ADM2_EN,d2$provname)],levels=lab2)

# create lines for major provinces
cebpt <- rbind(c(123.93,10.71),c(121.91,9.52))
ceblab <- vect(matrix(c(cebpt[2,1]-0.5,cebpt[2,2]),nrow=1),type="points",crs=crs(shp))
ceblin <- vect(cebpt,type="lines",crs=crs(shp))

benpt <- rbind(c(120.68,16.48),c(119.65,17.51)) # first coordinate is middle point of province, 2nd coordinate is open space
benlab <- vect(matrix(c(benpt[2,1]-0.8,benpt[2,2]),nrow=1),type="points",crs=crs(shp))
benlin <- vect(benpt,type="lines",crs=crs(shp))

nuept <- rbind(c(121.09,15.74),c(119.30,17.50))
nuelab <- vect(matrix(c(nuept[2,1]-0.4,nuept[2,2]),nrow=1),type="points",crs=crs(shp))
nuelin <- vect(nuept,type="lines",crs=crs(shp))

ddnpt <- rbind(c(125.66,7.43),c(123.26,6.45))
ddnlab <- vect(matrix(c(ddnpt[2,1]-0.6,ddnpt[2,2]),nrow=1),type="points",crs=crs(shp))
ddnlin <- vect(ddnpt,type="lines",crs=crs(shp))

albpt <- rbind(c(123.56,13.20),c(122.80,12.86))
alblab <- vect(matrix(c(albpt[2,1]-0.6,albpt[2,2]),nrow=1),type="points",crs=crs(shp))
alblin <- vect(albpt,type="lines",crs=crs(shp))

tarpt <- rbind(c(120.48,15.49),c(122.05,15.91)) # TARLAC
tarlab <- vect(matrix(c(tarpt[2,1]-0.6,tarpt[2,2]),nrow=1),type="points",crs=crs(shp))
tarlin <- vect(tarpt,type="lines",crs=crs(shp))

cotpt <- rbind(c(124.88,7.25),c(123.51,7.17)) # COTABATO
cotlab <- vect(matrix(c(cotpt[2,1]-0.6,cotpt[2,2]),nrow=1),type="points",crs=crs(shp))
cotlin <- vect(cotpt,type="lines",crs=crs(shp))

negpt <- rbind(c(123.02,9.44),c(122.43,8.79)) # NEGROS ORIENTAL
neglab <- vect(matrix(c(negpt[2,1]-0.6,negpt[2,2]),nrow=1),type="points",crs=crs(shp))
neglin <- vect(negpt,type="lines",crs=crs(shp))

#campt <- rbind(c(123.24,13.59),c(121.88,11.08)) # CAM SUR
campt <- rbind(c(122.73,14.15),c(119.61,12.88)) # CAM NORTE
camlab <- vect(matrix(c(campt[2,1]-0.8,campt[2,2]),nrow=1),type="points",crs=crs(shp))
camlin <- vect(campt,type="lines",crs=crs(shp))

bulpt <- rbind(c(121.13,15.10),c(119.35,15.50))
bullab <- vect(matrix(c(bulpt[2,1]-0.8,bulpt[2,2]),nrow=1),type="points",crs=crs(shp))
bullin <- vect(bulpt,type="lines",crs=crs(shp))

pampt <- rbind(c(120.67,15.07),c(119.22,15.61))
pamlab <- vect(matrix(c(pampt[2,1]-0.9,pampt[2,2]),nrow=1),type="points",crs=crs(shp))
pamlin <- vect(pampt,type="lines",crs=crs(shp))


# colors
col1 <- c("#A50026","#F46D43","#FDAE61","#FEE090","#E0F3F8")

# PLOT EXCESS DEATHS
tsiz <- 2.5 # text size of province names
mmsiz <- 7 # size of metro manila text
rwid <- c(1,0.15)

p1 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=attr)) +
  geom_spatvector(data=extvec,color="blue",lwd=0.5,fill=NA) +
  scale_fill_manual(name="Excess Deaths in All Ages",values=rev(col1)) +
  geom_spatvector(data=ddnlin,color="black") +
  geom_spatvector_text(data=ddnlab,label="DAVAO\nDEL NORTE",size=tsiz) +
  geom_spatvector(data=bullin,color="black") +
  geom_spatvector_text(data=bullab,label="BULACAN",size=tsiz) +
  geom_spatvector(data=ceblin,color="black") +
  geom_spatvector_text(data=ceblab,label="CEBU",size=tsiz) +
  geom_spatvector(data=nuelin,color="black") +
  geom_spatvector_text(data=nuelab,label="NUEVA\nECIJA",size=tsiz) +
  geom_spatvector(data=mmlin,color="black") + # METRO MANILA
  coord_sf(xlim=c(116,127),
           ylim=c(4,21),expand=F) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),  
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5,"cm"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5,-0.5,0,0),"cm")) +  
  guides(fill=guide_legend(title.position="top",nrow=2,byrow=TRUE))

p2 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=attr)) +
  scale_fill_manual(name="",values=rev(col1)) +
  coord_sf(xlim=c(extnt$xmin,extnt$xmax),
           ylim=c(extnt$ymin,extnt$ymax),expand=F) +
  labs(title="METRO MANILA") +
  theme_bw() +
  theme(plot.title = element_text(size=mmsiz),
        legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0,0.5,3,-1),"cm"), #c(top,right,bottom,left)
        panel.border=element_rect(colour="blue",fill=NA,linewidth=2))

plt1 <- cowplot::plot_grid(p1,p2,nrow=1,rel_widths=rwid,greedy=FALSE) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


p3 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=diff)) +
  geom_spatvector(data=extvec,color="blue",lwd=0.5,fill=NA) +
  scale_fill_manual(name="Relative Difference with Media Reports (ACLED)",values=rev(col1)) +
  geom_spatvector(data=alblin,color="black") +
  geom_spatvector_text(data=alblab,label="ALBAY",size=tsiz) +
  geom_spatvector(data=camlin,color="black") +
  geom_spatvector_text(data=camlab,label="CAMARINES\nNORTE",size=tsiz) +
  geom_spatvector(data=benlin,color="black") +
  geom_spatvector_text(data=benlab,label="BENGUET",size=tsiz) +
  geom_spatvector(data=pamlin,color="black") +
  geom_spatvector_text(data=pamlab,label="PAMPANGA",size=tsiz) +
  geom_spatvector(data=mmlin,color="black") + # METRO MANILA
  coord_sf(xlim=c(116,127),
           ylim=c(4,21),expand=F) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),  
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5,"cm"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5,-0.5,0,0),"cm")) +  
  guides(fill=guide_legend(title.position="top",nrow=2,byrow=TRUE))

p4 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=diff)) +
  scale_fill_manual(name="",values=rev(col1)) +
  coord_sf(xlim=c(extnt$xmin,extnt$xmax),
           ylim=c(extnt$ymin,extnt$ymax),expand=F) +
  labs(title="METRO MANILA") +
  theme_bw() +
  theme(plot.title = element_text(size=mmsiz),
        legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0,0.5,3,-1),"cm"), #c(top,right,bottom,left)
        panel.border=element_rect(colour="blue",fill=NA,linewidth=2))

plt2 <- cowplot::plot_grid(p3,p4,nrow=1,rel_widths=rwid,greedy=FALSE) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

plt <- cowplot::plot_grid(plt1,plt2,greedy=FALSE)

sjPlot::save_plot(paste0("fig6_maps.png"),plt,height=16,width=22,dpi=2500)



#rm(list=ls());gc()

