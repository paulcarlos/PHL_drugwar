
################ FIGURE 1. PREDICTED & COUNTERFACTUAL ################
# NOTE: ONE PROVINCE PER PLOT

# SET WORKING DIRECTORY
setwd("")
library(ggplot2)

# week values
dut <- seq(ymd(20160509),ymd(20200126),1) # Duterte election win as president without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))
noy <- seq(ymd(20100630),ymd(20160509),1) # PNoy presidency, minus weeks prior to Duterte election win
noywk <- unique(paste0(isoyear(noy),"-",sprintf("%02d",isoweek(noy))))
noywk <- noywk[noywk!=dutwk[1]]
twk <- c(noywk,dutwk)

# SUBSET DATA
load("modeloutputs_allage.rda")
g1 <- "fira" # shortname options: "fira","infc","circ","others"
p1 <- "Metro Manila" # SELECT PROVINCE NAMES HERE
d1 <- dlist[[paste0(g1,"_",p1)]] 

# FIX VARIABLES
d1$pnoy <- NA; d1$pnoy[d1$yrwk %in% noywk] <- d1$fit[d1$yrwk %in% noywk]
d1$fit[d1$yrwk %in% noywk[1:305]] <- NA; d1$cfact[d1$yrwk %in% noywk[1:305]] <- NA

# PLOT
brk <- grep("-26",twk)
lab <- twk[brk]
pres <- which("2016-26"==twk)
stxt <- 12
xtxt <- 8
lwd <- 2
colr <- c("#FF1F58","#00CD6C","#009ADE","#AF58BA","#FFC61E","#F28522","#E9002D")

ggplot(data=d1, aes(x=week)) +
  geom_vline(xintercept = pres, linewidth = 1, linetype = 2, color="gray20") +
  geom_point(aes(y=out),color="gray10",alpha=0.7) +
  #geom_ribbon(aes(ymin=lower, ymax=upper), fill="pink", alpha=0.5) +
  geom_line(aes(y=fit),color=colr[7],linewidth=lwd,alpha=0.7) +
  #geom_ribbon(aes(ymin=cfact_lower, ymax=cfact_upper), fill="lightblue", alpha=0.5) +
  geom_line(aes(y=cfact),color=colr[3],linewidth=lwd,alpha=0.7) +
  geom_line(aes(y=pnoy),color=colr[2],linewidth=lwd,alpha=0.7) +
  labs(x="ISO Weeks", y="Weekly Deaths (95%CI)",title="A. Metro Manila (All Ages)") +
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(breaks = brk, labels = lab) +
  theme_bw() +
  theme(strip.text = element_text(size = stxt, face = "bold"),
        axis.text.x=element_text(size=xtxt))



################ FIGURE 2. RELATIVE RISKS ################
# NOTE: ONE PROVINCE PER PLOT

# SET WORKING DIRECTORY
setwd("")
library(ggplot2)

# SUBSET DATA
load("modeloutputs_allage.rda")
g1 <- "Firearm" # long name options: "Firearm","Infectious","Circulatory","Others"
p1 <- "Metro Manila" # CHANGE PROVINCE NAME HERE
d1 <- rrdf[rrdf$type==g1 & rrdf$prov==p1, ]

# PLOT
xbrk <- grep("-26|-01",d1$dwar)
lab <- d1$dwar[xbrk]
pres <- which("2016-26"==d1$dwar)
stxt <- 12
xtxt <- 8
lwd <- 2

d1brk <- seq(1, 6, 1); d1lab <- (d1brk-1)*100 # need to manually adjust breaks
ggplot(data=d1, aes(x=week)) +
  geom_hline(yintercept = 1, linewidth = 1, linetype = 1, color="black") +
  geom_vline(xintercept = pres, linewidth = 1, linetype = 2, color="gray20") +
  geom_ribbon(aes(ymin=lci, ymax=uci), fill="pink", alpha=0.5) +
  geom_line(aes(y=rr),color="#E9002D",linewidth=lwd,alpha=0.7) +
  geom_point(aes(y=deaths/20),color="gray10",alpha=0.7) +
  scale_y_continuous(breaks=d1brk, labels=d1lab, sec.axis=sec_axis(transform=~.*20,name="Observed Deaths",breaks=seq(0,200,20))) +
  scale_x_continuous(breaks = xbrk, labels = lab) +
  labs(x="ISO Weeks", y="Percentage Change in Risk (95%CI)",title="A. Metro Manila (All Ages)") +
  theme_bw() +
  theme(strip.text = element_text(size = stxt, face = "bold"),
        axis.text.x=element_text(size=xtxt))


rm(list=ls());gc()

