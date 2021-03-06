setwd("C:/DAFNE/Svensk F�geltaxering/Databases and TRIM/F�geldata")
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

Skogsf�glar <- list("Bergfink","Bj�rktrast","Bl�mes","Bofink","Domherre","Dubbeltrast","Entita","Grans�ngare","Gr� flugsnappare","Gr�spett","Gr�ng�ling","Gr�nsiska","Gr�ns�ngare","G�rdsmyg","G�ktyta","Halsbandsflugsnappare","H�rms�ngare","J�rnsparv","J�rpe","Korp","Kungsf�gel","Lappmes","Lavskrika","L�vs�ngare","Mindre flugsnappare","Mindre hackspett","Mindre korsn�bb","Morkulla","N�ktergal","N�tkr�ka","N�tskrika","N�tv�cka","Orre","R�dhake","R�dstj�rt","R�dvingetrast","Sidensvans","Skogsduva","Skogssn�ppa","Spillkr�ka","Stenkn�ck","Stj�rtmes","St�rre hackspett","St�rre korsn�bb","Svarth�tta","Svartmes","Svartvit flugsnappare","Talgoxe","Talltita","Taltrast","Tj�der","Tofsmes","Tret�spett","Tr�dg�rdss�ngare","Tr�dkrypare","Tr�dl�rka","Tr�dpipl�rka","Videsparv")
Skogsf�glarENG <- list("Brambling", "Fieldfare","Blue tit","Chaffinch","Bullfinch","Mistle thrush","Marsh tit","Chiffchaff","Spotted flycatcher","Grey-headed woodpecker","Green woodpecker","Siskin","Wood warbler","Wren","Wryneck","Collared flycatcher","Icterine warbler","Dunnock","Hazel grouse","Raven","Goldcrest","Siberian tit","Siberian jay","Willow warbler","Red-breasted flycatcher","Lesser spotted woodpecker","Crossbill","Woodcock","Thrush nightingale","Nutcracker","Jay","Nuthatch","Black grouse","Robin","Redstart","Redwing","Waxwing","Stock dove","Green sandpiper","Black woodpecker","Hawfinch","Long-tailed tit","Great spotted woodpecker","Parrot crossbill","Blackcap","Coal tit","Pied flycatcher","Great tit","Willow tit","Song thrush","Capercaillie","Crested tit","Three-toed woodpecker","Garden warbler","Treecreeper","Woodlark","Tree pipit","Rustic bunting")

SF<-function(x){
  x<-x[x$arthela %in% Skogsf�glar,c("�r","Index","SE","arthela")]
  return(x)
}

SF2<-function(x){
  x<-x[x$arthela %in% Skogsf�glar,c("arthela","d.Year","SIGN")]
  return(x)
}

HS <- read.csv("Index STD 1998-2015 Hela Sverige Ej fj�ll.csv", sep=";", dec=",")
HS <-SF(HS)
EUROLIST<-read.csv("EUROLIST.csv", sep = ",", colClasses="character")[ ,c('art', 'arthela','englishname')] #read in the arthela names from EUROLIST
HSeng<-merge(HS, EUROLIST, by="arthela")
HSeng$englishname<-as.factor(HSeng$englishname)
HSeng$art<-NULL

DW<-c("Gr�ng�ling","Mindre hackspett","Tret�spett","Entita","Talltita")
L�S<-c("Skogsduva","Gr�ng�ling","Mindre hackspett","Tret�spett","Stj�rtmes","Entita","Tr�dkrypare")
GS<-c("Tj�der","Tret�spett","Lavskrika","Svartmes","Tofsmes","Lappmes","Talltita","Tr�dkrypare","Domherre")

###### total table ###################

sNN <- read.csv("Slopes and Significance STD 2002-2015 BD AC Z Ej fj�ll.csv", sep=",", dec=",")
sNN$NN<-paste(format(sNN$d.Year,digits=1),sNN$SIGN, sep="")
sNN <-sNN[,c(2,19)]
sNN <-sNN[sNN$arthela %in% Skogsf�glar,c("arthela","NN")]
sSN <- read.csv("Slopes and Significance STD 2002-2015 Y X W Ej fj�ll.csv", sep=",", dec=",")
sSN$SN<-paste(format(sSN$d.Year,digits=1),sSN$SIGN, sep="")
sSN <-sSN[,c(2,19)]
sSN <-sSN[sSN$arthela %in% Skogsf�glar,c("arthela","SN")]
sSG <- read.csv("Slopes and Significance STD 2002-2015 M K N.csv", sep=",", dec=",")
sSG$SG<-paste(format(sSG$d.Year,digits=1),sSG$SIGN, sep="")
sSG <-sSG[,c(2,19)]
sSG <-sSG[sSG$arthela %in% Skogsf�glar,c("arthela","SG")]
s�G <- read.csv("Slopes and Significance STD 2002-2015 E F H G I.csv", sep=",", dec=",")
s�G$�G<-paste(format(s�G$d.Year,digits=1),s�G$SIGN, sep="")
s�G <-s�G[,c(2,19)]
s�G <-s�G[s�G$arthela %in% Skogsf�glar,c("arthela","�G")]
sVG <- read.csv("Slopes and Significance STD 2002-2015 S O.csv", sep=",", dec=",")
sVG$VG<-paste(format(sVG$d.Year,digits=1),sVG$SIGN, sep="")
sVG <-sVG[,c(2,19)]
sVG <-sVG[sVG$arthela %in% Skogsf�glar,c("arthela","VG")]
s�S <- read.csv("Slopes and Significance STD 2002-2015 T U C AB D.csv", sep=",", dec=",")
s�S$�S<-paste(format(s�S$d.Year,digits=1),s�S$SIGN, sep="")
s�S <-s�S[,c(2,19)]
s�S <-s�S[s�S$arthela %in% Skogsf�glar,c("arthela","�S")]
sHS <- read.csv("Slopes and Significance STD 2002-2015 Hela Sverige Ej fj�ll.csv", sep=",", dec=",")
sHS$National98<-paste(format(sHS$d.Year,digits=1),sHS$SIGN, sep="")
sHS <-sHS[,c(2,19)]
sHS <-sHS[sHS$arthela %in% Skogsf�glar,c("arthela","National98")]
sHS2 <- read.csv("Slopes and Significance STD 1998-2015 Hela Sverige Ej fj�ll.csv", sep=",", dec=",")
sHS2$National02<-paste(format(sHS2$d.Year,digits=1),sHS2$SIGN, sep="")
sHS2 <-sHS2[,c(2,19)]
sHS2 <-sHS2[sHS2$arthela %in% Skogsf�glar,c("arthela","National02")]

sT<-Reduce(function(x, y) merge(x, y, all=TRUE), list(sNN,sSN,sSG,s�G,sVG,s�S,sHS,sHS2))
sT$arthela<-as.character(sT$arthela)
sT <- sT[order(sT$arthela),]
sT[is.na(sT)] <- ""
EUROLIST<-read.csv("EUROLIST.csv", sep = ",", colClasses="character")[ ,c('art', 'arthela','englishname')] 
sT<-merge(EUROLIST,sT, by="arthela")
sT <- sT[order(sT$art),] 
sT$arthela<-NULL
sT$art<-NULL
write.csv2(sT, file = (paste("sT",".csv", sep='')),row.names=FALSE) 


tt1 <- ttheme_default( core = list(fg_params=list(cex = 0.5, hjust=0, x=0.1)),
                       colhead = list(fg_params=list(cex = 0.5, hjust=0, x=0.1)))
                      
pdf("bird trends ej fj�ll 2002_98-2015.pdf",width = 9, height = 18)
sTG<-tableGrob(sT, theme=tt1,  rows = NULL)
grid.arrange(sTG)
dev.off()

## 2 periods table
sHS <- read.csv("Slopes and Significance STD 1998-2015 Hela Sverige Ej fj�ll.csv", sep=",", dec=",")
sHS$Nationellt<-paste(format(sHS$d.Year,digits=1),sHS$SIGN, sep="")
sHS <-sHS[,c(2,19)]
sHS <-sHS[sHS$arthela %in% Skogsf�glar,c("arthela","Nationellt")]
HS1 <- read.csv("Slopes and Significance STD 1998-2007 Hela Sverige Ej fj�ll.csv", sep=",", dec=",")
HS1$period1<-paste(format(HS1$d.Year,digits=1),HS1$SIGN, sep="")
HS1 <-HS1[,c(2,19)]
HS1 <-HS1[HS1$arthela %in% Skogsf�glar,c("arthela","period1")]
HS2 <- read.csv("Slopes and Significance STD 2008-2015 Hela Sverige Ej fj�ll.csv", sep=",", dec=",")
HS2$period2<-paste(format(HS2$d.Year,digits=1),HS2$SIGN, sep="")
HS2 <-HS2[,c(2,19)]
HS2 <-HS2[HS2$arthela %in% Skogsf�glar,c("arthela","period2")]

sT2<-Reduce(function(x, y) merge(x, y, all=TRUE), list(HS1, HS2, sHS))
sT2$arthela<-as.character(sT2$arthela)
sT2 <- sT2[order(sT2$arthela),]
sT2[is.na(sT2)] <- ""

pdf("bird trends ej fj�ll 2 periods.pdf",width = 9, height = 12)
sTG<-tableGrob(sT2, core.just = "left", gpar.coretext =gpar(fontsize=8), gpar.coltext=gpar(fontsize=9), gpar.rowtext=gpar(fontsize=8),show.rownames=FALSE,padding.v = unit(2.5, "mm"),padding.h = unit(10, "mm"),h.even.alpha = 1, h.odd.alpha = 0, v.even.alpha = 1, v.odd.alpha = 1 )
grid.arrange(sTG)
dev.off()


###### species plots (specialists) ##############################

plot1 <- ggplot(subset(HS,arthela %in% "Skogsduva"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Stock dove, ", italic("Columba oenas"))))

plot2 <- ggplot(subset(HS,arthela %in% "Stj�rtmes"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Long tailed tit, ", italic("Aegithalos caudatus"))))

plot3 <- ggplot(subset(HS,arthela %in% "Mindre hackspett"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=9), colour="#000000")+
  geom_hline(aes(yintercept=7), colour="#000000")+
  geom_hline(aes(yintercept=5), colour="#000000")+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(1, 9, 2),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Lesser spotted woodpecker, ", italic("Dendrocopos minor"))))

plot4 <- ggplot(subset(HS,arthela %in% "Gr�ng�ling"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Green woodpecker, ", italic("Picus viridis"))))

plot5 <- ggplot(subset(HS,arthela %in% "Entita"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Marsh tit, ", italic("Poecile palustris"))))

plot6 <- ggplot(subset(HS,arthela %in% "Tr�dkrypare"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Treecreeper, ", italic("Certhia familiaris"))))

plot7 <- ggplot(subset(HS,arthela %in% "Tret�spett"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=7), colour="#000000")+
  geom_hline(aes(yintercept=5), colour="#000000")+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(1, 7, 2),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Three-toed woodpecker, ", italic("Picoides tridactylus"))))

plot8 <- ggplot(subset(HS,arthela %in% "Talltita"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Willow tit, ", italic("Poecile montanus"))))

plot9 <- ggplot(subset(HS,arthela %in% "Tj�der"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Capercaillie, ", italic("Tetrao urogallus"))))

plot10 <- ggplot(subset(HS,arthela %in% "Lavskrika"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 4))+
  ggtitle(expression(paste("Siberian jay, ", italic("Perisoreus infaustus"))))

plot11 <- ggplot(subset(HS,arthela %in% "Svartmes"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 4))+
  ggtitle(expression(paste("Coal tit, ", italic("Periparus ater"))))

plot12 <- ggplot(subset(HS,arthela %in% "Tofsmes"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 4))+
  ggtitle(expression(paste("Crested tit, ", italic("Lophophanes cristatus"))))

plot13 <- ggplot(subset(HS,arthela %in% "Lappmes"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 4))+
  ggtitle(expression(paste("Siberian tit, ", italic("Poecile cinctus"))))

plot14 <- ggplot(subset(HS,arthela %in% "Domherre"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Bullfinch, ", italic("Pyrrhula pyrrhula"))))

plot15 <- ggplot(subset(HS,arthela %in% "N�tkr�ka"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Nutcracker, ", italic("Nucifraga caryocatactes"))))

plot16 <- ggplot(subset(HS,arthela %in% "J�rpe"), aes(x=�r)) + 
  geom_line(aes(y = Index), size=1) + 
  #  geom_line(aes(y = Index+SE), color="grey")+
  #  geom_line(aes(y = Index-SE), color="grey")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8)) +
  scale_y_continuous(breaks=seq(0, 2.5, 0.5),expand = c(0, 0))+
  scale_x_continuous(breaks=seq(1995, 2015, 5))+
  ggtitle(expression(paste("Hazel grouse, ", italic("Tetrastes bonasia"))))


win.metafile("species1.wmf",width = 6, height = 9)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2)
dev.off()

win.metafile("species2.wmf",width = 6, height = 9)
grid.arrange(plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, ncol=2)
dev.off()


## FIX HORIZONTAL LINES!!

b<-list()
for(i in 1:length(Skogsf�glarENG)){
  art<-Skogsf�glarENG[i]
  onespecies<-subset(HSeng,englishname %in% art)
  maxindex <- max(onespecies[,3], na.rm=T)
  ymax <- ceiling((maxindex+0.0001)*2)/2
b[[i]]<-ggplot(onespecies, aes(x=�r)) + 
    geom_line(aes(y = Index), color = "#000000", size=1) + 
    geom_hline(aes(yintercept=4), colour="#000000")+
    geom_hline(aes(yintercept=3.5), colour="#000000")+
    geom_hline(aes(yintercept=3), colour="#000000")+
    geom_hline(aes(yintercept=2.5), colour="#000000")+
    geom_hline(aes(yintercept=2), colour="#000000")+
    geom_hline(aes(yintercept=1.5), colour="#000000")+
    geom_hline(aes(yintercept=1), colour="#000000")+
    geom_hline(aes(yintercept=0.5), colour="#000000")+
    geom_hline(aes(yintercept=0), colour="#000000")+
    theme_classic(base_size = 10) +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(), plot.title=element_text(size=8),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
    scale_y_continuous(limits=c(0, ymax),expand = c(0, 0))+  
    ggtitle(paste(art))
}


grid.arrange(b[[1]],b[[2]],b[[3]],b[[4]],b[[5]],b[[6]],b[[7]],b[[8]],ncol=2)
grid.arrange(b[[9]],b[[10]],b[[11]],b[[12]],b[[13]],b[[14]],b[[15]],b[[16]],ncol=2)
grid.arrange(b[[17]],b[[18]],b[[19]],b[[20]],b[[21]],b[[22]],b[[23]],b[[24]],ncol=2)
grid.arrange(b[[25]],b[[26]],b[[27]],b[[28]],b[[29]],b[[30]],b[[31]],b[[32]],ncol=2)
grid.arrange(b[[33]],b[[34]],b[[35]],b[[36]],b[[37]],b[[38]],b[[39]],b[[40]],ncol=2)
grid.arrange(b[[41]],b[[42]],b[[43]],b[[44]],b[[45]],b[[46]],b[[47]],b[[48]],ncol=2)
grid.arrange(b[[49]],b[[50]],b[[51]],b[[52]],b[[53]],b[[54]],b[[55]],b[[56]],ncol=2)
grid.arrange(b[[57]],b[[58]],ncol=2)


###### indicators ###############################

GammalskogNN <- read.csv("GammalskogNN.csv", sep=";", dec=",")
GammalskogNN[,3:6]<-list(NULL)
GammalskogNN$Region <- "Norra Norrland"
GammalskogSG <- read.csv("GammalskogSG.csv", sep=";", dec=",")
GammalskogSG[,3:6]<-list(NULL)
GammalskogSG$Region <- "S�dra G�taland"
Gammalskog�S <- read.csv("Gammalskog�S.csv", sep=";", dec=",")
Gammalskog�S[,3:6]<-list(NULL)
Gammalskog�S$Region <- "�stra Svealand"
GammalskogVG <- read.csv("GammalskogVG.csv", sep=";", dec=",")
GammalskogVG[,3:6]<-list(NULL)
GammalskogVG$Region <- "V�stra G�ta- och Svealand"
GammalskogSN <- read.csv("GammalskogSN.csv", sep=";", dec=",")
GammalskogSN[,3:6]<-list(NULL)
GammalskogSN$Region <- "S�dra Norrland"
Gammalskog�G <- read.csv("Gammalskog�G.csv", sep=";", dec=",")
Gammalskog�G[,3:6]<-list(NULL)
Gammalskog�G$Region <- "�stra G�taland"
GammalskogHS <- read.csv("GammalskogHS.csv", sep=";", dec=",")
GammalskogHS[,3:6]<-list(NULL)
GammalskogHS$Region <- "Hela Sverige"
Gammalskog<-rbind(GammalskogNN, GammalskogSG, Gammalskog�S, GammalskogVG, GammalskogSN, Gammalskog�G,GammalskogHS)
Gammalskog<- Gammalskog[,c(1,3,2)]
Gammalskog<-dcast(Gammalskog, �r~Region)
colnames(Gammalskog)<-c("�r","var1","var2","var7","var3","var5","var6","var4")

write.csv2(Skogsf�glar, "Skogsf�glarT.csv")


Skogsf<-Skogsf�glar

Skogsf <- melt(Skogsf, id="�r")
colnames(Skogsf)<- c("�r","Region","Area")
ma <- c(
  rollapply(Skogsf[1:13,3],  width= 5, mean, fill=NA), 
  rollapply(Skogsf[14:26,3], width= 5, mean, fill=NA), 
  rollapply(Skogsf[27:39,3], width= 5, mean, fill=NA), 
  rollapply(Skogsf[40:52,3], width= 5, mean, fill=NA),
  rollapply(Skogsf[53:65,3], width= 5, mean, fill=NA),
  rollapply(Skogsf[66:78,3], width= 5, mean, fill=NA),
  rollapply(Skogsf[79:91,3], width= 5, mean, fill=NA)
)
Skogsf[,3]<- ma
Skogsf         <- dcast(Skogsf, �r~Region)


