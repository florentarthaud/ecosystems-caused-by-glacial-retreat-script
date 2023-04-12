library(stringr)
library(matrixStats)
library(zoo)
library(png)


chemin_averaged<-("../data_analyse/regional_data/averaged/")
chemin_figures<-("../data_analyse/Figures/")
#les 3 chemins suivants correspond normalement  volume_overdpn
chemin_daby<-("../data_analyse/regional_data/volume_overdpn/daby/")
chemin_vol_all_scenario<-("../data_analyse/regional_data/volume_overdpn/")
chemin_vol_all_scenario_2020<-("../data_analyse/regional_data/volume_overdpn/")
chemin_vol_all_scenario_rel<-("../data_analyse/regional_data/volume_overdpn/")
chemin_table_summary<-("../data_analyse/Matthias_data_analyse/Figures/")



zonefiles= list.files(chemin_averaged)                                                        
chemin_zonefiles<-paste(chemin_averaged,zonefiles,"/",sep="")
filesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx("overview*.dat"))
STDfilesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx("STD_overview*.dat"))
filestot<-c()
filesname<-c()
for (z in zonefiles){
  filesname<-c(filesname,  str_sub(list.files(paste(chemin_averaged,z,"/",sep=""),pattern=glob2rx("overview*.dat")),end =-5))
  filestot<-c(filestot,paste(chemin_averaged,z,"/", list.files(paste(chemin_averaged,z,"/",sep=""),pattern=glob2rx("overview*.dat")), sep = ""))
}

Region<-unique(str_sub(filesname,start =10,end=14))
Scenario<-unique(str_sub(filesname,start=-6))

#Si on veut avoir une seul r??gion, changer le num et enlever le di??se
#Region="RGI01"
#zonefiles="Alaska"

table_colnames<-c("Variable","Region","Mean2020_ssp119","Sd2020_ssp119","Mean2050_ssp119","Sd2050_ssp119",
                  "Mean2100_ssp119","Sd2100_ssp119"
                  ,"Mean2020_ssp585","Sd2020_ssp585","Mean2050_ssp585","Sd2050_ssp585",
                  "Mean2100_ssp585","Sd2100_ssp585",
                  "%_2100/2020_spp119", "%_2100/2020_spp585",
                  "2100_ssp585/2100_ssp119"
)  
table_summary<-data.frame(matrix(NA,nrow=0,ncol=17,dimnames = list(c(),table_colnames)),stringsAsFactors = FALSE)

for (r in Region){
  
zonefiles[which(Region==r)]

####///OVERVIEW FILES####
####Glacier N %####




# exportPDF
png(paste(chemin_figures,r,"/",r,"_N_Glaciers.png",sep=""),width=4.5,height=4, units = 'in', res = 300)


# Lecture fichier qui contient les data

  s="ssp126" 
for (s in  Scenario){  
  assign(paste("Res_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/overview_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("Std_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_overview_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
  }        



N_Glaciers<-c( "Number of glaciers",r,
              Res_ssp119[Res_ssp119$V1=="2020",]$V2, Std_ssp119[Std_ssp119$V1=="2020",]$V2,Res_ssp119[Res_ssp119$V1=="2050",]$V2, Std_ssp119[Std_ssp119$V1=="2050",]$V2,Res_ssp119[Res_ssp119$V1=="2100",]$V2, Std_ssp119[Std_ssp119$V1=="2100",]$V2 ,
              Res_ssp585[Res_ssp585$V1=="2020",]$V2, Std_ssp585[Std_ssp585$V1=="2020",]$V2,Res_ssp585[Res_ssp585$V1=="2050",]$V2, Std_ssp585[Std_ssp585$V1=="2050",]$V2,Res_ssp585[Res_ssp585$V1=="2100",]$V2, Std_ssp585[Std_ssp585$V1=="2100",]$V2 ,
              100*Res_ssp119[Res_ssp119$V1=="2100",]$V2/Res_ssp119[Res_ssp119$V1=="2020",]$V2,
              100*Res_ssp585[Res_ssp585$V1=="2100",]$V2/Res_ssp585[Res_ssp585$V1=="2020",]$V2,
              100*Res_ssp119[Res_ssp119$V1=="2100",]$V15/Res_ssp119[Res_ssp119$V1=="2100",]$V2-100*Res_ssp585[Res_ssp585$V1=="2100",]$V2/Res_ssp585[Res_ssp585$V1=="2020",]$V2)
table_summary<-rbind(table_summary,N_Glaciers,stringsAsFactors = FALSE)



# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V2+Std_ssp585[Std_ssp585$V1>"2010",]$V2))
Ymax_abs126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V2+Std_ssp126[Std_ssp126$V1>"2010",]$V2))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1a
plot(Res_ssp585$V1,Res_ssp585$V2, main="Number of glaciers", axes=FALSE, xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V2+Std_ssp126[Std_ssp126$V1>"2010",]$V2)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V2-Std_ssp126[Std_ssp126$V1>"2010",]$V2)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V2+Std_ssp585[Std_ssp585$V1>"2010",]$V2)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V2-Std_ssp585[Std_ssp585$V1>"2010",]$V2)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(Res_ssp585$V1,Res_ssp585$V2,col="firebrick4",lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V2,col="red",lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V2,col="orange",lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V2,col="darkblue",lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V2,col="dodgerblue",lwd=1.7)

mycol <- rgb(1,1,1,alpha = 0.7,names= "Twhite")
legend("bottomleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

Ymax_rel585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V2+Std_ssp585[Std_ssp585$V1>"2010",]$V2)/Res_ssp585[Res_ssp585$V1=="2020",]$V2*100)
Ymax_rel126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V2+Std_ssp126[Std_ssp126$V1>"2010",]$V2)/Res_ssp126[Res_ssp126$V1=="2020",]$V2*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V2/Res_ssp119[Res_ssp119$V1=="2020",]$V2*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0),yaxt="n")
mtext("n relative to 2020 (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Glacier Vol####

png(paste(chemin_figures,r,"/",r,"_Volume_Glaciers.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Volume_Glaciers<-c( "Volume_Glaciers",r,
               Res_ssp119[Res_ssp119$V1=="2020",]$V7, Std_ssp119[Std_ssp119$V1=="2020",]$V7,Res_ssp119[Res_ssp119$V1=="2050",]$V7, Std_ssp119[Std_ssp119$V1=="2050",]$V7,Res_ssp119[Res_ssp119$V1=="2100",]$V7, Std_ssp119[Std_ssp119$V1=="2100",]$V7 ,
               Res_ssp585[Res_ssp585$V1=="2020",]$V7, Std_ssp585[Std_ssp585$V1=="2020",]$V7,Res_ssp585[Res_ssp585$V1=="2050",]$V7, Std_ssp585[Std_ssp585$V1=="2050",]$V7,Res_ssp585[Res_ssp585$V1=="2100",]$V7, Std_ssp585[Std_ssp585$V1=="2100",]$V7 ,
               100*Res_ssp119[Res_ssp119$V1=="2100",]$V7/Res_ssp119[Res_ssp119$V1=="2020",]$V7,
               100*Res_ssp585[Res_ssp585$V1=="2100",]$V7/Res_ssp585[Res_ssp585$V1=="2020",]$V7,
               100*Res_ssp119[Res_ssp119$V1=="2100",]$V7/Res_ssp119[Res_ssp119$V1=="2020",]$V7-100*Res_ssp585[Res_ssp585$V1=="2100",]$V7/Res_ssp585[Res_ssp585$V1=="2020",]$V7)
table_summary<-rbind(table_summary,Volume_Glaciers,stringsAsFactors = FALSE)





# affichage graph double Y
par(mar=c(4,4,2,4))



Ymax_abs585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V7+Std_ssp585[Std_ssp585$V1>"2010",]$V7))
Ymax_abs126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V7+Std_ssp126[Std_ssp126$V1>"2010",]$V7))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1a
plot(Res_ssp126$V1,Res_ssp126$V7, axes=FALSE, main="Glacier volume", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("volume (km"^~"3"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V7+Std_ssp126[Std_ssp126$V1>"2010",]$V7)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V7-Std_ssp126[Std_ssp126$V1>"2010",]$V7)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V7+Std_ssp585[Std_ssp585$V1>"2010",]$V7)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V7-Std_ssp585[Std_ssp585$V1>"2010",]$V7)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(Res_ssp585$V1,Res_ssp585$V7,col="firebrick4",lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V7,col="red",lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V7,col="orange",lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V7,col="darkblue",lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V7,col="dodgerblue",lwd=1.7)

legend("bottomleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)
# box.lty=0

Ymax_rel585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V7+Std_ssp585[Std_ssp585$V1>"2010",]$V7)/Res_ssp585[Res_ssp585$V1=="2020",]$V7*100)
Ymax_rel126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V7+Std_ssp126[Std_ssp126$V1>"2010",]$V7)/Res_ssp126[Res_ssp126$V1=="2020",]$V7*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V7/Res_ssp119[Res_ssp119$V1=="2020",]$V7*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0),yaxt="n")
mtext("Relative to 2020 (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Glacier Vol WATER Equivalent####

png(paste(chemin_figures,r,"/",r,"_Volume_Glaciers_Water_Eq.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Volume_Glaciers<-c( "Volume_Glaciers",r,
                    Res_ssp119[Res_ssp119$V1=="2020",]$V7, Std_ssp119[Std_ssp119$V1=="2020",]$V7,Res_ssp119[Res_ssp119$V1=="2050",]$V7, Std_ssp119[Std_ssp119$V1=="2050",]$V7,Res_ssp119[Res_ssp119$V1=="2100",]$V7, Std_ssp119[Std_ssp119$V1=="2100",]$V7 ,
                    Res_ssp585[Res_ssp585$V1=="2020",]$V7, Std_ssp585[Std_ssp585$V1=="2020",]$V7,Res_ssp585[Res_ssp585$V1=="2050",]$V7, Std_ssp585[Std_ssp585$V1=="2050",]$V7,Res_ssp585[Res_ssp585$V1=="2100",]$V7, Std_ssp585[Std_ssp585$V1=="2100",]$V7 ,
                    100*Res_ssp119[Res_ssp119$V1=="2100",]$V7/Res_ssp119[Res_ssp119$V1=="2020",]$V7,
                    100*Res_ssp585[Res_ssp585$V1=="2100",]$V7/Res_ssp585[Res_ssp585$V1=="2020",]$V7,
                    100*Res_ssp119[Res_ssp119$V1=="2100",]$V7/Res_ssp119[Res_ssp119$V1=="2020",]$V7-100*Res_ssp585[Res_ssp585$V1=="2100",]$V7/Res_ssp585[Res_ssp585$V1=="2020",]$V7)
table_summary<-rbind(table_summary,Volume_Glaciers,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V7*0.9+Std_ssp585[Std_ssp585$V1>"2010",]$V7*0.9))
Ymax_abs126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V7*0.9+Std_ssp126[Std_ssp126$V1>"2010",]$V7*0.9))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1a
plot(Res_ssp126$V1,Res_ssp126$V7*0.9, axes=FALSE, main="Glacier volume", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("water volume (km"^~"3"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V7*0.9+Std_ssp126[Std_ssp126$V1>"2010",]$V7*0.9)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V7*0.9-Std_ssp126[Std_ssp126$V1>"2010",]$V7*0.9)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V7*0.9+Std_ssp585[Std_ssp585$V1>"2010",]$V7*0.9)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V7*0.9-Std_ssp585[Std_ssp585$V1>"2010",]$V7*0.9)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(Res_ssp585$V1,Res_ssp585$V7*0.9,col="firebrick4",lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V7*0.9,col="red",lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V7*0.9,col="orange",lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V7*0.9,col="darkblue",lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V7*0.9,col="dodgerblue",lwd=1.7)

legend("bottomleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)
# box.lty=0

Ymax_rel585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V7+Std_ssp585[Std_ssp585$V1>"2010",]$V7)/Res_ssp585[Res_ssp585$V1=="2020",]$V7*100)
Ymax_rel126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V7+Std_ssp126[Std_ssp126$V1>"2010",]$V7)/Res_ssp126[Res_ssp126$V1=="2020",]$V7*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V7/Res_ssp119[Res_ssp119$V1=="2020",]$V7*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0),yaxt="n")
mtext("Relative to 2020 (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Overdeepenings N####

png(paste(chemin_figures,r,"/",r,"_N_Overdeepenings.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

N_Overdeepenings<-c( "N_Overdeepenings",r,
                    Res_ssp119[Res_ssp119$V1=="2020",]$V15, Std_ssp119[Std_ssp119$V1=="2020",]$V15,Res_ssp119[Res_ssp119$V1=="2050",]$V15-Res_ssp119[Res_ssp119$V1=="2020",]$V15, Std_ssp119[Std_ssp119$V1=="2050",]$V15,Res_ssp119[Res_ssp119$V1=="2100",]$V15-Res_ssp119[Res_ssp119$V1=="2020",]$V15, Std_ssp119[Std_ssp119$V1=="2100",]$V15 ,
                    Res_ssp585[Res_ssp585$V1=="2020",]$V15, Std_ssp585[Std_ssp585$V1=="2020",]$V15,Res_ssp585[Res_ssp585$V1=="2050",]$V15-Res_ssp585[Res_ssp585$V1=="2020",]$V15, Std_ssp585[Std_ssp585$V1=="2050",]$V15,Res_ssp585[Res_ssp585$V1=="2100",]$V15-Res_ssp585[Res_ssp585$V1=="2020",]$V15, Std_ssp585[Std_ssp585$V1=="2100",]$V15 ,
                    100*Res_ssp119[Res_ssp119$V1=="2100",]$V15/Res_ssp119[Res_ssp119$V1=="2100",]$V2,
                    100*Res_ssp585[Res_ssp585$V1=="2100",]$V15/Res_ssp585[Res_ssp585$V1=="2100",]$V2,
                    -100*Res_ssp119[Res_ssp119$V1=="2100",]$V15/Res_ssp119[Res_ssp119$V1=="2100",]$V2+100*Res_ssp585[Res_ssp585$V1=="2100",]$V15/Res_ssp585[Res_ssp585$V1=="2100",]$V2)
table_summary<-rbind(table_summary,N_Overdeepenings,stringsAsFactors = FALSE)



# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(Res_ssp585$V15-Res_ssp585[Res_ssp585$V1=="2020",]$V15+Std_ssp585$V15))
Ymax_abs126<-(max(Res_ssp126$V15-Res_ssp126[Res_ssp126$V1=="2020",]$V15+Std_ssp126$V15))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1a
plot(Res_ssp126$V1,Res_ssp126$V15-Res_ssp126[Res_ssp126$V1=="2020",]$V15, axes=FALSE, main="Number of overdeepenings", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V15-Res_ssp126[Res_ssp126$V1=="2020",]$V15+Std_ssp126[Std_ssp126$V1>"2010",]$V15)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V15-Res_ssp126[Res_ssp126$V1=="2020",]$V15-Std_ssp126[Std_ssp126$V1>"2010",]$V15)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V15-Res_ssp585[Res_ssp585$V1=="2020",]$V15+Std_ssp585[Std_ssp585$V1>"2010",]$V15)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V15-Res_ssp585[Res_ssp585$V1=="2020",]$V15-Std_ssp585[Std_ssp585$V1>"2010",]$V15)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(Res_ssp585$V1,Res_ssp585$V15-Res_ssp585[Res_ssp585$V1=="2020",]$V15,col="firebrick4",lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V15-Res_ssp370[Res_ssp370$V1=="2020",]$V15,col="red",lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V15-Res_ssp245[Res_ssp245$V1=="2020",]$V15,col="orange",lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V15-Res_ssp126[Res_ssp126$V1=="2020",]$V15,col="darkblue",lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V15-Res_ssp119[Res_ssp119$V1=="2020",]$V15,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)
# box.lty=0

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Glacier Degla Land TO OBSL Areas####

png(paste(chemin_figures,r,"/",r,"_Area_G_D_L_L_O.png",sep=""),width=4.5,height=4, units = 'in', res = 300)


# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V4+Std_ssp585[Std_ssp585$V1>"2010",]$V4))
Ymax_abs126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V4+Std_ssp126[Std_ssp126$V1>"2010",]$V4))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V4, axes=FALSE, main="Glacier and deglaciated areas", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V4+Std_ssp126[Std_ssp126$V1>"2010",]$V4)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V4-Std_ssp126[Std_ssp126$V1>"2010",]$V4)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V4+Std_ssp585[Std_ssp585$V1>"2010",]$V4)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V4-Std_ssp585[Std_ssp585$V1>"2010",]$V4)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data glaciers
lines(Res_ssp585$V1,Res_ssp585$V4,col="firebrick4",lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V4,col="red",lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V4,col="orange",lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V4,col="darkblue",lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V4,col="dodgerblue",lwd=1.7)

# Data deglaciated areas
lines(Res_ssp585$V1,Res_ssp585$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5,col="firebrick4",lwd=1)
lines(Res_ssp370$V1,Res_ssp370$V5-Res_ssp370[Res_ssp370$V1=="2020",]$V5,col="red",lwd=1)
lines(Res_ssp245$V1,Res_ssp245$V5-Res_ssp245[Res_ssp245$V1=="2020",]$V5,col="orange",lwd=1)
lines(Res_ssp126$V1,Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5,col="darkblue",lwd=1)
lines(Res_ssp119$V1,Res_ssp119$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5,col="dodgerblue",lwd=1)

# Data land areas
lines(Res_ssp585$V1,Res_ssp585$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5-(Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13),col="firebrick4",lty=2, lwd=1)
lines(Res_ssp370$V1,Res_ssp370$V5-Res_ssp370[Res_ssp370$V1=="2020",]$V5-(Res_ssp370$V13-Res_ssp370[Res_ssp370$V1=="2020",]$V13),col="red",lty=2, lwd=1)
lines(Res_ssp245$V1,Res_ssp245$V5-Res_ssp245[Res_ssp245$V1=="2020",]$V5-(Res_ssp245$V13-Res_ssp245[Res_ssp245$V1=="2020",]$V13),col="orange",lty=2, lwd=1)
lines(Res_ssp126$V1,Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5-(Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13),col="darkblue",lty=2, lwd=1)
lines(Res_ssp119$V1,Res_ssp119$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5-(Res_ssp119$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13),col="dodgerblue",lty=2, lwd=1)

# Data terrestrial overdeepenings
lines(Res_ssp585$V1,Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13-(Res_ssp585$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6),col="firebrick4",lty=6, lwd=1)
lines(Res_ssp370$V1,Res_ssp370$V13-Res_ssp370[Res_ssp370$V1=="2020",]$V13-(Res_ssp370$V6-Res_ssp370[Res_ssp370$V1=="2020",]$V6),col="red",lty=6, lwd=1)
lines(Res_ssp245$V1,Res_ssp245$V13-Res_ssp245[Res_ssp245$V1=="2020",]$V13-(Res_ssp245$V6-Res_ssp245[Res_ssp245$V1=="2020",]$V6),col="orange",lty=6, lwd=1)
lines(Res_ssp126$V1,Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13-(Res_ssp126$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6),col="darkblue",lty=6, lwd=1)
lines(Res_ssp119$V1,Res_ssp119$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13-(Res_ssp119$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6),col="dodgerblue",lty=6, lwd=1)

# Data overdeepenings BSL
lines(Res_ssp585$V1,Res_ssp585$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6,col="firebrick4",lty=3, lwd=0.7)
lines(Res_ssp370$V1,Res_ssp370$V6-Res_ssp370[Res_ssp370$V1=="2020",]$V6,col="red",lty=3, lwd=0.7)
lines(Res_ssp245$V1,Res_ssp245$V6-Res_ssp245[Res_ssp245$V1=="2020",]$V6,col="orange",lty=3, lwd=0.7)
lines(Res_ssp126$V1,Res_ssp126$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6,col="darkblue",lty=3, lwd=0.7)
lines(Res_ssp119$V1,Res_ssp119$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6,col="dodgerblue",lty=3, lwd=0.7)

legend("left", inset=.05, legend=c("Glaciers","Deglaciated areas including :","Land","Terrestrial overdeepenings","Overdeepenings below sea level","SSP585","SSP370","SSP245","SSP126","SSP119"), col=c("black","black","black","black","black","firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=c(1,1,2,6,3,1,1,1,1,1), lwd=c(1.7,1,1,1,1,1,1,1,1,1), cex=0.5, bg=mycol)
Ymax_rel585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V4+Std_ssp585[Std_ssp585$V1>"2010",]$V4)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V4+Std_ssp126[Std_ssp126$V1>"2010",]$V4)/Res_ssp126[Res_ssp126$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to 2020 (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Glacier Area####

png(paste(chemin_figures,r,"/",r,"_Area_Glaciers.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Area_Glaciers<-c( "Area_Glaciers",r,
                     Res_ssp119[Res_ssp119$V1=="2020",]$V4, Std_ssp119[Std_ssp119$V1=="2020",]$V4,Res_ssp119[Res_ssp119$V1=="2050",]$V4, Std_ssp119[Std_ssp119$V1=="2050",]$V4,Res_ssp119[Res_ssp119$V1=="2100",]$V4, Std_ssp119[Std_ssp119$V1=="2100",]$V4 ,
                     Res_ssp585[Res_ssp585$V1=="2020",]$V4, Std_ssp585[Std_ssp585$V1=="2020",]$V4,Res_ssp585[Res_ssp585$V1=="2050",]$V4, Std_ssp585[Std_ssp585$V1=="2050",]$V4,Res_ssp585[Res_ssp585$V1=="2100",]$V4, Std_ssp585[Std_ssp585$V1=="2100",]$V4 ,
                     100*Res_ssp119[Res_ssp119$V1=="2100",]$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4,
                     100*Res_ssp585[Res_ssp585$V1=="2100",]$V4/Res_ssp585[Res_ssp585$V1=="2020",]$V4,
                  100*Res_ssp119[Res_ssp119$V1=="2100",]$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4-100*Res_ssp585[Res_ssp585$V1=="2100",]$V4/Res_ssp585[Res_ssp585$V1=="2020",]$V4)
table_summary<-rbind(table_summary,Area_Glaciers,stringsAsFactors = FALSE)


# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V4+Std_ssp585[Std_ssp585$V1>"2010",]$V4))
Ymax_abs126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V4+Std_ssp126[Std_ssp126$V1>"2010",]$V4))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V4, axes=FALSE, main="Glacier area", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V4+Std_ssp126[Std_ssp126$V1>"2010",]$V4)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V4-Std_ssp126[Std_ssp126$V1>"2010",]$V4)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V4+Std_ssp585[Std_ssp585$V1>"2010",]$V4)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V4-Std_ssp585[Std_ssp585$V1>"2010",]$V4)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data glaciers
lines(Res_ssp585$V1,Res_ssp585$V4,col="firebrick4",lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V4,col="red",lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V4,col="orange",lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V4,col="darkblue",lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V4,col="dodgerblue",lwd=1.7)

legend("bottomleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=c(1,1,1,1,1), cex=0.5, bg=mycol)

Ymax_rel585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V4+Std_ssp585[Std_ssp585$V1>"2010",]$V4)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V4+Std_ssp126[Std_ssp126$V1>"2010",]$V4)/Res_ssp126[Res_ssp126$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to 2020 (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()


###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Deglaciated Area####

png(paste(chemin_figures,r,"/",r,"_Area_Deglaciated.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Area_Deglaciated<-c( "Area_Deglaciated",r,
                  Res_ssp119[Res_ssp119$V1=="2020",]$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5, Std_ssp119[Std_ssp119$V1=="2020",]$V5,Res_ssp119[Res_ssp119$V1=="2050",]$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5, Std_ssp119[Std_ssp119$V1=="2050",]$V5,Res_ssp119[Res_ssp119$V1=="2100",]$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5, Std_ssp119[Std_ssp119$V1=="2100",]$V5 ,
                  Res_ssp585[Res_ssp585$V1=="2020",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5, Std_ssp585[Std_ssp585$V1=="2020",]$V5,Res_ssp585[Res_ssp585$V1=="2050",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5, Std_ssp585[Std_ssp585$V1=="2050",]$V5,Res_ssp585[Res_ssp585$V1=="2100",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5, Std_ssp585[Std_ssp585$V1=="2100",]$V5 ,
                  (100*Res_ssp119[Res_ssp119$V1=="2100",]$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5)/Res_ssp119[Res_ssp119$V1=="2020",]$V4,
                  (100*Res_ssp585[Res_ssp585$V1=="2100",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5)/Res_ssp585[Res_ssp585$V1=="2020",]$V4,
                  -((100*Res_ssp119[Res_ssp119$V1=="2100",]$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5)/Res_ssp119[Res_ssp119$V1=="2020",]$V4)+((100*Res_ssp585[Res_ssp585$V1=="2100",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5)/Res_ssp585[Res_ssp585$V1=="2020",]$V4))
table_summary<-rbind(table_summary,Area_Deglaciated,stringsAsFactors = FALSE)


# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5+Std_ssp585$V5))
Ymax_abs126<-(max(Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5+Std_ssp126$V5))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5, axes=FALSE, main="Deglaciated area", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5+Std_ssp126[Std_ssp126$V1>"2010",]$V5)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5-Std_ssp126[Std_ssp126$V1>"2010",]$V5)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5+Std_ssp585[Std_ssp585$V1>"2010",]$V5)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5-Std_ssp585[Std_ssp585$V1>"2010",]$V5)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new lakes
lines(Res_ssp585$V1,Res_ssp585$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5,col="firebrick4", lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V5-Res_ssp370[Res_ssp370$V1=="2020",]$V5,col="red", lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V5-Res_ssp245[Res_ssp245$V1=="2020",]$V5,col="orange", lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5,col="darkblue", lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5,col="dodgerblue", lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=c(1,1,1,1,1), cex=0.5, bg=mycol)

Ymax_rel585<-(max(Res_ssp585$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5+Std_ssp585$V5)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel126<-(max(Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5+Std_ssp126$V5)/Res_ssp126[Res_ssp126$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2020 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Deglaciated Area by year####

png(paste(chemin_figures,r,"/",r,"_Area_Deglaciated_by_year.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

#r="RGI20"
#r="RGI11"
#r="RGI05"


daby<-read.csv(paste(chemin_daby,"daby_",r,".csv",sep=""), sep=",", header=T )



# affichage graph double Y
par(mar=c(4,4,2,2))

DA_byYear119abs<-daby[,15]
DA_byYear126abs<-daby[,3]
DA_byYear245abs<-daby[,6]
DA_byYear370abs<-daby[,9]
DA_byYear585abs<-daby[,12]

DA_byYear119abs[is.na(DA_byYear119abs)] <- 0
DA_byYear126abs[is.na(DA_byYear126abs)] <- 0
DA_byYear245abs[is.na(DA_byYear245abs)] <- 0
DA_byYear370abs[is.na(DA_byYear370abs)] <- 0
DA_byYear585abs[is.na(DA_byYear585abs)] <- 0

std_DA_byYear119abs<-daby[,17]
std_DA_byYear126abs<-daby[,5]
std_DA_byYear245abs<-daby[,8]
std_DA_byYear370abs<-daby[,11]
std_DA_byYear585abs<-daby[,14]

std_DA_byYear119abs[is.na(std_DA_byYear119abs)] <- 0
std_DA_byYear126abs[is.na(std_DA_byYear126abs)] <- 0
std_DA_byYear245abs[is.na(std_DA_byYear245abs)] <- 0
std_DA_byYear370abs[is.na(std_DA_byYear370abs)] <- 0
std_DA_byYear585abs[is.na(std_DA_byYear585abs)] <- 0

Ymax<-max(DA_byYear585abs+std_DA_byYear585abs)

# Plot 1
plot(daby$Year,DA_byYear119abs, axes=FALSE, main="Deglaciated area by year", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax), yaxs="i",xaxs="i", type="n",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~"/year)"),side=2, line=2.5)

# Data new lakes
lines(daby$Year+10,DA_byYear585abs,type="l", col="firebrick4",lwd=1.7)
lines(daby$Year+10,DA_byYear370abs,type="l",col="red", lwd=1.7)
lines(daby$Year+10,DA_byYear245abs,type="l",col="orange", lwd=1.7)
lines(daby$Year+10,DA_byYear126abs,type="l",col="darkblue", lwd=1.7)
lines(daby$Year+10,DA_byYear119abs,type="l",col="dodgerblue", lwd=1.7)

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)


# Cordonn??es Y de StDev ssp126
StPos<-(DA_byYear126abs+std_DA_byYear126abs)
StNeg<-(DA_byYear126abs-std_DA_byYear126abs)
# Polygone de la StDev ssp126
polygon(x=c(daby$Year[-11]+10,rev(daby$Year[-11]+10)),y=c(StNeg[-11],rev(StPos[-11])),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(DA_byYear585abs+std_DA_byYear585abs)
StNeg<-(DA_byYear585abs-std_DA_byYear585abs)
# Polygone de la StDev ssp585
polygon(x=c(daby$Year[-11]+10,rev(daby$Year[-11]+10)),y=c(StNeg[-11],rev(StPos[-11])),col = rgb(1,0,0,alpha = 0.3),border=NA)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=c(1,1,1,1,1), cex=0.5, bg=mycol)


dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Land Area####

png(paste(chemin_figures,r,"/",r,"_Area_Land.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

land_area119<-(Res_ssp119$V5-Res_ssp119[Res_ssp119$V1=="2020",]$V5-(Res_ssp119$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13))
land_area126<-(Res_ssp126$V5-Res_ssp126[Res_ssp126$V1=="2020",]$V5-(Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13))
land_area245<-(Res_ssp245$V5-Res_ssp245[Res_ssp245$V1=="2020",]$V5-(Res_ssp245$V13-Res_ssp245[Res_ssp245$V1=="2020",]$V13))
land_area370<-(Res_ssp370$V5-Res_ssp370[Res_ssp370$V1=="2020",]$V5-(Res_ssp370$V13-Res_ssp370[Res_ssp370$V1=="2020",]$V13))
land_area585<-(Res_ssp585$V5-Res_ssp585[Res_ssp585$V1=="2020",]$V5-(Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13))

std_land_area119<-sqrt((Std_ssp119$V5*Std_ssp119$V5)+(Std_ssp119$V13*Std_ssp119$V13))
std_land_area126<-sqrt((Std_ssp126$V5*Std_ssp126$V5)+(Std_ssp126$V13*Std_ssp126$V13))
std_land_area245<-sqrt((Std_ssp245$V5*Std_ssp245$V5)+(Std_ssp245$V13*Std_ssp245$V13))
std_land_area370<-sqrt((Std_ssp370$V5*Std_ssp370$V5)+(Std_ssp370$V13*Std_ssp370$V13))
std_land_area585<-sqrt((Std_ssp585$V5*Std_ssp585$V5)+(Std_ssp585$V13*Std_ssp585$V13))
Ymax_abs<-(max(land_area585+std_land_area585))

Area_Land<-c( "Area_Land",r,
              land_area119[Res_ssp119$V1=="2020"], std_land_area119[Std_ssp119$V1=="2020"],land_area119[Res_ssp119$V1=="2050"], std_land_area119[Std_ssp119$V1=="2050"],land_area119[Res_ssp119$V1=="2100"], std_land_area119[Std_ssp119$V1=="2100"] ,
              land_area585[Res_ssp585$V1=="2020"], std_land_area585[Std_ssp585$V1=="2020"],land_area585[Res_ssp585$V1=="2050"], std_land_area585[Std_ssp585$V1=="2050"],land_area585[Res_ssp585$V1=="2100"], std_land_area585[Std_ssp585$V1=="2100"] ,
              100*land_area119[Res_ssp119$V1=="2100"]/Res_ssp119[Res_ssp119$V1=="2020",]$V4,
              100*land_area585[Res_ssp585$V1=="2100"]/Res_ssp585[Res_ssp585$V1=="2020",]$V4,
              -100*land_area119[Res_ssp119$V1=="2100"]/Res_ssp119[Res_ssp119$V1=="2020",]$V4+100*land_area585[Res_ssp585$V1=="2100"]/Res_ssp585[Res_ssp585$V1=="2020",]$V4)
table_summary<-rbind(table_summary,Area_Land,stringsAsFactors = FALSE)

# Plot 1
plot(Res_ssp126$V1,land_area126, axes=FALSE, main="Land area", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# RAJOUTER SD + COLONNE DROITE AVEC VOL RELATIVE TAILLE INITIALE GLACIER ?

# Cordonn??es Y de StDev ssp126
StPos<-(land_area126+std_land_area126)
StNeg<-(land_area126-std_land_area126)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126$V1,rev(Res_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(land_area585+std_land_area585)
StNeg<-(land_area585-std_land_area585)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585$V1,rev(Res_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new land
lines(Res_ssp126$V1,land_area585,col="firebrick4", lwd=1.7)
lines(Res_ssp126$V1,land_area370,col="red", lwd=1.7)
lines(Res_ssp126$V1,land_area245,col="orange", lwd=1.7)
lines(Res_ssp126$V1,land_area126,col="darkblue", lwd=1.7)
lines(Res_ssp126$V1,land_area119,col="dodgerblue", lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=c(1,1,1,1,1), cex=0.5, bg=mycol)

Ymax_rel585<-(max(land_area585+Std_ssp585$V5)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel126<-(max(land_area126+Std_ssp126$V5)/Res_ssp126[Res_ssp126$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2020 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####OBSL area####

png(paste(chemin_figures,r,"/",r,"_Area_OBSL.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Area_OBSL<-c( "Area_OBSL",r,
              Res_ssp119[Res_ssp119$V1=="2020",]$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6, Std_ssp119[Std_ssp119$V1=="2020",]$V6,Res_ssp119[Res_ssp119$V1=="2050",]$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6, Std_ssp119[Std_ssp119$V1=="2050",]$V6,Res_ssp119[Res_ssp119$V1=="2100",]$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6, Std_ssp119[Std_ssp119$V1=="2100",]$V6 ,
              Res_ssp585[Res_ssp585$V1=="2020",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6, Std_ssp585[Std_ssp585$V1=="2020",]$V6,Res_ssp585[Res_ssp585$V1=="2050",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6, Std_ssp585[Std_ssp585$V1=="2050",]$V6,Res_ssp585[Res_ssp585$V1=="2100",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6, Std_ssp585[Std_ssp585$V1=="2100",]$V6 ,
              (100*Res_ssp119[Res_ssp119$V1=="2100",]$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6)/Res_ssp119[Res_ssp119$V1=="2020",]$V4,
              (100*Res_ssp585[Res_ssp585$V1=="2100",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6)/Res_ssp585[Res_ssp585$V1=="2020",]$V4,
              -((100*Res_ssp119[Res_ssp119$V1=="2100",]$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6)/Res_ssp119[Res_ssp119$V1=="2020",]$V4)+((100*Res_ssp585[Res_ssp585$V1=="2100",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6)/Res_ssp585[Res_ssp585$V1=="2020",]$V4))
table_summary<-rbind(table_summary,Area_OBSL,stringsAsFactors = FALSE)


# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6+Std_ssp585$V6))
Ymax_abs126<-(max(Res_ssp126$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6+Std_ssp126$V6))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V4, axes=FALSE, main="Overdeepening below sea level area", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs585), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6+Std_ssp126[Std_ssp126$V1>"2010",]$V6)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6-Std_ssp126[Std_ssp126$V1>"2010",]$V6)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6+Std_ssp585[Std_ssp585$V1>"2010",]$V6)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6-Std_ssp585[Std_ssp585$V1>"2010",]$V6)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new ocean
lines(Res_ssp585$V1,Res_ssp585$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6,col="firebrick4", lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V6-Res_ssp370[Res_ssp370$V1=="2020",]$V6,col="red", lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V6-Res_ssp245[Res_ssp245$V1=="2020",]$V6,col="orange", lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6,col="darkblue", lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6,col="dodgerblue", lwd=1.7)

legend("bottomright", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=1, cex=0.55, bg=mycol)

Ymax_rel585<-(max(Res_ssp585$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6+Std_ssp585$V6)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel126<-(max(Res_ssp126$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6+Std_ssp126$V6)/Res_ssp126[Res_ssp126$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2020 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####All overdeepening area####

png(paste(chemin_figures,r,"/",r,"_Area_all_overdeepening.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Area_all_overdeepening<-c( "Area_all_overdeepening",r,
              Res_ssp119[Res_ssp119$V1=="2020",]$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13, Std_ssp119[Std_ssp119$V1=="2020",]$V13,Res_ssp119[Res_ssp119$V1=="2050",]$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13, Std_ssp119[Std_ssp119$V1=="2050",]$V13,Res_ssp119[Res_ssp119$V1=="2100",]$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13, Std_ssp119[Std_ssp119$V1=="2100",]$V13 ,
              Res_ssp585[Res_ssp585$V1=="2020",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13, Std_ssp585[Std_ssp585$V1=="2020",]$V13,Res_ssp585[Res_ssp585$V1=="2050",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13, Std_ssp585[Std_ssp585$V1=="2050",]$V13,Res_ssp585[Res_ssp585$V1=="2100",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13, Std_ssp585[Std_ssp585$V1=="2100",]$V13 ,
              (100*Res_ssp119[Res_ssp119$V1=="2100",]$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13)/Res_ssp119[Res_ssp119$V1=="2020",]$V4,
              (100*Res_ssp585[Res_ssp585$V1=="2100",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13)/Res_ssp585[Res_ssp585$V1=="2020",]$V4,
              -((100*Res_ssp119[Res_ssp119$V1=="2100",]$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13)/Res_ssp119[Res_ssp119$V1=="2020",]$V4)+((100*Res_ssp585[Res_ssp585$V1=="2100",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13)/Res_ssp585[Res_ssp585$V1=="2020",]$V4))
table_summary<-rbind(table_summary,Area_all_overdeepening,stringsAsFactors = FALSE)



# affichage graph double Y
par(mar=c(4,4,2,4))

Ymax_abs585<-(max(Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13+Std_ssp585$V13))
Ymax_abs126<-(max(Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13+Std_ssp126$V13))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V4, axes=FALSE, main="Overdeepening (all) area", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13+Std_ssp126[Std_ssp126$V1>"2010",]$V13)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13-Std_ssp126[Std_ssp126$V1>"2010",]$V13)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13+Std_ssp585[Std_ssp585$V1>"2010",]$V13)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13-Std_ssp585[Std_ssp585$V1>"2010",]$V13)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new lakes
lines(Res_ssp585$V1,Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13,col="firebrick4",lty=6, lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V13-Res_ssp370[Res_ssp370$V1=="2020",]$V13,col="red",lty=6, lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V13-Res_ssp245[Res_ssp245$V1=="2020",]$V13,col="orange",lty=6, lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13,col="darkblue",lty=6, lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13,col="dodgerblue",lty=6, lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=1, cex=0.55, bg=mycol)

Ymax_rel585<-(max(Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13+Std_ssp585$V13)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel126<-(max(Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13+Std_ssp126$V13)/Res_ssp126[Res_ssp126$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2020 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Terrestrial overdeepening area####

png(paste(chemin_figures,r,"/",r,"_Area_terrestrial_overdeepening.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

lake_area119<-(Res_ssp119$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13-(Res_ssp119$V6-Res_ssp119[Res_ssp119$V1=="2020",]$V6))
lake_area126<-(Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13-(Res_ssp126$V6-Res_ssp126[Res_ssp126$V1=="2020",]$V6))
lake_area245<-(Res_ssp245$V13-Res_ssp245[Res_ssp245$V1=="2020",]$V13-(Res_ssp245$V6-Res_ssp245[Res_ssp245$V1=="2020",]$V6))
lake_area370<-(Res_ssp370$V13-Res_ssp370[Res_ssp370$V1=="2020",]$V13-(Res_ssp370$V6-Res_ssp370[Res_ssp370$V1=="2020",]$V6))
lake_area585<-(Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13-(Res_ssp585$V6-Res_ssp585[Res_ssp585$V1=="2020",]$V6))

std_lake_area119<-(Std_ssp119$V13+(Std_ssp119$V6))
std_lake_area126<-(Std_ssp126$V13+(Std_ssp126$V6))
std_lake_area245<-(Std_ssp245$V13+(Std_ssp245$V6))
std_lake_area370<-(Std_ssp370$V13+(Std_ssp370$V6))
std_lake_area585<-(Std_ssp585$V13+(Std_ssp585$V6))

Area_terrestrial_overdeepening<-c( "Area_terrestrial_overdeepening",r,
                                   lake_area119[Res_ssp119$V1=="2020"], std_lake_area119[Std_ssp119$V1=="2020"],lake_area119[Res_ssp119$V1=="2050"], std_lake_area119[Std_ssp119$V1=="2050"],lake_area119[Res_ssp119$V1=="2100"], std_lake_area119[Std_ssp119$V1=="2100"] ,
                                   lake_area585[Res_ssp585$V1=="2020"], std_lake_area585[Std_ssp585$V1=="2020"],lake_area585[Res_ssp585$V1=="2050"], std_lake_area585[Std_ssp585$V1=="2050"],lake_area585[Res_ssp585$V1=="2100"], std_lake_area585[Std_ssp585$V1=="2100"] ,
                                   100*lake_area119[Res_ssp119$V1=="2100"]/Res_ssp119[Res_ssp119$V1=="2020",]$V4,
                                   100*lake_area585[Res_ssp585$V1=="2100"]/Res_ssp585[Res_ssp585$V1=="2020",]$V4,
                                   -100*lake_area119[Res_ssp119$V1=="2100"]/Res_ssp119[Res_ssp119$V1=="2020",]$V4+100*lake_area585[Res_ssp585$V1=="2100"]/Res_ssp585[Res_ssp585$V1=="2020",]$V4)
table_summary<-rbind(table_summary,Area_terrestrial_overdeepening,stringsAsFactors = FALSE)


if (sum(lake_area119,na.rm=T)>0){
std_lake_area119<-sqrt((Std_ssp119$V6*Std_ssp119$V6)+(Std_ssp119$V13*Std_ssp119$V13))
}

if (sum(lake_area119,na.rm=T)==0){
  std_lake_area119<-rep(0,length(lake_area119))
}

if (sum(lake_area126,na.rm=T)>0){
  std_lake_area126<-sqrt((Std_ssp126$V6*Std_ssp126$V6)+(Std_ssp126$V13*Std_ssp126$V13))
}

if (sum(lake_area126,na.rm=T)==0){
  std_lake_area126<-rep(0,length(lake_area126))
}

if (sum(lake_area245,na.rm=T)>0){
  std_lake_area245<-sqrt((Std_ssp245$V6*Std_ssp245$V6)+(Std_ssp245$V13*Std_ssp245$V13))
}

if (sum(lake_area245,na.rm=T)==0){
  std_lake_area245<-rep(0,length(lake_area245))
}

if (sum(lake_area370,na.rm=T)>0){
  std_lake_area370<-sqrt((Std_ssp370$V6*Std_ssp370$V6)+(Std_ssp370$V13*Std_ssp370$V13))
}

if (sum(lake_area370,na.rm=T)==0){
  std_lake_area370<-rep(0,length(lake_area370))
}
if (sum(lake_area585,na.rm=T)>0){
  std_lake_area585<-sqrt((Std_ssp585$V6*Std_ssp585$V6)+(Std_ssp585$V13*Std_ssp585$V13))
}

if (sum(lake_area585,na.rm=T)==0){
  std_lake_area585<-rep(0,length(lake_area585))
}



Ymax_abs<-(max(lake_area585+std_lake_area585))

# Plot 1
plot(Res_ssp126$V1,lake_area126, axes=FALSE, main="Terrestrial overdeepening area", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(lake_area126+std_lake_area126)
StNeg<-(lake_area126-std_lake_area126)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126$V1,rev(Res_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(lake_area585+std_lake_area585)
StNeg<-(lake_area585-std_lake_area585)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585$V1,rev(Res_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new land
lines(Res_ssp585$V1,lake_area585,col="firebrick4", lwd=1.7)
lines(Res_ssp370$V1,lake_area370,col="red", lwd=1.7)
lines(Res_ssp245$V1,lake_area245,col="orange", lwd=1.7)
lines(Res_ssp126$V1,lake_area126,col="darkblue", lwd=1.7)
lines(Res_ssp119$V1,lake_area119,col="dodgerblue", lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=c(1,1,1,1,1), cex=0.5, bg=mycol)

Ymax_rel585<-(max(lake_area585+std_lake_area585)/Res_ssp585[Res_ssp585$V1=="2020",]$V4*100)
Ymax_rel<-(max(Ymax_rel126,Ymax_rel585))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(Res_ssp119$V1,Res_ssp119$V4/Res_ssp119[Res_ssp119$V1=="2020",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2020 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.5)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Overdeepenings depth####

png(paste(chemin_figures,r,"/",r,"_Depth_overdeepening_mean.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Depth_overdeepening_mean<-c( "Depth_overdeepening_mean",r,
                     Res_ssp119[Res_ssp119$V1=="2020",]$V14, Std_ssp119[Std_ssp119$V1=="2020",]$V14,Res_ssp119[Res_ssp119$V1=="2050",]$V14, Std_ssp119[Std_ssp119$V1=="2050",]$V14,Res_ssp119[Res_ssp119$V1=="2100",]$V14, Std_ssp119[Std_ssp119$V1=="2100",]$V14 ,
                     Res_ssp585[Res_ssp585$V1=="2020",]$V14, Std_ssp585[Std_ssp585$V1=="2020",]$V14,Res_ssp585[Res_ssp585$V1=="2050",]$V14, Std_ssp585[Std_ssp585$V1=="2050",]$V14,Res_ssp585[Res_ssp585$V1=="2100",]$V14, Std_ssp585[Std_ssp585$V1=="2100",]$V14 ,
                    NA,
                     NA,
                     Res_ssp119[Res_ssp119$V1=="2100",]$V14-Res_ssp585[Res_ssp585$V1=="2100",]$V14)
table_summary<-rbind(table_summary,Depth_overdeepening_mean,stringsAsFactors = FALSE)



# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(Res_ssp585[Res_ssp585$V1>"2010",]$V14+Std_ssp585[Std_ssp585$V1>"2010",]$V14))
Ymax_abs126<-(max(Res_ssp126[Res_ssp126$V1>"2010",]$V14+Std_ssp126[Std_ssp126$V1>"2010",]$V14))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V14, axes=FALSE, main="Mean overdeepening depth", xlab="", ylab="", lty=6, lwd=1.7, xlim=c(2020,2100),ylim=c(Ymax_abs,0), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("(m)",side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Res_ssp126[Res_ssp126$V1>"2010",]$V14+Std_ssp126[Std_ssp126$V1>"2010",]$V14)
StNeg<-(Res_ssp126[Res_ssp126$V1>"2010",]$V14-Std_ssp126[Std_ssp126$V1>"2010",]$V14)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126[Res_ssp126$V1>"2010",]$V1,rev(Res_ssp126[Res_ssp126$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Res_ssp585[Res_ssp585$V1>"2010",]$V14+Std_ssp585[Std_ssp585$V1>"2010",]$V14)
StNeg<-(Res_ssp585[Res_ssp585$V1>"2010",]$V14-Std_ssp585[Std_ssp585$V1>"2010",]$V14)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585[Res_ssp585$V1>"2010",]$V1,rev(Res_ssp585[Res_ssp585$V1>"2010",]$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new lakes
lines(Res_ssp585$V1,Res_ssp585$V14,col="firebrick4",lty=6, lwd=1.7)
lines(Res_ssp370$V1,Res_ssp370$V14,col="red",lty=6, lwd=1.7)
lines(Res_ssp245$V1,Res_ssp245$V14,col="orange",lty=6, lwd=1.7)
lines(Res_ssp126$V1,Res_ssp126$V14,col="darkblue",lty=6, lwd=1.7)
lines(Res_ssp119$V1,Res_ssp119$V14,col="dodgerblue",lty=6, lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Overdeepenings Vol#### 

png(paste(chemin_figures,r,"/",r,"_Volume_Overdeepening.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

#Lecture fichier qui contient les data
vol_all_scenario <-read.csv(paste(chemin_vol_all_scenario,"volume_overdpn",r,".csv",sep=""), sep=",", header=F, skip = 1)

Volume_Overdeepening<-c( "volume_overdeepening",r,
                         vol_all_scenario[Res_ssp119$V1=="2020",]$V16-vol_all_scenario[Res_ssp119$V1=="2020",]$V16, vol_all_scenario[Std_ssp119$V1=="2020",]$V17,-vol_all_scenario[Res_ssp119$V1=="2050",]$V16-vol_all_scenario[Res_ssp119$V1=="2020",]$V16, vol_all_scenario[Std_ssp119$V1=="2050",]$V17,-vol_all_scenario[Res_ssp119$V1=="2100",]$V16-vol_all_scenario[Res_ssp119$V1=="2020",]$V16, vol_all_scenario[Std_ssp119$V1=="2100",]$V17 ,
                         vol_all_scenario[Res_ssp585$V1=="2020",]$V13-vol_all_scenario[Res_ssp585$V1=="2020",]$V13, vol_all_scenario[Std_ssp585$V1=="2020",]$V14,-vol_all_scenario[Res_ssp585$V1=="2050",]$V13-vol_all_scenario[Res_ssp585$V1=="2020",]$V13, vol_all_scenario[Std_ssp585$V1=="2050",]$V14,-vol_all_scenario[Res_ssp585$V1=="2100",]$V13-vol_all_scenario[Res_ssp585$V1=="2020",]$V13, vol_all_scenario[Std_ssp585$V1=="2100",]$V14 ,
                           NA,
                         NA,
                         -vol_all_scenario[Res_ssp119$V1=="2100",]$V16+vol_all_scenario[Res_ssp585$V1=="2100",]$V13)
table_summary<-rbind(table_summary,Volume_Overdeepening,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs<-(max(vol_all_scenario$V13*(-1)+vol_all_scenario$V14))

# Plot 1
plot(Res_ssp126$V1,Res_ssp126$V4, axes=FALSE, main="Overdeepening storage volume", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext(expression("km"^~"3"),side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos126<-(vol_all_scenario$V4*(-1)+vol_all_scenario$V5)
StNeg126<-(vol_all_scenario$V4*(-1)-vol_all_scenario$V5)
# Polygone de la StDev ssp126
polygon(x=c(vol_all_scenario$V2,rev(vol_all_scenario$V2)),y=c(StNeg126,rev(StPos126)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos585<-(vol_all_scenario$V13*(-1)+vol_all_scenario$V14)
StNeg585<-(vol_all_scenario$V13*(-1)-vol_all_scenario$V14)
# Polygone de la StDev ssp585
polygon(x=c(vol_all_scenario$V2,rev(vol_all_scenario$V2)),y=c(StNeg585,rev(StPos585)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data new lakes
lines(vol_all_scenario$V2,vol_all_scenario$V13*(-1),col="firebrick4",lwd=1.7)
lines(vol_all_scenario$V2,vol_all_scenario$V10*(-1),col="red",lwd=1.7)
lines(vol_all_scenario$V2,vol_all_scenario$V7*(-1),col="orange",lwd=1.7)
lines(vol_all_scenario$V2,vol_all_scenario$V4*(-1),col="darkblue",lwd=1.7)
lines(vol_all_scenario$V2,vol_all_scenario$V16*(-1),col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()



##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Overdeepenging vs.Glacier Vol####

png(paste(chemin_figures,r,"/",r,"_Volume_Overdeepening_vs_glacier.png",sep=""),width=4.5,height=4, units = 'in', res = 300)


Vol_rel_585<-vol_all_scenario_rel$V9
Vol_SDrel_585<-vol_all_scenario_rel$V10
Vol_rel_126<-vol_all_scenario_rel$V3
Vol_SDrel_126<-vol_all_scenario_rel$V4

vol_all_scenario_rel <-read.csv(paste(chemin_vol_all_scenario_rel,"volume_overdpn_rel",r,".csv",sep=""), sep=",", header=F, skip = 1)
vol_all_scenario_2020 <-read.csv(paste(chemin_vol_all_scenario_2020,"volume_overdpn_2020",r,".csv",sep=""), sep=",", header=F, skip = 1)

#for (r in Region){
#vol_all_scenario <-read.csv(paste(chemin_vol_all_scenario,"volume",r,".csv",sep=""), sep=",", header=F, skip = 1)
#write.csv(vol_all_scenario,paste(chemin_vol_all_scenario_rel,"Volume_2020",r,".csv",sep=""))
#write.csv(vol_all_scenario,paste(chemin_vol_all_scenario_rel,"Volume_REL",r,".csv",sep=""))
#write.csv(vol_all_scenario,paste(chemin_vol_all_scenario_rel,"Volume_",r,".csv",sep=""))
#}

# affichage graph double Y
par(mar=c(4,4,2,2))

Vol_rel_585<-vol_all_scenario_rel$V9
Vol_SDrel_585<-vol_all_scenario_rel$V10
Vol_rel_126<-vol_all_scenario_rel$V3
Vol_SDrel_126<-vol_all_scenario_rel$V4
Ymax_abs<-(max(Vol_rel_585+Vol_SDrel_585,Vol_rel_126+Vol_SDrel_126))

# Plot 1
plot(Res_ssp126$V1,Vol_rel_585, axes=FALSE, main="Overdeepening vs. glacier volume", xlab="", ylab="", lwd=1.7, xlim=c(2020,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("volume (%)",side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
StPos<-(Vol_rel_126+Vol_SDrel_126)
StNeg<-(Vol_rel_126-Vol_SDrel_126)
# Polygone de la StDev ssp126
polygon(x=c(Res_ssp126$V1,rev(Res_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(Vol_rel_585+Vol_SDrel_585)
StNeg<-(Vol_rel_585-Vol_SDrel_585)
# Polygone de la StDev ssp585
polygon(x=c(Res_ssp585$V1,rev(Res_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data vol relative to 2020
lines(Res_ssp585$V1,vol_all_scenario_2020$V9, col="firebrick4",lty=2, lwd=1.2)
lines(Res_ssp370$V1,vol_all_scenario_2020$V7,col="red",lty=2, lwd=1.2)
lines(Res_ssp245$V1,vol_all_scenario_2020$V5,col="orange",lty=2, lwd=1.2)
lines(Res_ssp126$V1,vol_all_scenario_2020$V3,col="darkblue",lty=2, lwd=1.2)
lines(Res_ssp119$V1,vol_all_scenario_2020$V11,col="dodgerblue",lty=2, lwd=1.2)

# Data vol relative
lines(Res_ssp585$V1,vol_all_scenario_rel$V9, col="firebrick4",lty=1, lwd=1.7)
lines(Res_ssp370$V1,vol_all_scenario_rel$V7,col="red",lty=1, lwd=1.7)
lines(Res_ssp245$V1,vol_all_scenario_rel$V5,col="orange",lty=1, lwd=1.7)
lines(Res_ssp126$V1,vol_all_scenario_rel$V3,col="darkblue",lty=1, lwd=1.7)
lines(Res_ssp119$V1,vol_all_scenario_rel$V11,col="dodgerblue",lty=1, lwd=1.7)

legend("topleft", inset=.05, legend=c("Overdeepening vs. glacier volume over time","Overdeepening vs. 2020 glacier volume","SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("black", "black", "firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=c(1,6,1,1,1,1,1), cex=0.55, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()





                                           # SLOPE


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####///SLOPE FILES####
####Land area Slope####

png(paste(chemin_figures,r,"/",r,"_Slope_land_area.png",sep=""),width=4.5,height=4, units = 'in', res = 300)


# affichage graph double Y
par(mar=c(4,4,2,2))

# Lecture fichier qui contient les data

for (s in  Scenario){  
  assign(paste("SLO_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/slope_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_slo_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_slope_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 

Ymax_abs585<-(max(SLO_ssp585$V12-SLO_ssp585$V4+STD_slo_ssp585$V12))
Ymax_abs126<-(max(SLO_ssp126$V12-SLO_ssp126$V4+STD_slo_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(SLO_ssp126$V1,SLO_ssp126$V12-SLO_ssp126$V4, axes=FALSE, main="Slope in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(0,80),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("slope (degree)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(30,30),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(SLO_ssp126$V12-SLO_ssp126$V4+STD_slo_ssp126$V12)
StNeg<-(SLO_ssp126$V12-SLO_ssp126$V4-STD_slo_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(SLO_ssp126$V1,rev(SLO_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(SLO_ssp585$V12-SLO_ssp585$V4+STD_slo_ssp585$V12)
StNeg<-(SLO_ssp585$V12-SLO_ssp585$V4-STD_slo_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(SLO_ssp585$V1,rev(SLO_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data slope 2050
lines(SLO_ssp585$V1,SLO_ssp585$V7-SLO_ssp585$V4,col="firebrick4",lty=2, lwd=1)
lines(SLO_ssp370$V1,SLO_ssp370$V7-SLO_ssp370$V4,col="red",lty=2, lwd=1)
lines(SLO_ssp245$V1,SLO_ssp245$V7-SLO_ssp245$V4,col="orange",lty=2, lwd=1)
lines(SLO_ssp126$V1,SLO_ssp126$V7-SLO_ssp126$V4,col="darkblue",lty=2, lwd=1)
lines(SLO_ssp119$V1,SLO_ssp119$V7-SLO_ssp119$V4,col="dodgerblue",lty=2, lwd=1)

# Data slope 2100
lines(SLO_ssp585$V1,SLO_ssp585$V12-SLO_ssp585$V4,col="firebrick4",lwd=1.7)
lines(SLO_ssp370$V1,SLO_ssp370$V12-SLO_ssp370$V4,col="red",lwd=1.7)
lines(SLO_ssp245$V1,SLO_ssp245$V12-SLO_ssp245$V4,col="orange",lwd=1.7)
lines(SLO_ssp126$V1,SLO_ssp126$V12-SLO_ssp126$V4,col="darkblue",lwd=1.7)
lines(SLO_ssp119$V1,SLO_ssp119$V12-SLO_ssp119$V4,col="dodgerblue",lwd=1.7)

legend("topright", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(80,80),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####All DA area Slope####

png(paste(chemin_figures,r,"/",r,"_Slope_deglaciated_area.png",sep=""),width=4.5,height=4, units = 'in', res = 300)



# creation fichiers
# 585
SLO_ssp585_2100 <- (SLO_ssp585$V12-SLO_ssp585$V4)
SLO_ssp585_2050 <- (SLO_ssp585$V7-SLO_ssp585$V4)
Area_DA_ssp585 <- (Res_ssp585$V13-Res_ssp585[Res_ssp585$V1=="2020",]$V13)
FlatArea2100 <- (SLO_ssp585_2100[1:1]+Area_DA_ssp585[11:11])
FlatArea2050 <- (SLO_ssp585_2050[1:1]+Area_DA_ssp585[6:6])
SteepArea2100 <- (SLO_ssp585_2100[2:80])
SteepArea2050 <- (SLO_ssp585_2050[2:80])
all_SLO_ssp585_2100 <- c(FlatArea2100, SteepArea2100)
all_SLO_ssp585_2050 <- c(FlatArea2050, SteepArea2050)

# 370
SLO_ssp370_2100 <- (SLO_ssp370$V12-SLO_ssp370$V4)
SLO_ssp370_2050 <- (SLO_ssp370$V7-SLO_ssp370$V4)
Area_DA_ssp370 <- (Res_ssp370$V13-Res_ssp370[Res_ssp370$V1=="2020",]$V13)
FlatArea2100 <- (SLO_ssp370_2100[1:1]+Area_DA_ssp370[11:11])
FlatArea2050 <- (SLO_ssp370_2050[1:1]+Area_DA_ssp370[6:6])
SteepArea2100 <- (SLO_ssp370_2100[2:80])
SteepArea2050 <- (SLO_ssp370_2050[2:80])
all_SLO_ssp370_2100 <- c(FlatArea2100, SteepArea2100)
all_SLO_ssp370_2050 <- c(FlatArea2050, SteepArea2050)

# 245
SLO_ssp245_2100 <- (SLO_ssp245$V12-SLO_ssp245$V4)
SLO_ssp245_2050 <- (SLO_ssp245$V7-SLO_ssp245$V4)
Area_DA_ssp245 <- (Res_ssp245$V13-Res_ssp245[Res_ssp245$V1=="2020",]$V13)
FlatArea2100 <- (SLO_ssp245_2100[1:1]+Area_DA_ssp245[11:11])
FlatArea2050 <- (SLO_ssp245_2050[1:1]+Area_DA_ssp245[6:6])
SteepArea2100 <- (SLO_ssp245_2100[2:80])
SteepArea2050 <- (SLO_ssp245_2050[2:80])
all_SLO_ssp245_2100 <- c(FlatArea2100, SteepArea2100)
all_SLO_ssp245_2050 <- c(FlatArea2050, SteepArea2050)

# 126
SLO_ssp126_2100 <- (SLO_ssp126$V12-SLO_ssp126$V4)
SLO_ssp126_2050 <- (SLO_ssp126$V7-SLO_ssp126$V4)
Area_DA_ssp126 <- (Res_ssp126$V13-Res_ssp126[Res_ssp126$V1=="2020",]$V13)
FlatArea2100 <- (SLO_ssp126_2100[1:1]+Area_DA_ssp126[11:11])
FlatArea2050 <- (SLO_ssp126_2050[1:1]+Area_DA_ssp126[6:6])
SteepArea2100 <- (SLO_ssp126_2100[2:80])
SteepArea2050 <- (SLO_ssp126_2050[2:80])
all_SLO_ssp126_2100 <- c(FlatArea2100, SteepArea2100)
all_SLO_ssp126_2050 <- c(FlatArea2050, SteepArea2050)

# 119
SLO_ssp119_2100 <- (SLO_ssp119$V12-SLO_ssp119$V4)
SLO_ssp119_2050 <- (SLO_ssp119$V7-SLO_ssp119$V4)
Area_DA_ssp119 <- (Res_ssp119$V13-Res_ssp119[Res_ssp119$V1=="2020",]$V13)
FlatArea2100 <- (SLO_ssp119_2100[1:1]+Area_DA_ssp119[11:11])
FlatArea2050 <- (SLO_ssp119_2050[1:1]+Area_DA_ssp119[6:6])
SteepArea2100 <- (SLO_ssp119_2100[2:80])
SteepArea2050 <- (SLO_ssp119_2050[2:80])
all_SLO_ssp119_2100 <- c(FlatArea2100, SteepArea2100)
all_SLO_ssp119_2050 <- c(FlatArea2050, SteepArea2050)

# STD585
STD_slo_ssp585_2100 <- (STD_slo_ssp585$V12)
STD_slo_ssp585_2050 <- (STD_slo_ssp585$V7)
STD_Area_DA_ssp585 <- (Std_ssp585$V13-Std_ssp585[Std_ssp585$V1=="2020",]$V13)
STD_FlatArea2100 <- (STD_slo_ssp585_2100[1:1]+STD_Area_DA_ssp585[11:11])
STD_FlatArea2050 <- (STD_slo_ssp585_2050[1:1]+STD_Area_DA_ssp585[6:6])
STD_SteepArea2100 <- (STD_slo_ssp585_2100[2:80])
STD_SteepArea2050 <- (STD_slo_ssp585_2050[2:80])
all_STD_slo_ssp585_2100 <- c(STD_FlatArea2100, STD_SteepArea2100)
all_STD_slo_ssp585_2050 <- c(STD_FlatArea2050, STD_SteepArea2050)

# STD370
STD_slo_ssp370_2100 <- (STD_slo_ssp370$V12)
STD_slo_ssp370_2050 <- (STD_slo_ssp370$V7)
STD_Area_DA_ssp370 <- (Std_ssp370$V13-Std_ssp370[Std_ssp370$V1=="2020",]$V13)
STD_FlatArea2100 <- (STD_slo_ssp370_2100[1:1]+STD_Area_DA_ssp370[11:11])
STD_FlatArea2050 <- (STD_slo_ssp370_2050[1:1]+STD_Area_DA_ssp370[6:6])
STD_SteepArea2100 <- (STD_slo_ssp370_2100[2:80])
STD_SteepArea2050 <- (STD_slo_ssp370_2050[2:80])
all_STD_slo_ssp370_2100 <- c(STD_FlatArea2100, STD_SteepArea2100)
all_STD_slo_ssp370_2050 <- c(STD_FlatArea2050, STD_SteepArea2050)

# STD245
STD_slo_ssp245_2100 <- (STD_slo_ssp245$V12)
STD_slo_ssp245_2050 <- (STD_slo_ssp245$V7)
STD_Area_DA_ssp245 <- (Std_ssp245$V13-Std_ssp245[Std_ssp245$V1=="2020",]$V13)
STD_FlatArea2100 <- (STD_slo_ssp245_2100[1:1]+STD_Area_DA_ssp245[11:11])
STD_FlatArea2050 <- (STD_slo_ssp245_2050[1:1]+STD_Area_DA_ssp245[6:6])
STD_SteepArea2100 <- (STD_slo_ssp245_2100[2:80])
STD_SteepArea2050 <- (STD_slo_ssp245_2050[2:80])
all_STD_slo_ssp245_2100 <- c(STD_FlatArea2100, STD_SteepArea2100)
all_STD_slo_ssp245_2050 <- c(STD_FlatArea2050, STD_SteepArea2050)

# STD126
STD_slo_ssp126_2100 <- (STD_slo_ssp126$V12)
STD_slo_ssp126_2050 <- (STD_slo_ssp126$V7)
STD_Area_DA_ssp126 <- (Std_ssp126$V13-Std_ssp126[Std_ssp126$V1=="2020",]$V13)
STD_FlatArea2100 <- (STD_slo_ssp126_2100[1:1]+STD_Area_DA_ssp126[11:11])
STD_FlatArea2050 <- (STD_slo_ssp126_2050[1:1]+STD_Area_DA_ssp126[6:6])
STD_SteepArea2100 <- (STD_slo_ssp126_2100[2:80])
STD_SteepArea2050 <- (STD_slo_ssp126_2050[2:80])
all_STD_slo_ssp126_2100 <- c(STD_FlatArea2100, STD_SteepArea2100)
all_STD_slo_ssp126_2050 <- c(STD_FlatArea2050, STD_SteepArea2050)

# STD119
STD_slo_ssp119_2100 <- (STD_slo_ssp119$V12)
STD_slo_ssp119_2050 <- (STD_slo_ssp119$V7)
STD_Area_DA_ssp119 <- (Std_ssp119$V13-Std_ssp119[Std_ssp119$V1=="2020",]$V13)
STD_FlatArea2100 <- (STD_slo_ssp119_2100[1:1]+STD_Area_DA_ssp119[11:11])
STD_FlatArea2050 <- (STD_slo_ssp119_2050[1:1]+STD_Area_DA_ssp119[6:6])
STD_SteepArea2100 <- (STD_slo_ssp119_2100[2:80])
STD_SteepArea2050 <- (STD_slo_ssp119_2050[2:80])
all_STD_slo_ssp119_2100 <- c(STD_FlatArea2100, STD_SteepArea2100)
all_STD_slo_ssp119_2050 <- c(STD_FlatArea2050, STD_SteepArea2050)

ymax <- max(all_SLO_ssp585_2100+all_STD_slo_ssp585_2100)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(SLO_ssp126$V1,all_SLO_ssp119_2100, axes=FALSE, main="Slope in deglaciated area", xlab="", ylab="", lwd=0.7, xlim=c(0,80),ylim=c(0,ymax), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("slope (degree)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(30,30),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(all_SLO_ssp126_2100+all_STD_slo_ssp126_2100)
StNeg<-(all_SLO_ssp126_2100-all_STD_slo_ssp126_2100)
# Polygone de la StDev ssp126
polygon(x=c(SLO_ssp126$V1,rev(SLO_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(all_SLO_ssp585_2100+all_STD_slo_ssp585_2100)
StNeg<-(all_SLO_ssp585_2100-all_STD_slo_ssp585_2100)
# Polygone de la StDev ssp585
polygon(x=c(SLO_ssp585$V1,rev(SLO_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data slope 2050
lines(SLO_ssp585$V1,all_SLO_ssp585_2050,col="firebrick4",lty=2, lwd=1)
lines(SLO_ssp370$V1,all_SLO_ssp370_2050,col="red",lty=2, lwd=1)
lines(SLO_ssp245$V1,all_SLO_ssp245_2050,col="orange",lty=2, lwd=1)
lines(SLO_ssp126$V1,all_SLO_ssp126_2050,col="darkblue",lty=2, lwd=1)
lines(SLO_ssp119$V1,all_SLO_ssp119_2050,col="dodgerblue",lty=2, lwd=1)

# Data slope 2100
lines(SLO_ssp585$V1,all_SLO_ssp585_2100,col="firebrick4",lwd=1.7)
lines(SLO_ssp370$V1,all_SLO_ssp370_2100,col="red",lwd=1.7)
lines(SLO_ssp245$V1,all_SLO_ssp245_2100,col="orange",lwd=1.7)
lines(SLO_ssp126$V1,all_SLO_ssp126_2100,col="darkblue",lwd=1.7)
lines(SLO_ssp119$V1,all_SLO_ssp119_2100,col="dodgerblue",lwd=1.7)

legend("topright", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(80,80),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Deglaciated  area Slope Sum####

png(paste(chemin_figures,r,"/",r,"_Slope_land_area_SUM.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Ajout colonne_cummul??e
# a<-cumsum(SLO_ssp585$V12)
# b<-cbind(SLO_ssp585,a)
# lines(b$V13,b$V1,col="darkblue",lty=2, lwd=1)

Varcumsum585<-STD_slo_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum126<-STD_slo_ssp126$V12^2
STDcumsum126<-sqrt(cumsum(Varcumsum126))
Ymax_abs<-(max(cumsum(SLO_ssp585$V12)-cumsum(SLO_ssp585$V4)+STDcumsum585))

# Plot 1
plot(SLO_ssp126$V1,cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4), axes=FALSE, main="Slope in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(0,80),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("slope (degree)",side=1, line=2.5)
mtext(expression("cumulated area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(30,30),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4)+STDcumsum126)
StNeg<-(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4)-STDcumsum126)
# Polygone de la StDev ssp126
polygon(x=c(SLO_ssp126$V1,rev(SLO_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(SLO_ssp585$V12)-cumsum(SLO_ssp585$V4)+STDcumsum585)
StNeg<-(cumsum(SLO_ssp585$V12)-cumsum(SLO_ssp585$V4)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(SLO_ssp585$V1,rev(SLO_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data slope 2100
lines(SLO_ssp585$V1,cumsum(SLO_ssp585$V12)-cumsum(SLO_ssp585$V4),col="firebrick4",lwd=1.7)
lines(SLO_ssp370$V1,cumsum(SLO_ssp370$V12)-cumsum(SLO_ssp370$V4),col="red",lwd=1.7)
lines(SLO_ssp245$V1,cumsum(SLO_ssp245$V12)-cumsum(SLO_ssp245$V4),col="orange",lwd=1.7)
lines(SLO_ssp126$V1,cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4),col="darkblue",lwd=1.7)
lines(SLO_ssp119$V1,cumsum(SLO_ssp119$V12)-cumsum(SLO_ssp119$V4),col="dodgerblue",lwd=1.7)

# Data slope 2050
lines(SLO_ssp585$V1,cumsum(SLO_ssp585$V7)-cumsum(SLO_ssp585$V4),col="firebrick4",lty=2, lwd=1)
lines(SLO_ssp370$V1,cumsum(SLO_ssp370$V7)-cumsum(SLO_ssp370$V4),col="red",lty=2, lwd=1)
lines(SLO_ssp245$V1,cumsum(SLO_ssp245$V7)-cumsum(SLO_ssp245$V4),col="orange",lty=2, lwd=1)
lines(SLO_ssp126$V1,cumsum(SLO_ssp126$V7)-cumsum(SLO_ssp126$V4),col="darkblue",lty=2, lwd=1)
lines(SLO_ssp119$V1,cumsum(SLO_ssp119$V7)-cumsum(SLO_ssp119$V4),col="dodgerblue",lty=2, lwd=1)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(80,80),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####All DA area Slope Sum####

png(paste(chemin_figures,r,"/",r,"_Slope_deglaciated_area_SUM.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum_DA_585<-all_STD_slo_ssp585_2100^2
STDcumsum_DA_585<-sqrt(cumsum(Varcumsum_DA_585))
Varcumsum_DA_126<-all_STD_slo_ssp126_2100^2
STDcumsum_DA_126<-sqrt(cumsum(Varcumsum_DA_126))
Ymax_abs<-(max(cumsum(all_SLO_ssp585_2100)+STDcumsum_DA_585))

# Plot 1
plot(SLO_ssp126$V1,cumsum(all_SLO_ssp119_2100), axes=FALSE, main="Slope in deglaciated area", xlab="", ylab="", lwd=0.7, xlim=c(0,80),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("slope (degree)",side=1, line=2.5)
mtext(expression("cumulated area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(30,30),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(cumsum(all_SLO_ssp126_2100)+STDcumsum_DA_126)
StNeg<-(cumsum(all_SLO_ssp126_2100)-STDcumsum_DA_126)
# Polygone de la StDev ssp126
polygon(x=c(SLO_ssp126$V1,rev(SLO_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(all_SLO_ssp585_2100)+STDcumsum_DA_585)
StNeg<-(cumsum(all_SLO_ssp585_2100)-STDcumsum_DA_585)
# Polygone de la StDev ssp585
polygon(x=c(SLO_ssp585$V1,rev(SLO_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data slope 2100
lines(SLO_ssp585$V1,cumsum(all_SLO_ssp585_2100),col="firebrick4",lwd=1.7)
lines(SLO_ssp370$V1,cumsum(all_SLO_ssp370_2100),col="red",lwd=1.7)
lines(SLO_ssp245$V1,cumsum(all_SLO_ssp245_2100),col="orange",lwd=1.7)
lines(SLO_ssp126$V1,cumsum(all_SLO_ssp126_2100),col="darkblue",lwd=1.7)
lines(SLO_ssp119$V1,cumsum(all_SLO_ssp119_2100),col="dodgerblue",lwd=1.7)

# Data slope 2050
lines(SLO_ssp585$V1,cumsum(all_SLO_ssp585_2050),col="firebrick4",lty=2, lwd=1)
lines(SLO_ssp370$V1,cumsum(all_SLO_ssp370_2050),col="red",lty=2, lwd=1)
lines(SLO_ssp245$V1,cumsum(all_SLO_ssp245_2050),col="orange",lty=2, lwd=1)
lines(SLO_ssp126$V1,cumsum(all_SLO_ssp126_2050),col="darkblue",lty=2, lwd=1)
lines(SLO_ssp119$V1,cumsum(all_SLO_ssp119_2050),col="dodgerblue",lty=2, lwd=1)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(80,80),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()



##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Land area Slope Sum %####

png(paste(chemin_figures,r,"/",r,"_Slope_land_area_SUM_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(SLO_ssp126$V1,(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4))/(max(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4))*100), axes=FALSE, main="Slope in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(0,80),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")

axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("slope (degree)",side=1, line=2.5)
mtext("cumulated area (in %)",side=2, line=2.5)
lines(x=c(30,30),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
lines(x=c(-1000000,1000000),y=c(50,50),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data slope 2100
lines(SLO_ssp585$V1,100*(cumsum(SLO_ssp585$V12)-cumsum(SLO_ssp585$V4))/(max(cumsum(SLO_ssp585$V12)-cumsum(SLO_ssp585$V4))),col="firebrick4",lwd=1.7)
lines(SLO_ssp370$V1,100*(cumsum(SLO_ssp370$V12)-cumsum(SLO_ssp370$V4))/(max(cumsum(SLO_ssp370$V12)-cumsum(SLO_ssp370$V4))),col="red",lwd=1.7)
lines(SLO_ssp245$V1,100*(cumsum(SLO_ssp245$V12)-cumsum(SLO_ssp245$V4))/(max(cumsum(SLO_ssp245$V12)-cumsum(SLO_ssp245$V4))),col="orange",lwd=1.7)
lines(SLO_ssp126$V1,100*(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4))/(max(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4))),col="darkblue",lwd=1.7)
lines(SLO_ssp119$V1,100*(cumsum(SLO_ssp119$V12)-cumsum(SLO_ssp119$V4))/(max(cumsum(SLO_ssp119$V12)-cumsum(SLO_ssp119$V4))),col="dodgerblue",lwd=1.7)

# Data slope 2050
lines(SLO_ssp585$V1,100*(cumsum(SLO_ssp585$V7)-cumsum(SLO_ssp585$V4))/(max(cumsum(SLO_ssp585$V7)-cumsum(SLO_ssp585$V4))),col="firebrick4",lty=2, lwd=1)
lines(SLO_ssp370$V1,100*(cumsum(SLO_ssp370$V7)-cumsum(SLO_ssp370$V4))/(max(cumsum(SLO_ssp370$V7)-cumsum(SLO_ssp370$V4))),col="red",lty=2, lwd=1)
lines(SLO_ssp245$V1,100*(cumsum(SLO_ssp245$V7)-cumsum(SLO_ssp245$V4))/(max(cumsum(SLO_ssp245$V7)-cumsum(SLO_ssp245$V4))),col="orange",lty=2, lwd=1)
lines(SLO_ssp126$V1,100*(cumsum(SLO_ssp126$V7)-cumsum(SLO_ssp126$V4))/(max(cumsum(SLO_ssp126$V7)-cumsum(SLO_ssp126$V4))),col="darkblue",lty=2, lwd=1)
lines(SLO_ssp119$V1,100*(cumsum(SLO_ssp119$V7)-cumsum(SLO_ssp119$V4))/(max(cumsum(SLO_ssp119$V7)-cumsum(SLO_ssp119$V4))),col="dodgerblue",lty=2, lwd=1)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####DA area Slope Sum %####


png(paste(chemin_figures,r,"/",r,"_Slope_deglaciated_area_SUM_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(SLO_ssp126$V1,(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4))/(max(cumsum(SLO_ssp126$V12)-cumsum(SLO_ssp126$V4))*100), axes=FALSE, main="Slope in deglaciated area", xlab="", ylab="", lwd=1.7, xlim=c(0,80),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")

axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("slope (degree)",side=1, line=2.5)
mtext("cumulated area (in %)",side=2, line=2.5)
lines(x=c(30,30),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
lines(x=c(-1000000,1000000),y=c(50,50),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data slope 2100
lines(SLO_ssp585$V1,100*(cumsum(all_SLO_ssp585_2100)/(max(cumsum(all_SLO_ssp585_2100)))),col="firebrick4",lwd=1.7)
lines(SLO_ssp370$V1,100*(cumsum(all_SLO_ssp370_2100)/(max(cumsum(all_SLO_ssp370_2100)))),col="red",lwd=1.7)
lines(SLO_ssp245$V1,100*(cumsum(all_SLO_ssp245_2100)/(max(cumsum(all_SLO_ssp245_2100)))),col="orange",lwd=1.7)
lines(SLO_ssp126$V1,100*(cumsum(all_SLO_ssp126_2100)/(max(cumsum(all_SLO_ssp126_2100)))),col="darkblue",lwd=1.7)
lines(SLO_ssp119$V1,100*(cumsum(all_SLO_ssp119_2100)/(max(cumsum(all_SLO_ssp119_2100)))),col="dodgerblue",lwd=1.7)

# Data slope 2050
lines(SLO_ssp585$V1,100*(cumsum(all_SLO_ssp585_2050)/(max(cumsum(all_SLO_ssp585_2050)))),col="firebrick4",lty=2, lwd=1)
lines(SLO_ssp370$V1,100*(cumsum(all_SLO_ssp370_2050)/(max(cumsum(all_SLO_ssp370_2050)))),col="red",lty=2, lwd=1)
lines(SLO_ssp245$V1,100*(cumsum(all_SLO_ssp245_2050)/(max(cumsum(all_SLO_ssp245_2050)))),col="orange",lty=2, lwd=1)
lines(SLO_ssp126$V1,100*(cumsum(all_SLO_ssp126_2050)/(max(cumsum(all_SLO_ssp126_2050)))),col="darkblue",lty=2, lwd=1)
lines(SLO_ssp119$V1,100*(cumsum(all_SLO_ssp119_2050)/(max(cumsum(all_SLO_ssp119_2050)))),col="dodgerblue",lty=2, lwd=1)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

dev.off()





###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####///MAAT FILES DA####


####MAAT Land AREA####
png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Lecture fichier qui contient les data
for (s in  Scenario){  
  assign(paste("MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/MAAT_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_MAAT_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 

# Temp<-seq(-30, 19.5, by = 0.5)

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp126$V1, MAAT_ssp126$V12, axes=FALSE, main="MAAT in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(MAAT_ssp126$V12+STD_MAAT_ssp126$V12)
StNeg<-(MAAT_ssp126$V12-STD_MAAT_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(MAAT_ssp585$V12+STD_MAAT_ssp585$V12)
StNeg<-(MAAT_ssp585$V12-STD_MAAT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2050
lines(MAAT_ssp585$V1,MAAT_ssp585$V7,col="firebrick4",lty=2, lwd=1)
lines(MAAT_ssp370$V1,MAAT_ssp370$V7,col="red",lty=2, lwd=1)
lines(MAAT_ssp245$V1,MAAT_ssp245$V7,col="orange",lty=2, lwd=1)
lines(MAAT_ssp126$V1,MAAT_ssp126$V7,col="darkblue",lty=2, lwd=1)
lines(MAAT_ssp119$V1,MAAT_ssp119$V7,col="dodgerblue",lty=2, lwd=1)

# Data MAAT 2100
lines(MAAT_ssp585$V1,MAAT_ssp585$V12,col="firebrick4",lwd=1.7)
lines(MAAT_ssp370$V1,MAAT_ssp370$V12,col="red",lwd=1.7)
lines(MAAT_ssp245$V1,MAAT_ssp245$V12,col="orange",lwd=1.7)
lines(MAAT_ssp126$V1,MAAT_ssp126$V12,col="darkblue",lwd=1.7)
lines(MAAT_ssp119$V1,MAAT_ssp119$V12,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2050","2100","SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("black","black","firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=c(2,1,1,1,1,1,1), lwd=c(0.7,1,1,1,1,1,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT Land AREA SUM####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SUM_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum585<-STD_MAAT_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum126<-STD_MAAT_ssp126$V12^2
STDcumsum126<-sqrt(cumsum(Varcumsum126))
Ymax_abs<-(max(cumsum(MAAT_ssp585$V12)+STDcumsum585))

# Plot 1
plot(MAAT_ssp126$V1, cumsum(MAAT_ssp126$V12), axes=FALSE, main="MAAT in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(-5,-5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(5,5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(-1000000,1000000),y=c(1000,1000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(-1000000,1000000),y=c(2000,2000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(cumsum(MAAT_ssp126$V12)+STDcumsum126)
StNeg<-(cumsum(MAAT_ssp126$V12)-STDcumsum126)
# Polygone de la StDev ssp126
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(MAAT_ssp585$V12)+STDcumsum585)
StNeg<-(cumsum(MAAT_ssp585$V12)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2050
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V7),col="firebrick4",lwd=1, lty=2)
lines(MAAT_ssp370$V1,cumsum(MAAT_ssp370$V7),col="red",lwd=1, lty=2)
lines(MAAT_ssp245$V1,cumsum(MAAT_ssp245$V7),col="orange",lwd=1, lty=2)
lines(MAAT_ssp126$V1,cumsum(MAAT_ssp126$V7),col="darkblue",lwd=1, lty=2)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V7),col="dodgerblue",lwd=1, lty=2)

# Data MAAT 2100
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V12),col="firebrick4",lwd=1.7)
lines(MAAT_ssp370$V1,cumsum(MAAT_ssp370$V12),col="red",lwd=1.7)
lines(MAAT_ssp245$V1,cumsum(MAAT_ssp245$V12),col="orange",lwd=1.7)
lines(MAAT_ssp126$V1,cumsum(MAAT_ssp126$V12),col="darkblue",lwd=1.7)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V12),col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2050","2100","SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("black","black","firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=c(2,1,1,1,1,1,1), lwd=c(0.7,1,1,1,1,1,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT Land AREA SUM %####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SUM_percentage_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)


# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(MAAT_ssp126$V1,(cumsum(MAAT_ssp126$V12))/(max(cumsum(MAAT_ssp126$V12))*100), axes=FALSE, main="MAAT in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")

axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext("cumulated area (in %)",side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
lines(x=c(-1000000,1000000),y=c(50,50),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data MAATpe 2100
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V12))/(max(cumsum(MAAT_ssp585$V12))),col="firebrick4",lwd=1.7)
lines(MAAT_ssp370$V1,100*(cumsum(MAAT_ssp370$V12))/(max(cumsum(MAAT_ssp370$V12))),col="red",lwd=1.7)
lines(MAAT_ssp245$V1,100*(cumsum(MAAT_ssp245$V12))/(max(cumsum(MAAT_ssp245$V12))),col="orange",lwd=1.7)
lines(MAAT_ssp126$V1,100*(cumsum(MAAT_ssp126$V12))/(max(cumsum(MAAT_ssp126$V12))),col="darkblue",lwd=1.7)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V12))/(max(cumsum(MAAT_ssp119$V12))),col="dodgerblue",lwd=1.7)

# Data MAATpe 2050
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V7))/(max(cumsum(MAAT_ssp585$V7))),col="firebrick4",lty=2, lwd=1)
lines(MAAT_ssp370$V1,100*(cumsum(MAAT_ssp370$V7))/(max(cumsum(MAAT_ssp370$V7))),col="red",lty=2, lwd=1)
lines(MAAT_ssp245$V1,100*(cumsum(MAAT_ssp245$V7))/(max(cumsum(MAAT_ssp245$V7))),col="orange",lty=2, lwd=1)
lines(MAAT_ssp126$V1,100*(cumsum(MAAT_ssp126$V7))/(max(cumsum(MAAT_ssp126$V7))),col="darkblue",lty=2, lwd=1)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V7))/(max(cumsum(MAAT_ssp119$V7))),col="dodgerblue",lty=2, lwd=1)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT Land AREA SSP119####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SSP119_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp119$V1,MAAT_ssp119$V12, axes=FALSE, xlab="", ylab="", lwd=1.7, main="MAAT in emerging land area under SSP119", xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp119
StPos<-(MAAT_ssp119$V12+STD_MAAT_ssp119$V12)
StNeg<-(MAAT_ssp119$V12-STD_MAAT_ssp119$V12)
# Polygone de la StDev ssp119
polygon(x=c(MAAT_ssp119$V1,rev(MAAT_ssp119$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0.5,1,alpha = 0.3),border=NA)

# Data MAAT 2020-2100
lines(MAAT_ssp119$V1,MAAT_ssp119$V4,col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,MAAT_ssp119$V6,col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,MAAT_ssp119$V8,col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,MAAT_ssp119$V10,col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,MAAT_ssp119$V12,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2020", "2040", "2060","2080","2100"), col="dodgerblue", lwd=c(0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT Land AREA SSP126####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SSP126_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)


# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp126$V1,MAAT_ssp126$V12, axes=FALSE, xlab="", ylab="", main="MAAT in emerging land area under SSP126", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(MAAT_ssp126$V12+STD_MAAT_ssp126$V12)
StNeg<-(MAAT_ssp126$V12-STD_MAAT_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp126$V1,MAAT_ssp126$V4,col="darkblue",lwd=0.9)
lines(MAAT_ssp126$V1,MAAT_ssp126$V6,col="darkblue",lwd=1.1)
lines(MAAT_ssp126$V1,MAAT_ssp126$V8,col="darkblue",lwd=1.3)
lines(MAAT_ssp126$V1,MAAT_ssp126$V10,col="darkblue",lwd=1.5)
lines(MAAT_ssp126$V1,MAAT_ssp126$V12,col="darkblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2020", "2040", "2060","2080","2100"), col="darkblue", lwd=c(0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT land AREA SSP585####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SSP585_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp585$V1,MAAT_ssp585$V12, axes=FALSE, main="MAAT in emerging land area under SSP585", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp585
StPos<-(MAAT_ssp585$V12+STD_MAAT_ssp585$V12)
StNeg<-(MAAT_ssp585$V12-STD_MAAT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp585$V1,MAAT_ssp585$V4,col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,MAAT_ssp585$V6,col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,MAAT_ssp585$V8,col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,MAAT_ssp585$V10,col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,MAAT_ssp585$V12,col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("2020", "2040", "2060","2080","2100"), col="firebrick4", lwd=c(0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT Land AREA SSP119 & 585####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SSP119-585_2100-2000.png.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp119$V1,MAAT_ssp119$V12, axes=FALSE, xlab="", ylab="", main="MAAT in emerging land area", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp119
StPos<-(MAAT_ssp119$V12+STD_MAAT_ssp119$V12)
StNeg<-(MAAT_ssp119$V12-STD_MAAT_ssp119$V12)
# Polygone de la StDev ssp119
polygon(x=c(MAAT_ssp119$V1,rev(MAAT_ssp119$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0.5,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(MAAT_ssp585$V12+STD_MAAT_ssp585$V12)
StNeg<-(MAAT_ssp585$V12-STD_MAAT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp119$V1,MAAT_ssp119$V4,col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,MAAT_ssp119$V6,col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,MAAT_ssp119$V8,col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,MAAT_ssp119$V10,col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,MAAT_ssp119$V12,col="dodgerblue",lwd=1.7)
lines(MAAT_ssp585$V1,MAAT_ssp585$V4,col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,MAAT_ssp585$V6,col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,MAAT_ssp585$V8,col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,MAAT_ssp585$V10,col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,MAAT_ssp585$V12,col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP119", "SSP585", "2020", "2040", "2060","2080","2100"), col=c("dodgerblue","firebrick4","black","black","black","black","black"), lwd=c(1,1,0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT land AREA SSP119 & 585 SUM####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SSP119-585_SUM_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum585<-STD_MAAT_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum119<-STD_MAAT_ssp119$V12^2
STDcumsum119<-sqrt(cumsum(Varcumsum119))
Ymax_abs<-(max(cumsum(MAAT_ssp585$V12)+STDcumsum585))

# Plot 1
plot(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V12), axes=FALSE, main="MAAT in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(-5,-5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(5,5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(-1000000,1000000),y=c(1000,1000),col=rgb(0.5,0.5,0.5,alpha=0.3))
#lines(x=c(-1000000,1000000),y=c(2000,2000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp119
StPos<-(cumsum(MAAT_ssp119$V12)+STDcumsum119)
StNeg<-(cumsum(MAAT_ssp119$V12)-STDcumsum119)
# Polygone de la StDev ssp119
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0.5,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(MAAT_ssp585$V12)+STDcumsum585)
StNeg<-(cumsum(MAAT_ssp585$V12)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)
# Plot 1

# Data MAAT 
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V4),col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V6),col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V8),col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V10),col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V12),col="dodgerblue",lwd=1.7)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V4),col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V6),col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V8),col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V10),col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V12),col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP119", "SSP585", "2020", "2040", "2060","2080","2100"), col=c("dodgerblue","firebrick4","black","black","black","black","black"), lwd=c(1,1,0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT Land AREA SSP119 & 585 SUM %####

png(paste(chemin_figures,r,"/",r,"_MAAT_Land_area_SSP119-585_SUM_Percentage_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)



# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(MAAT_ssp119$V1,(cumsum(MAAT_ssp119$V12))/(max(cumsum(MAAT_ssp119$V12))*100), axes=FALSE, main="MAAT in emerging land area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")

axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext("cumulated area (in %)",side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
lines(x=c(-1000000,1000000),y=c(50,50),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data MAAT 
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V4))/(max(cumsum(MAAT_ssp119$V4))),col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V6))/(max(cumsum(MAAT_ssp119$V6))),col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V8))/(max(cumsum(MAAT_ssp119$V8))),col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V10))/(max(cumsum(MAAT_ssp119$V10))),col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V12))/(max(cumsum(MAAT_ssp119$V12))),col="dodgerblue",lwd=1.7)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V4))/(max(cumsum(MAAT_ssp585$V4))),col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V6))/(max(cumsum(MAAT_ssp585$V6))),col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V8))/(max(cumsum(MAAT_ssp585$V8))),col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V10))/(max(cumsum(MAAT_ssp585$V10))),col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V12))/(max(cumsum(MAAT_ssp585$V12))),col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP119", "SSP585", "2020", "2040", "2060","2080","2100"), col=c("dodgerblue","firebrick4","black","black","black","black","black"), lwd=c(1,1,0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

dev.off()



                                   # MAAT Overdeepening area


####///MAAT FILES Overdeepening area####
####MAAT Overdeepening area#####
###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)



# affichage graph double Y
par(mar=c(4,4,2,2))

# Lecture fichier qui contient les data

for (s in  Scenario){  
  assign(paste("MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/MAAT_lakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_MAAT_delakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 


Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp126$V1,MAAT_ssp126$V12, axes=FALSE, main="MAAT in emerging overdeepening area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(MAAT_ssp126$V12+STD_MAAT_ssp126$V12)
StNeg<-(MAAT_ssp126$V12-STD_MAAT_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(MAAT_ssp585$V12+STD_MAAT_ssp585$V12)
StNeg<-(MAAT_ssp585$V12-STD_MAAT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2050
lines(MAAT_ssp585$V1,MAAT_ssp585$V7,col="firebrick4",lty=2, lwd=1)
lines(MAAT_ssp370$V1,MAAT_ssp370$V7,col="red",lty=2, lwd=1)
lines(MAAT_ssp245$V1,MAAT_ssp245$V7,col="orange",lty=2, lwd=1)
lines(MAAT_ssp126$V1,MAAT_ssp126$V7,col="darkblue",lty=2, lwd=1)
lines(MAAT_ssp119$V1,MAAT_ssp119$V7,col="dodgerblue",lty=2, lwd=1)

# Data MAAT 2100/Volumes/LACIE SHARE/Matthias_data_analyse/Figures/RGI11/RGI11_OBSL_area.png
lines(MAAT_ssp126$V1,MAAT_ssp585$V12,col="firebrick4",lwd=1.7)
lines(MAAT_ssp126$V1,MAAT_ssp370$V12,col="red",lwd=1.7)
lines(MAAT_ssp126$V1,MAAT_ssp245$V12,col="orange",lwd=1.7)
lines(MAAT_ssp126$V1,MAAT_ssp126$V12,col="darkblue",lwd=1.7)
lines(MAAT_ssp126$V1,MAAT_ssp119$V12,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2050","2100","SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("black","black","firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=c(2,1,1,1,1,1,1), lwd=c(0.7,1,1,1,1,1,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SUM#####


png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SUM_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum585<-STD_MAAT_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum126<-STD_MAAT_ssp126$V12^2
STDcumsum126<-sqrt(cumsum(Varcumsum126))
Ymax_abs<-(max(cumsum(MAAT_ssp585$V12)+STDcumsum585))

# Plot 1
plot(MAAT_ssp126$V1, cumsum(MAAT_ssp126$V12), axes=FALSE, main="MAAT in emerging overdeepening area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(cumsum(MAAT_ssp126$V12)+STDcumsum126)
StNeg<-(cumsum(MAAT_ssp126$V12)-STDcumsum126)
# Polygone de la StDev ssp126
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(MAAT_ssp585$V12)+STDcumsum585)
StNeg<-(cumsum(MAAT_ssp585$V12)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2050
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V7),col="firebrick4",lwd=1, lty=2)
lines(MAAT_ssp370$V1,cumsum(MAAT_ssp370$V7),col="red",lwd=1, lty=2)
lines(MAAT_ssp245$V1,cumsum(MAAT_ssp245$V7),col="orange",lwd=1, lty=2)
lines(MAAT_ssp126$V1,cumsum(MAAT_ssp126$V7),col="darkblue",lwd=1, lty=2)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V7),col="dodgerblue",lwd=1, lty=2)

# Data MAAT 2100
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V12),col="firebrick4",lwd=1.7)
lines(MAAT_ssp370$V1,cumsum(MAAT_ssp370$V12),col="red",lwd=1.7)
lines(MAAT_ssp245$V1,cumsum(MAAT_ssp245$V12),col="orange",lwd=1.7)
lines(MAAT_ssp126$V1,cumsum(MAAT_ssp126$V12),col="darkblue",lwd=1.7)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V12),col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2050","2100","SSP585", "SSP370", "SSP245", "SSP126","SSP119"), col=c("black","black","firebrick4", "red", "orange", "darkblue","dodgerblue"), lty=c(2,1,1,1,1,1,1), lwd=c(0.7,1,1,1,1,1,1), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening AREA SUM %####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SUM_Percentage_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(MAAT_ssp126$V1,(cumsum(MAAT_ssp126$V12))/(max(cumsum(MAAT_ssp126$V12))*100), axes=FALSE, main="MAAT in emerging overdeepening area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext("cumulated area (in %)",side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
lines(x=c(-1000000,1000000),y=c(50,50),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data MAATpe 2100
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V12))/(max(cumsum(MAAT_ssp585$V12))),col="firebrick4",lwd=1.7)
lines(MAAT_ssp370$V1,100*(cumsum(MAAT_ssp370$V12))/(max(cumsum(MAAT_ssp370$V12))),col="red",lwd=1.7)
lines(MAAT_ssp245$V1,100*(cumsum(MAAT_ssp245$V12))/(max(cumsum(MAAT_ssp245$V12))),col="orange",lwd=1.7)
lines(MAAT_ssp126$V1,100*(cumsum(MAAT_ssp126$V12))/(max(cumsum(MAAT_ssp126$V12))),col="darkblue",lwd=1.7)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V12))/(max(cumsum(MAAT_ssp119$V12))),col="dodgerblue",lwd=1.7)

# Data MAATpe 2050
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V7))/(max(cumsum(MAAT_ssp585$V7))),col="firebrick4",lty=2, lwd=1)
lines(MAAT_ssp370$V1,100*(cumsum(MAAT_ssp370$V7))/(max(cumsum(MAAT_ssp370$V7))),col="red",lty=2, lwd=1)
lines(MAAT_ssp245$V1,100*(cumsum(MAAT_ssp245$V7))/(max(cumsum(MAAT_ssp245$V7))),col="orange",lty=2, lwd=1)
lines(MAAT_ssp126$V1,100*(cumsum(MAAT_ssp126$V7))/(max(cumsum(MAAT_ssp126$V7))),col="darkblue",lty=2, lwd=1)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V7))/(max(cumsum(MAAT_ssp119$V7))),col="dodgerblue",lty=2, lwd=1)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

dev.off()
  
###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SSP119#####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SSP119_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp119$V1,MAAT_ssp119$V12, axes=FALSE, xlab="", ylab="", main="MAAT in emerging overdeepening area under SSP119", cex.main=0.9, lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp119
StPos<-(MAAT_ssp119$V12+STD_MAAT_ssp119$V12)
StNeg<-(MAAT_ssp119$V12-STD_MAAT_ssp119$V12)
# Polygone de la StDev ssp119
polygon(x=c(MAAT_ssp119$V1,rev(MAAT_ssp119$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0.5,1,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp119$V1,MAAT_ssp119$V4,col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,MAAT_ssp119$V6,col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,MAAT_ssp119$V8,col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,MAAT_ssp119$V10,col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,MAAT_ssp119$V12,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2020", "2040", "2060","2080","2100"), col="dodgerblue", lwd=c(0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SSP126#####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SSP126_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp126$V1,MAAT_ssp126$V12, axes=FALSE, xlab="", ylab="", main="MAAT in emerging overdeepening area under SSP126", cex.main=0.9, lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(MAAT_ssp126$V12+STD_MAAT_ssp126$V12)
StNeg<-(MAAT_ssp126$V12-STD_MAAT_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp126$V1,MAAT_ssp126$V4,col="darkblue",lwd=0.9)
lines(MAAT_ssp126$V1,MAAT_ssp126$V6,col="darkblue",lwd=1.1)
lines(MAAT_ssp126$V1,MAAT_ssp126$V8,col="darkblue",lwd=1.3)
lines(MAAT_ssp126$V1,MAAT_ssp126$V10,col="darkblue",lwd=1.5)
lines(MAAT_ssp126$V1,MAAT_ssp126$V12,col="darkblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("2020", "2040", "2060","2080","2100"), col="darkblue", lwd=c(0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SSP585#####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SSP585_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp585$V1,MAAT_ssp585$V12, axes=FALSE, xlab="", ylab="", main="MAAT in emerging overdeepening area under SSP585", cex.main=0.9, lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp585
StPos<-(MAAT_ssp585$V12+STD_MAAT_ssp585$V12)
StNeg<-(MAAT_ssp585$V12-STD_MAAT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp585$V1,MAAT_ssp585$V4,col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,MAAT_ssp585$V6,col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,MAAT_ssp585$V8,col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,MAAT_ssp585$V10,col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,MAAT_ssp585$V12,col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("2020", "2040", "2060","2080","2100"), col="firebrick4", lwd=c(0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SSP119 & 585#####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SSP119-585_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Ymax_abs585<-(max(MAAT_ssp585$V12+STD_MAAT_ssp585$V12))
Ymax_abs126<-(max(MAAT_ssp126$V12+STD_MAAT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(MAAT_ssp119$V1,MAAT_ssp119$V12, axes=FALSE, xlab="", ylab="", lwd=1.7, main="MAAT in emerging overdeepening area", xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp119
StPos<-(MAAT_ssp119$V12+STD_MAAT_ssp119$V12)
StNeg<-(MAAT_ssp119$V12-STD_MAAT_ssp119$V12)
# Polygone de la StDev ssp119
polygon(x=c(MAAT_ssp119$V1,rev(MAAT_ssp119$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0.5,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(MAAT_ssp585$V12+STD_MAAT_ssp585$V12)
StNeg<-(MAAT_ssp585$V12-STD_MAAT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data MAAT 2100
lines(MAAT_ssp119$V1,MAAT_ssp119$V4,col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,MAAT_ssp119$V6,col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,MAAT_ssp119$V8,col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,MAAT_ssp119$V10,col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,MAAT_ssp119$V12,col="dodgerblue",lwd=1.7)
lines(MAAT_ssp585$V1,MAAT_ssp585$V4,col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,MAAT_ssp585$V6,col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,MAAT_ssp585$V8,col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,MAAT_ssp585$V10,col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,MAAT_ssp585$V12,col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP119", "SSP585", "2020", "2040", "2060","2080","2100"), col=c("dodgerblue","firebrick4","black","black","black","black","black"), lwd=c(1,1,0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SSP119 & 585 SUM#####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SSP119-585_SUM_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum585<-STD_MAAT_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum119<-STD_MAAT_ssp119$V12^2
STDcumsum119<-sqrt(cumsum(Varcumsum119))
Ymax_abs<-(max(cumsum(MAAT_ssp585$V12)+STDcumsum585))

# Plot 1
plot(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V12), axes=FALSE, xlab="", ylab="", main="MAAT in emerging overdeepening area", lwd=1.7, xlim=c(-20,20),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp119
StPos<-(cumsum(MAAT_ssp119$V12)+STDcumsum119)
StNeg<-(cumsum(MAAT_ssp119$V12)-STDcumsum119)
# Polygone de la StDev ssp119
polygon(x=c(MAAT_ssp126$V1,rev(MAAT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0.5,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(MAAT_ssp585$V12)+STDcumsum585)
StNeg<-(cumsum(MAAT_ssp585$V12)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(MAAT_ssp585$V1,rev(MAAT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Plot 1
# Data MAAT 2100
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V4),col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V6),col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V8),col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V10),col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,cumsum(MAAT_ssp119$V12),col="dodgerblue",lwd=1.7)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V4),col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V6),col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V8),col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V10),col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,cumsum(MAAT_ssp585$V12),col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP119", "SSP585", "2020", "2040", "2060","2080","2100"), col=c("dodgerblue","firebrick4","black","black","black","black","black"), lwd=c(1,1,0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(-20,-20),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(20,20),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

########-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####MAAT overdeepening SSP119 & 585 SUM %####

png(paste(chemin_figures,r,"/",r,"_MAAT_overdeepening_area_SSP119-585_SUM_Percentage_2100-2000.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(MAAT_ssp119$V1,(cumsum(MAAT_ssp119$V12))/(max(cumsum(MAAT_ssp119$V12))*100), axes=FALSE, main="MAAT in emerging overdeepening area", xlab="", ylab="", lwd=1.7, xlim=c(-20,20),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")

axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("MAAT (degreeC)",side=1, line=2.5)
mtext("cumulated area (in %)",side=2, line=2.5)
lines(x=c(0,0),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))
lines(x=c(-1000000,1000000),y=c(50,50),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data MAAT 
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V4))/(max(cumsum(MAAT_ssp119$V4))),col="dodgerblue",lwd=0.9)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V6))/(max(cumsum(MAAT_ssp119$V6))),col="dodgerblue",lwd=1.1)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V8))/(max(cumsum(MAAT_ssp119$V8))),col="dodgerblue",lwd=1.3)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V10))/(max(cumsum(MAAT_ssp119$V10))),col="dodgerblue",lwd=1.5)
lines(MAAT_ssp119$V1,100*(cumsum(MAAT_ssp119$V12))/(max(cumsum(MAAT_ssp119$V12))),col="dodgerblue",lwd=1.7)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V4))/(max(cumsum(MAAT_ssp585$V4))),col="firebrick4",lwd=0.9)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V6))/(max(cumsum(MAAT_ssp585$V6))),col="firebrick4",lwd=1.1)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V8))/(max(cumsum(MAAT_ssp585$V8))),col="firebrick4",lwd=1.3)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V10))/(max(cumsum(MAAT_ssp585$V10))),col="firebrick4",lwd=1.5)
lines(MAAT_ssp585$V1,100*(cumsum(MAAT_ssp585$V12))/(max(cumsum(MAAT_ssp585$V12))),col="firebrick4",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP119", "SSP585", "2020", "2040", "2060","2080","2100"), col=c("dodgerblue","firebrick4","black","black","black","black","black"), lwd=c(1,1,0.9,1.1,1.3,1.5,1.7), cex=0.5, bg=mycol)

dev.off()





####///DEPTH overdeepening AREA####
##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening area Depth####

png(paste(chemin_figures,r,"/",r,"_Depth_overdeepening_area.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Lecture fichier qui contient les data
for (s in  Scenario){  
  assign(paste("DPT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/lakearea_depth_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_DPT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_lakearea_depth_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 


Ymax_abs585<-(max(DPT_ssp585$V12-DPT_ssp585$V4+STD_DPT_ssp585$V12))
Ymax_abs126<-(max(DPT_ssp126$V12-DPT_ssp126$V4+STD_DPT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(DPT_ssp126$V1,DPT_ssp126$V12-DPT_ssp126$V4, axes=FALSE, main="Area by depth in emerging overdeepening", xlab="", ylab="", lwd=1.7, xlim=c(0,500),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("depth (m)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(5,5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(DPT_ssp126$V12-DPT_ssp126$V4+STD_DPT_ssp126$V12)
StNeg<-(DPT_ssp126$V12-DPT_ssp126$V4-STD_DPT_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(DPT_ssp126$V1,rev(DPT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(DPT_ssp585$V12-DPT_ssp585$V4+STD_DPT_ssp585$V12)
StNeg<-(DPT_ssp585$V12-DPT_ssp585$V4-STD_DPT_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(DPT_ssp585$V1,rev(DPT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data DPT 2050
lines(DPT_ssp585$V1,DPT_ssp585$V7-DPT_ssp585$V4,col="firebrick4",lty=2, lwd=1)
lines(DPT_ssp370$V1,DPT_ssp370$V7-DPT_ssp370$V4,col="red",lty=2, lwd=1)
lines(DPT_ssp245$V1,DPT_ssp245$V7-DPT_ssp245$V4,col="orange",lty=2, lwd=1)
lines(DPT_ssp126$V1,DPT_ssp126$V7-DPT_ssp126$V4,col="darkblue",lty=2, lwd=1)
lines(DPT_ssp119$V1,DPT_ssp119$V7-DPT_ssp119$V4,col="dodgerblue",lty=2, lwd=1)

# Data DPT 2100
lines(DPT_ssp585$V1,DPT_ssp585$V12-DPT_ssp585$V4,col="firebrick4",lwd=1.7)
lines(DPT_ssp370$V1,DPT_ssp370$V12-DPT_ssp370$V4,col="red",lwd=1.7)
lines(DPT_ssp245$V1,DPT_ssp245$V12-DPT_ssp245$V4,col="orange",lwd=1.7)
lines(DPT_ssp126$V1,DPT_ssp126$V12-DPT_ssp126$V4,col="darkblue",lwd=1.7)
lines(DPT_ssp119$V1,DPT_ssp119$V12-DPT_ssp119$V4,col="dodgerblue",lwd=1.7)

legend("topright", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(500,500),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening area Depth SMOOTH####

png(paste(chemin_figures,r,"/",r,"_Depth_overdeepening_area_smooth.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

#choisir de combien est la moyenne roulante (k)
kk<-3

# affichage graph double Y
par(mar=c(4,4,2,2))

# Lecture fichier qui contient les data
for (s in  Scenario){  
  assign(paste("DPT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/lakearea_depth_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_DPT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_lakearea_depth_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 


Ymax_abs585<-(max(DPT_ssp585$V12-DPT_ssp585$V4+STD_DPT_ssp585$V12))
Ymax_abs126<-(max(DPT_ssp126$V12-DPT_ssp126$V4+STD_DPT_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(DPT_ssp126$V1,DPT_ssp126$V12-DPT_ssp126$V4, axes=FALSE, main="Area by depth in emerging overdeepening", xlab="", ylab="", lwd=1.7, xlim=c(0,500),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="n",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("depth (m)",side=1, line=2.5)
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(5,5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

c(DPT_ssp126$V12-DPT_ssp126$V4+STD_DPT_ssp126$V12)[c(1:3)]

# Cordonn??es X de StDev ssp126
StPos<-c((DPT_ssp126$V12-DPT_ssp126$V4+STD_DPT_ssp126$V12)[c(1:(kk-1))],rollmean((DPT_ssp126$V12-DPT_ssp126$V4+STD_DPT_ssp126$V12),k=kk,na.pad = F,align="right"))
StNeg<-c((DPT_ssp126$V12-DPT_ssp126$V4-STD_DPT_ssp126$V12)[c(1:(kk-1))],rollmean((DPT_ssp126$V12-DPT_ssp126$V4-STD_DPT_ssp126$V12),k=kk,na.pad = F,align="right"))
# Polygone de la StDev ssp126
polygon(x=c(DPT_ssp126$V1,rev(DPT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)
rep(NA,kk-2)
# Cordonn??es X de StDev ssp585
StPos<-c((DPT_ssp585$V12-DPT_ssp585$V4+STD_DPT_ssp585$V12)[c(1:(kk-1))],rollmean((DPT_ssp585$V12-DPT_ssp585$V4+STD_DPT_ssp585$V12),k=kk,na.pad = F,align="right"))
StNeg<-c((DPT_ssp585$V12-DPT_ssp585$V4-STD_DPT_ssp585$V12)[c(1:(kk-1))],rollmean((DPT_ssp585$V12-DPT_ssp585$V4-STD_DPT_ssp585$V12),k=kk,na.pad = F,align="right"))
# Polygone de la StDev ssp585
polygon(x=c(DPT_ssp585$V1,rev(DPT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data DPT 2050
lines(DPT_ssp585$V1,c((DPT_ssp585$V7-DPT_ssp585$V4)[c(1:(kk-1))],rollmean(DPT_ssp585$V7-DPT_ssp585$V4,k=kk,na.pad = F,align="right")),col="firebrick4",lty=2, lwd=1)
lines(DPT_ssp370$V1,c((DPT_ssp370$V7-DPT_ssp370$V4)[c(1:(kk-1))],rollmean(DPT_ssp370$V7-DPT_ssp370$V4,k=kk,na.pad = F,align="right")),col="red",lty=2, lwd=1)
lines(DPT_ssp245$V1,c((DPT_ssp245$V7-DPT_ssp245$V4)[c(1:(kk-1))],rollmean(DPT_ssp245$V7-DPT_ssp245$V4,k=kk,na.pad = F,align="right")),col="orange",lty=2, lwd=1)
lines(DPT_ssp126$V1,c((DPT_ssp126$V7-DPT_ssp126$V4)[c(1:(kk-1))],rollmean(DPT_ssp126$V7-DPT_ssp126$V4,k=kk,na.pad = F,align="right")),col="darkblue",lty=2, lwd=1)
lines(DPT_ssp119$V1,c((DPT_ssp119$V7-DPT_ssp119$V4)[c(1:(kk-1))],rollmean(DPT_ssp119$V7-DPT_ssp119$V4,k=kk,na.pad = F,align="right")),col="dodgerblue",lty=2, lwd=1)

# Data DPT 2100
lines(DPT_ssp585$V1,c((DPT_ssp585$V12-DPT_ssp585$V4)[c(1:(kk-1))],rollmean(DPT_ssp585$V12-DPT_ssp585$V4,k=kk,na.pad = F,align="right")),col="firebrick4",lty=1, lwd=1.7)
lines(DPT_ssp370$V1,c((DPT_ssp370$V12-DPT_ssp370$V4)[c(1:(kk-1))],rollmean(DPT_ssp370$V12-DPT_ssp370$V4,k=kk,na.pad = F,align="right")),col="red",lty=1, lwd=1.7)
lines(DPT_ssp245$V1,c((DPT_ssp245$V12-DPT_ssp245$V4)[c(1:(kk-1))],rollmean(DPT_ssp245$V12-DPT_ssp245$V4,k=kk,na.pad = F,align="right")),col="orange",lty=1, lwd=1.7)
lines(DPT_ssp126$V1,c((DPT_ssp126$V12-DPT_ssp126$V4)[c(1:(kk-1))],rollmean(DPT_ssp126$V12-DPT_ssp126$V4,k=kk,na.pad = F,align="right")),col="darkblue",lty=1, lwd=1.7)
lines(DPT_ssp119$V1,c((DPT_ssp119$V12-DPT_ssp119$V4)[c(1:(kk-1))],rollmean(DPT_ssp119$V12-DPT_ssp119$V4,k=kk,na.pad = F,align="right")),col="dodgerblue",lty=1, lwd=1.7)

legend("topright", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(500,500),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening area Depth SUM####

png(paste(chemin_figures,r,"/",r,"_Depth_overdeepening_area_SUM.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum585<-STD_DPT_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum126<-STD_DPT_ssp126 $V12^2
STDcumsum126<-sqrt(cumsum(Varcumsum126))
Ymax_abs<-(max(cumsum(DPT_ssp585$V12)-cumsum(DPT_ssp585$V4)+STDcumsum585))

# Plot 1
plot(DPT_ssp126$V1,cumsum(DPT_ssp126$V12)-cumsum(DPT_ssp126$V4), axes=FALSE, main="Area by depth in emerging overdeepening", xlab="", ylab="", lwd=1.7, xlim=c(0,500),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("depth (m)",side=1, line=2.5)
mtext(expression("cumulated area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(5,5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Cordonn??es X de StDev ssp126
StPos<-(cumsum(DPT_ssp126$V12)-cumsum(DPT_ssp126$V4)+STDcumsum126)
StNeg<-(cumsum(DPT_ssp126$V12)-cumsum(DPT_ssp126$V4)-STDcumsum126)
# Polygone de la StDev ssp126
polygon(x=c(DPT_ssp126$V1,rev(DPT_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(DPT_ssp585$V12)-cumsum(DPT_ssp585$V4)+STDcumsum585)
StNeg<-(cumsum(DPT_ssp585$V12)-cumsum(DPT_ssp585$V4)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(DPT_ssp585$V1,rev(DPT_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data DPTpe 2100
lines(DPT_ssp585$V1,cumsum(DPT_ssp585$V12)-cumsum(DPT_ssp585$V4),col="firebrick4",lwd=1.7)
lines(DPT_ssp370$V1,cumsum(DPT_ssp370$V12)-cumsum(DPT_ssp370$V4),col="red",lwd=1.7)
lines(DPT_ssp245$V1,cumsum(DPT_ssp245$V12)-cumsum(DPT_ssp245$V4),col="orange",lwd=1.7)
lines(DPT_ssp126$V1,cumsum(DPT_ssp126$V12)-cumsum(DPT_ssp126$V4),col="darkblue",lwd=1.7)
lines(DPT_ssp119$V1,cumsum(DPT_ssp119$V12)-cumsum(DPT_ssp119$V4),col="dodgerblue",lwd=1.7)

# Data DPTpe 2050
lines(DPT_ssp585$V1,cumsum(DPT_ssp585$V7)-cumsum(DPT_ssp585$V4),col="firebrick4",lty=2, lwd=1)
lines(DPT_ssp370$V1,cumsum(DPT_ssp370$V7)-cumsum(DPT_ssp370$V4),col="red",lty=2, lwd=1)
lines(DPT_ssp245$V1,cumsum(DPT_ssp245$V7)-cumsum(DPT_ssp245$V4),col="orange",lty=2, lwd=1)
lines(DPT_ssp126$V1,cumsum(DPT_ssp126$V7)-cumsum(DPT_ssp126$V4),col="darkblue",lty=2, lwd=1)
lines(DPT_ssp119$V1,cumsum(DPT_ssp119$V7)-cumsum(DPT_ssp119$V4),col="dodgerblue",lty=2, lwd=1)

legend("bottom", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(500,500),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening area Depth SUM %####

png(paste(chemin_figures,r,"/",r,"_Depth_overdeepening_area_SUM_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(DPT_ssp126$V1,(cumsum(DPT_ssp126$V12)-cumsum(DPT_ssp126$V4))/(sum(DPT_ssp126$V12)-sum(DPT_ssp126$V4))*100, axes=FALSE, main="Area by depth in emerging overdeepening",xlab="", ylab="", lwd=1.7, xlim=c(0,500),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("depth (m)",side=1, line=2.5)
mtext("cumulated area (%)",side=2, line=2.5)
lines(x=c(5,5),y=c(-1000000,1000000),col=rgb(0.5,0.5,0.5,alpha=0.3))

# Data DPTpe 2100
lines(DPT_ssp585$V1,(cumsum(DPT_ssp585$V12)-cumsum(DPT_ssp585$V4))/(sum(DPT_ssp585$V12)-sum(DPT_ssp585$V4))*100,col="firebrick4",lwd=1.7)
lines(DPT_ssp370$V1,(cumsum(DPT_ssp370$V12)-cumsum(DPT_ssp370$V4))/(sum(DPT_ssp370$V12)-sum(DPT_ssp370$V4))*100,col="red",lwd=1.7)
lines(DPT_ssp245$V1,(cumsum(DPT_ssp245$V12)-cumsum(DPT_ssp245$V4))/(sum(DPT_ssp245$V12)-sum(DPT_ssp245$V4))*100,col="orange",lwd=1.7)
lines(DPT_ssp126$V1,(cumsum(DPT_ssp126$V12)-cumsum(DPT_ssp126$V4))/(sum(DPT_ssp126$V12)-sum(DPT_ssp126$V4))*100,col="darkblue",lwd=1.7)
lines(DPT_ssp119$V1,(cumsum(DPT_ssp119$V12)-cumsum(DPT_ssp119$V4))/(sum(DPT_ssp119$V12)-sum(DPT_ssp119$V4))*100,col="dodgerblue",lwd=1.7)

# Data DPTpe 2050
lines(DPT_ssp585$V1,(cumsum(DPT_ssp585$V7)-cumsum(DPT_ssp585$V4))/(sum(DPT_ssp585$V7)-sum(DPT_ssp585$V4))*100,col="firebrick4",lty=2, lwd=1)
lines(DPT_ssp370$V1,(cumsum(DPT_ssp370$V7)-cumsum(DPT_ssp370$V4))/(sum(DPT_ssp370$V7)-sum(DPT_ssp370$V4))*100,col="red",lty=2, lwd=1)
lines(DPT_ssp245$V1,(cumsum(DPT_ssp245$V7)-cumsum(DPT_ssp245$V4))/(sum(DPT_ssp245$V7)-sum(DPT_ssp245$V4))*100,col="orange",lty=2, lwd=1)
lines(DPT_ssp126$V1,(cumsum(DPT_ssp126$V7)-cumsum(DPT_ssp126$V4))/(sum(DPT_ssp126$V7)-sum(DPT_ssp126$V4))*100,col="darkblue",lty=2, lwd=1)
lines(DPT_ssp119$V1,(cumsum(DPT_ssp119$V7)-cumsum(DPT_ssp119$V4))/(sum(DPT_ssp119$V7)-sum(DPT_ssp119$V4))*100,col="dodgerblue",lty=2, lwd=1)

legend("bottom", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

dev.off()



####///overdeepening N Area####
###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening N area####

png(paste(chemin_figures,r,"/",r,"_N_overdeepening_by_area.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Lecture fichier qui contient les data
for (s in  Scenario){  
  assign(paste("LN_Area_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/lakenumber_lakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_LN_Area_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_lakenumber_lakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 

Ymax_abs585<-(max(LN_Area_ssp585$V12-LN_Area_ssp585$V4+STD_LN_Area_ssp585$V12))
Ymax_abs126<-(max(LN_Area_ssp126$V12-LN_Area_ssp126$V4+STD_LN_Area_ssp126$V12))
Ymax_abs<-(max(Ymax_abs126,Ymax_abs585))

# Plot 1
plot(LN_Area_ssp126$V1,LN_Area_ssp126$V12-LN_Area_ssp126$V4, axes=FALSE, main="Emerging overdeepening (n) by area",xlab="", ylab="", lwd=1.7, xlim=c(0,2),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=1, line=2.5)
mtext("overdeepening (n)",side=2, line=2.5)

# Cordonn??es X de StDev ssp126
StPos<-(LN_Area_ssp126$V12-LN_Area_ssp126$V4+STD_LN_Area_ssp126$V12)
StNeg<-(LN_Area_ssp126$V12-LN_Area_ssp126$V4-STD_LN_Area_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(LN_Area_ssp126$V1,rev(LN_Area_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(LN_Area_ssp585$V12-LN_Area_ssp585$V4+STD_LN_Area_ssp585$V12)
StNeg<-(LN_Area_ssp585$V12-LN_Area_ssp585$V4-STD_LN_Area_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(LN_Area_ssp585$V1,rev(LN_Area_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data LN_Area 2050
lines(LN_Area_ssp585$V1,LN_Area_ssp585$V7-LN_Area_ssp585$V4,col="firebrick4",lty=2, lwd=1)
lines(LN_Area_ssp370$V1,LN_Area_ssp370$V7-LN_Area_ssp370$V4,col="red",lty=2, lwd=1)
lines(LN_Area_ssp245$V1,LN_Area_ssp245$V7-LN_Area_ssp245$V4,col="orange",lty=2, lwd=1)
lines(LN_Area_ssp126$V1,LN_Area_ssp126$V7-LN_Area_ssp126$V4,col="darkblue",lty=2, lwd=1)
lines(LN_Area_ssp119$V1,LN_Area_ssp119$V7-LN_Area_ssp119$V4,col="dodgerblue",lty=2, lwd=1)

# Data LN_Area 2100
lines(LN_Area_ssp585$V1,LN_Area_ssp585$V12-LN_Area_ssp585$V4,col="firebrick4",lwd=1.7)
lines(LN_Area_ssp370$V1,LN_Area_ssp370$V12-LN_Area_ssp370$V4,col="red",lwd=1.7)
lines(LN_Area_ssp245$V1,LN_Area_ssp245$V12-LN_Area_ssp245$V4,col="orange",lwd=1.7)
lines(LN_Area_ssp126$V1,LN_Area_ssp126$V12-LN_Area_ssp126$V4,col="darkblue",lwd=1.7)
lines(LN_Area_ssp119$V1,LN_Area_ssp119$V12-LN_Area_ssp119$V4,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2,2),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening N area SUM####

png(paste(chemin_figures,r,"/",r,"_N_overdeepening_by_area_SUM.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

Varcumsum585<-STD_LN_Area_ssp585$V12^2
STDcumsum585<-sqrt(cumsum(Varcumsum585))
Varcumsum126<-STD_LN_Area_ssp126$V12^2
STDcumsum126<-sqrt(cumsum(Varcumsum126))
Ymax_abs<-(max(cumsum(LN_Area_ssp585$V12)-cumsum(LN_Area_ssp585$V4)+STDcumsum585))

# Plot 1
plot(LN_Area_ssp126$V1,cumsum(LN_Area_ssp126$V12)-cumsum(LN_Area_ssp126$V4), main="Emerging overdeepening (n) by area",axes=FALSE, xlab="", ylab="", lwd=1.7, xlim=c(0,2),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=1, line=2.5)
mtext("cumulated overdeepening (n)",side=2, line=2.5)

# Cordonn??es X de StDev ssp126
StPos<-(cumsum(LN_Area_ssp126$V12)-cumsum(LN_Area_ssp126$V4)+STDcumsum126)
StNeg<-(cumsum(LN_Area_ssp126$V12)-cumsum(LN_Area_ssp126$V4)-STDcumsum126)
# Polygone de la StDev ssp126
polygon(x=c(LN_Area_ssp126$V1,rev(LN_Area_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es X de StDev ssp585
StPos<-(cumsum(LN_Area_ssp585$V12)-cumsum(LN_Area_ssp585$V4)+STDcumsum585)
StNeg<-(cumsum(LN_Area_ssp585$V12)-cumsum(LN_Area_ssp585$V4)-STDcumsum585)
# Polygone de la StDev ssp585
polygon(x=c(LN_Area_ssp585$V1,rev(LN_Area_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data LN_Areape 2100
lines(LN_Area_ssp585$V1,cumsum(LN_Area_ssp585$V12)-cumsum(LN_Area_ssp585$V4),col="firebrick4",lwd=1.7)
lines(LN_Area_ssp370$V1,cumsum(LN_Area_ssp370$V12)-cumsum(LN_Area_ssp370$V4),col="red",lwd=1.7)
lines(LN_Area_ssp245$V1,cumsum(LN_Area_ssp245$V12)-cumsum(LN_Area_ssp245$V4),col="orange",lwd=1.7)
lines(LN_Area_ssp126$V1,cumsum(LN_Area_ssp126$V12)-cumsum(LN_Area_ssp126$V4),col="darkblue",lwd=1.7)
lines(LN_Area_ssp119$V1,cumsum(LN_Area_ssp119$V12)-cumsum(LN_Area_ssp119$V4),col="dodgerblue",lwd=1.7)

# Data LN_Areape 2050
lines(LN_Area_ssp585$V1,cumsum(LN_Area_ssp585$V7)-cumsum(LN_Area_ssp585$V4),col="firebrick4",lty=2, lwd=1)
lines(LN_Area_ssp370$V1,cumsum(LN_Area_ssp370$V7)-cumsum(LN_Area_ssp370$V4),col="red",lty=2, lwd=1)
lines(LN_Area_ssp245$V1,cumsum(LN_Area_ssp245$V7)-cumsum(LN_Area_ssp245$V4),col="orange",lty=2, lwd=1)
lines(LN_Area_ssp126$V1,cumsum(LN_Area_ssp126$V7)-cumsum(LN_Area_ssp126$V4),col="darkblue",lty=2, lwd=1)
lines(LN_Area_ssp119$V1,cumsum(LN_Area_ssp119$V7)-cumsum(LN_Area_ssp119$V4),col="dodgerblue",lty=2, lwd=1)

legend("bottom", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2,2),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()


##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####overdeepening N area SUM %####

png(paste(chemin_figures,r,"/",r,"_N_overdeepening_by_area_SUM_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1
plot(LN_Area_ssp126$V1,(cumsum(LN_Area_ssp126$V12)-cumsum(LN_Area_ssp126$V4))/(sum(LN_Area_ssp126$V12)-sum(LN_Area_ssp126$V4))*100, main="Emerging overdeepening (n) by area",axes=FALSE, xlab="", ylab="", lwd=1.7, xlim=c(0,2),ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=1, line=2.5)
mtext("cumulated overdeepening (%)",side=2, line=2.5)

# Data LN_Areape 2100
lines(LN_Area_ssp585$V1,(cumsum(LN_Area_ssp585$V12)-cumsum(LN_Area_ssp585$V4))/(sum(LN_Area_ssp585$V12)-sum(LN_Area_ssp585$V4))*100,col="firebrick4",lwd=1.7)
lines(LN_Area_ssp370$V1,(cumsum(LN_Area_ssp370$V12)-cumsum(LN_Area_ssp370$V4))/(sum(LN_Area_ssp370$V12)-sum(LN_Area_ssp370$V4))*100,col="red",lwd=1.7)
lines(LN_Area_ssp245$V1,(cumsum(LN_Area_ssp245$V12)-cumsum(LN_Area_ssp245$V4))/(sum(LN_Area_ssp245$V12)-sum(LN_Area_ssp245$V4))*100,col="orange",lwd=1.7)
lines(LN_Area_ssp126$V1,(cumsum(LN_Area_ssp126$V12)-cumsum(LN_Area_ssp126$V4))/(sum(LN_Area_ssp126$V12)-sum(LN_Area_ssp126$V4))*100,col="darkblue",lwd=1.7)
lines(LN_Area_ssp119$V1,(cumsum(LN_Area_ssp119$V12)-cumsum(LN_Area_ssp119$V4))/(sum(LN_Area_ssp119$V12)-sum(LN_Area_ssp119$V4))*100,col="dodgerblue",lwd=1.7)

# Data LN_Areape 2050
lines(LN_Area_ssp585$V1,(cumsum(LN_Area_ssp585$V7)-cumsum(LN_Area_ssp585$V4))/(sum(LN_Area_ssp585$V7)-sum(LN_Area_ssp585$V4))*100,col="firebrick4",lty=2, lwd=1)
lines(LN_Area_ssp370$V1,(cumsum(LN_Area_ssp370$V7)-cumsum(LN_Area_ssp370$V4))/(sum(LN_Area_ssp370$V7)-sum(LN_Area_ssp370$V4))*100,col="red",lty=2, lwd=1)
lines(LN_Area_ssp245$V1,(cumsum(LN_Area_ssp245$V7)-cumsum(LN_Area_ssp245$V4))/(sum(LN_Area_ssp245$V7)-sum(LN_Area_ssp245$V4))*100,col="orange",lty=2, lwd=1)
lines(LN_Area_ssp126$V1,(cumsum(LN_Area_ssp126$V7)-cumsum(LN_Area_ssp126$V4))/(sum(LN_Area_ssp126$V7)-sum(LN_Area_ssp126$V4))*100,col="darkblue",lty=2, lwd=1)
lines(LN_Area_ssp119$V1,(cumsum(LN_Area_ssp119$V7)-cumsum(LN_Area_ssp119$V4))/(sum(LN_Area_ssp119$V7)-sum(LN_Area_ssp119$V4))*100,col="dodgerblue",lty=2, lwd=1)

legend("bottom", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126","SSP119","2050","2100"), col=c("firebrick4", "red", "orange", "darkblue","dodgerblue","black","black"), lty=c(1,1,1,1,1,2,1), cex=0.5, bg=mycol)

lines(x=c(0,0),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2,2),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()


####///HABITAT // EXPLORATORY LAND ECOLOGY####
#######------------------------------------------------------------------------------------------------------------
####Habitat/Cold&LowSlope####

png(paste(chemin_figures,r,"/",r,"_Habitat_Land_Cold&LowSlope.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Lecture fichier qui contient les data

for (s in  Scenario){  
  assign(paste("hab_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/habitat_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_hab_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_habitat_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 

Habitat_Land_Cold_LowSlope<-c( "Habitat_Land_Cold&LowSlope",r,
                               hab_ssp119[Res_ssp119$V1=="2020",]$V2, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V2,hab_ssp119[Res_ssp119$V1=="2050",]$V2, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V2,hab_ssp119[Res_ssp119$V1=="2100",]$V2, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V2 ,
                               hab_ssp585[Res_ssp585$V1=="2020",]$V2, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V2,hab_ssp585[Res_ssp585$V1=="2050",]$V2, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V2,hab_ssp585[Res_ssp585$V1=="2100",]$V2, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V2 ,
                               100*hab_ssp119[Res_ssp119$V1=="2100",]$V2/land_area119[Res_ssp119$V1=="2100"],
                               100*hab_ssp585[Res_ssp585$V1=="2100",]$V2/land_area585[Res_ssp585$V1=="2100"],
                               NA)
table_summary<-rbind(table_summary,Habitat_Land_Cold_LowSlope,stringsAsFactors = FALSE)


Ymax_absH1<-(max(hab_ssp585$V2+STD_hab_ssp585$V2))
Ymax_absH2<-(max(hab_ssp585$V3+STD_hab_ssp585$V3))
Ymax_absH3<-(max(hab_ssp585$V4+STD_hab_ssp585$V4))
Ymax_absH4<-(max(hab_ssp585$V5+STD_hab_ssp585$V5))
Ymax_abs<-(max(Ymax_absH1,Ymax_absH2,Ymax_absH3,Ymax_absH4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V2, axes=FALSE, main= expression("Emerging terrestrial habitat (MAAT < 0"*~degree*C*" and slope < 30"*~degree*C*")"), cex.main=0.8, xlab="", ylab="", lwd=1.7, xlim=c(2000,2100),ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V2+STD_hab_ssp126$V2)
StNeg<-(hab_ssp126$V2-STD_hab_ssp126$V2)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V2+STD_hab_ssp585$V2)
StNeg<-(hab_ssp585$V2-STD_hab_ssp585$V2)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V2,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V2,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V2,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V2,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V2,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)
# box.lty=0

Ymax_relH1<-(max(hab_ssp585$V2+STD_hab_ssp585$V2)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH2<-(max(hab_ssp585$V3+STD_hab_ssp585$V3)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH3<-(max(hab_ssp585$V4+STD_hab_ssp585$V4)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH4<-(max(hab_ssp585$V5+STD_hab_ssp585$V5)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_rel<-(max(Ymax_relH1,Ymax_relH2,Ymax_relH3,Ymax_relH4))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V2/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/Temperate&LowSlope####

png(paste(chemin_figures,r,"/",r,"_Habitat_Land_Temperate&LowSlope.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habitat_Land_Temperate_LowSlope<-c( "Habitat_Land_Temperate&LowSlope",r,
                               hab_ssp119[Res_ssp119$V1=="2020",]$V3, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V3,hab_ssp119[Res_ssp119$V1=="2050",]$V3, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V3,hab_ssp119[Res_ssp119$V1=="2100",]$V3, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V3 ,
                               hab_ssp585[Res_ssp585$V1=="2020",]$V3, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V3,hab_ssp585[Res_ssp585$V1=="2050",]$V3, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V3,hab_ssp585[Res_ssp585$V1=="2100",]$V3, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V3 ,
                               100*hab_ssp119[Res_ssp119$V1=="2100",]$V3/land_area119[Res_ssp119$V1=="2100"],
                               100*hab_ssp585[Res_ssp585$V1=="2100",]$V3/land_area585[Res_ssp585$V1=="2100"],
                               NA)
table_summary<-rbind(table_summary,Habitat_Land_Temperate_LowSlope,stringsAsFactors = FALSE)


# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V3, axes=FALSE, xlab="", ylab="", main= expression("Emerging terrestrial habitat (MAAT > 0"*~degree*C*" and slope < 30"*~degree*C*")"), cex.main=0.8,  lwd=1.7, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V3+STD_hab_ssp126$V3)
StNeg<-(hab_ssp126$V3-STD_hab_ssp126$V3)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V3+STD_hab_ssp585$V3)
StNeg<-(hab_ssp585$V3-STD_hab_ssp585$V3)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V3,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V3,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V3,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V3,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V3,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)
# box.lty=0

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V2/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/Cold&SteepSlope####

png(paste(chemin_figures,r,"/",r,"_Habitat_Land_Cold&Steep.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habitat_Land_Cold_Steep<-c( "Habitat_Land_Cold&Steep",r,
                                    hab_ssp119[Res_ssp119$V1=="2020",]$V4, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V4,hab_ssp119[Res_ssp119$V1=="2050",]$V4, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V4,hab_ssp119[Res_ssp119$V1=="2100",]$V4, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V4 ,
                                    hab_ssp585[Res_ssp585$V1=="2020",]$V4, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V4,hab_ssp585[Res_ssp585$V1=="2050",]$V4, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V4,hab_ssp585[Res_ssp585$V1=="2100",]$V4, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V4 ,
                                    100*hab_ssp119[Res_ssp119$V1=="2100",]$V4/land_area119[Res_ssp119$V1=="2100"],
                                    100*hab_ssp585[Res_ssp585$V1=="2100",]$V4/land_area585[Res_ssp585$V1=="2100"],
                                    NA)
table_summary<-rbind(table_summary,Habitat_Land_Cold_Steep,stringsAsFactors = FALSE)



# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V4, axes=FALSE, xlab="", ylab="", lwd=1.7, main= expression("Emerging terrestrial habitat (MAAT < 0"*~degree*C*" and slope > 30"*~degree*C*")"), cex.main=0.8, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V4+STD_hab_ssp126$V4)
StNeg<-(hab_ssp126$V4-STD_hab_ssp126$V4)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V4+STD_hab_ssp585$V4)
StNeg<-(hab_ssp585$V4-STD_hab_ssp585$V4)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V4,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V4,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V4,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V4,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V4,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V2/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/Temperate&SteepSlope####

png(paste(chemin_figures,r,"/",r,"_Habitat_Land_Temperate&Steep.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habitat_Land_Temperate_Steep<-c( "Habitat_Land_Temperate&Steep",r,
                            hab_ssp119[Res_ssp119$V1=="2020",]$V5, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V5,hab_ssp119[Res_ssp119$V1=="2050",]$V5, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V5,hab_ssp119[Res_ssp119$V1=="2100",]$V5, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V5 ,
                            hab_ssp585[Res_ssp585$V1=="2020",]$V5, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V5,hab_ssp585[Res_ssp585$V1=="2050",]$V5, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V5,hab_ssp585[Res_ssp585$V1=="2100",]$V5, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V5 ,
                            100*hab_ssp119[Res_ssp119$V1=="2100",]$V5/land_area119[Res_ssp119$V1=="2100"],
                            100*hab_ssp585[Res_ssp585$V1=="2100",]$V5/land_area585[Res_ssp585$V1=="2100"],
                            NA)
table_summary<-rbind(table_summary,Habitat_Land_Temperate_Steep,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V5, axes=FALSE, xlab="", ylab="", lwd=1.7, main= expression("Emerging terrestrial habitat (MAAT > 0"*~degree*C*" and slope > 30"*~degree*C*")"), cex.main=0.8, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V5,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V5,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V5,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V5,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V5,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V2/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/Land SSP 119 & 585####

png(paste(chemin_figures,r,"/",r,"_Habitat_Land_SSP119-SSP585.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp119$V1,hab_ssp119$V2, axes=FALSE, xlab="", ylab="", main= "Emerging terrestrial habitat under SSP119 & 585", cex.main=0.8, lwd=0.7, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
#StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
#StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
#polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
#StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
#StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
#polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V2,col="blue",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V3,col="green",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V4,col="#081d58",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V5,col="darkgreen",lty=2, lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V2,col="blue",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V3,col="green",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V4,col="#081d58",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V5,col="darkgreen",lty=1, lwd=1.3)

legend("topleft", inset=.05, legend=c("SSP585 (dotted line)","SSP119 (solid line)","Cold&LowSlope","Temperate&LowSlope","Cold&SteepSlope","Temperate&SteepSlope"), col=c("black", "black","blue","green","#081d58","darkgreen"), lty=c(2,1,1,1,1,1), lwd=c(1,1,1,1,1,1), cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V2/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/Land SSP 119 & 585 %####

png(paste(chemin_figures,r,"/",r,"_Habitat_Land_SSP119-SSP585_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1a
plot(hab_ssp119$V1,hab_ssp119$V2/(hab_ssp119$V2+hab_ssp119$V3+hab_ssp119$V4+hab_ssp119$V5)*100, axes=FALSE, xlab="", ylab="", main= "Emerging terrestrial habitat under SSP119 & 585", cex.main=0.8,lwd=0.7, xlim=c(2020,2100), ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("Relative area within emerging land area (%)",side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
#StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
#StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
#polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
#StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
#StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
#polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V2/(hab_ssp585$V2+hab_ssp585$V3+hab_ssp585$V4+hab_ssp585$V5)*100,col="blue",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V3/(hab_ssp585$V2+hab_ssp585$V3+hab_ssp585$V4+hab_ssp585$V5)*100,col="green",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V4/(hab_ssp585$V2+hab_ssp585$V3+hab_ssp585$V4+hab_ssp585$V5)*100,col="#081d58",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V5/(hab_ssp585$V2+hab_ssp585$V3+hab_ssp585$V4+hab_ssp585$V5)*100,col="darkgreen",lty=2, lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V2/(hab_ssp119$V2+hab_ssp119$V3+hab_ssp119$V4+hab_ssp119$V5)*100,col="blue",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V3/(hab_ssp119$V2+hab_ssp119$V3+hab_ssp119$V4+hab_ssp119$V5)*100,col="green",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V4/(hab_ssp119$V2+hab_ssp119$V3+hab_ssp119$V4+hab_ssp119$V5)*100,col="#081d58",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V5/(hab_ssp119$V2+hab_ssp119$V3+hab_ssp119$V4+hab_ssp119$V5)*100,col="darkgreen",lty=1, lwd=1.3)

legend("topleft", inset=.05, legend=c("SSP585 (dotted line)","SSP119 (solid line)","Cold&LowSlope","Temperate&LowSlope","Cold&SteepSlope","Temperate&SteepSlope"), col=c("black", "black","blue","green","#081d58","darkgreen"), lty=c(2,1,1,1,1,1), lwd=c(1,1,1,1,1,1), cex=0.55, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

############-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
####///HABITAT // EXPLORATORY FRESHWATER ECOLOGY####
###########-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/FRESHWATER/Cold&Shallow####

# affichage graph double Y
par(mar=c(4,4,2,4))

# Lecture fichier qui contient les data
for (s in  Scenario){  
  assign(paste("hab_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/habitat_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_hablo_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_habitat_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 

png(paste(chemin_figures,r,"/",r,"_Habita_Freshwater_Cold&ShallowArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habita_Freshwater_Cold_ShallowArea<-c( "Habita_Freshwater_Cold&ShallowArea",r,
                                       hab_ssp119[Res_ssp119$V1=="2020",]$V6, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V6,hab_ssp119[Res_ssp119$V1=="2050",]$V6, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V6,hab_ssp119[Res_ssp119$V1=="2100",]$V6, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V6 ,
                                       hab_ssp585[Res_ssp585$V1=="2020",]$V6, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V6,hab_ssp585[Res_ssp585$V1=="2050",]$V6, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V6,hab_ssp585[Res_ssp585$V1=="2100",]$V6, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V6 ,
                                       100*hab_ssp119[Res_ssp119$V1=="2100",]$V6/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                       100*hab_ssp585[Res_ssp585$V1=="2100",]$V6/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                       NA)
table_summary<-rbind(table_summary,Habita_Freshwater_Cold_ShallowArea,stringsAsFactors = FALSE)

Ymax_absH1<-(max(hab_ssp585$V6+STD_hab_ssp585$V6))
Ymax_absH2<-(max(hab_ssp585$V7+STD_hab_ssp585$V7))
Ymax_absH3<-(max(hab_ssp585$V8+STD_hab_ssp585$V8))
Ymax_absH4<-(max(hab_ssp585$V9+STD_hab_ssp585$V9))
Ymax_abs<-(max(Ymax_absH1,Ymax_absH2,Ymax_absH3,Ymax_absH4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V6, axes=FALSE, xlab="", ylab="", main= expression("Emerging freshwater habitat (MAAT < 0"*~degree*C*" and depth < 5m)"), cex.main=0.8, lwd=1.7, xlim=c(2000,2100) ,ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V6+STD_hab_ssp126$V6)
StNeg<-(hab_ssp126$V6-STD_hab_ssp126$V6)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V6+STD_hab_ssp585$V6)
StNeg<-(hab_ssp585$V6-STD_hab_ssp585$V6)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V6,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V6,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V6,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V6,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V6,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

Ymax_relH1<-(max(hab_ssp585$V6+STD_hab_ssp585$V6)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH2<-(max(hab_ssp585$V7+STD_hab_ssp585$V7)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH3<-(max(hab_ssp585$V8+STD_hab_ssp585$V8)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH4<-(max(hab_ssp585$V9+STD_hab_ssp585$V9)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_rel<-(max(Ymax_relH1,Ymax_relH2,Ymax_relH3,Ymax_relH4))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V6/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

########-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/FRESHWATER/Temperate&Shallow####

png(paste(chemin_figures,r,"/",r,"_Habita_Freshwater_Temperate&ShallowArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habita_Freshwater_Temperate_ShallowArea<-c( "Habita_Freshwater_Temperate&ShallowArea",r,
                                       hab_ssp119[Res_ssp119$V1=="2020",]$V7, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V7,hab_ssp119[Res_ssp119$V1=="2050",]$V7, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V7,hab_ssp119[Res_ssp119$V1=="2100",]$V7, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V7 ,
                                       hab_ssp585[Res_ssp585$V1=="2020",]$V7, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V7,hab_ssp585[Res_ssp585$V1=="2050",]$V7, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V7,hab_ssp585[Res_ssp585$V1=="2100",]$V7, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V7 ,
                                       100*hab_ssp119[Res_ssp119$V1=="2100",]$V7/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                       100*hab_ssp585[Res_ssp585$V1=="2100",]$V7/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                       NA)
table_summary<-rbind(table_summary,Habita_Freshwater_Temperate_ShallowArea,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V7, axes=FALSE, xlab="", ylab="", lwd=1.7, main= expression("Emerging freshwater habitat (MAAT > 0"*~degree*C*" and depth < 5m)"), cex.main=0.8, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V7+STD_hab_ssp126$V7)
StNeg<-(hab_ssp126$V7-STD_hab_ssp126$V7)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V7+STD_hab_ssp585$V7)
StNeg<-(hab_ssp585$V7-STD_hab_ssp585$V7)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V7,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V7,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V7,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V7,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V7,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V6/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/FRESHWATER/Cold&Deep####

png(paste(chemin_figures,r,"/",r,"_Habita_Freshwater_Cold&DeepArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habita_Freshwater_Cold_DeepArea<-c( "Habita_Freshwater_Cold&DeepArea",r,
                                            hab_ssp119[Res_ssp119$V1=="2020",]$V8, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V8,hab_ssp119[Res_ssp119$V1=="2050",]$V8, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V8,hab_ssp119[Res_ssp119$V1=="2100",]$V8, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V8 ,
                                            hab_ssp585[Res_ssp585$V1=="2020",]$V8, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V8,hab_ssp585[Res_ssp585$V1=="2050",]$V8, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V8,hab_ssp585[Res_ssp585$V1=="2100",]$V8, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V8 ,
                                            100*hab_ssp119[Res_ssp119$V1=="2100",]$V8/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                            100*hab_ssp585[Res_ssp585$V1=="2100",]$V8/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                            NA)
table_summary<-rbind(table_summary,Habita_Freshwater_Cold_DeepArea,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V8, axes=FALSE, xlab="", ylab="", main= expression("Emerging freshwater habitat (MAAT < 0"*~degree*C*" and depth > 5m)"), cex.main=0.8, lwd=1.7, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V8+STD_hab_ssp126$V8)
StNeg<-(hab_ssp126$V8-STD_hab_ssp126$V8)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V8+STD_hab_ssp585$V8)
StNeg<-(hab_ssp585$V8-STD_hab_ssp585$V8)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V8,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V8,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V8,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V8,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V8,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V6/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/FRESHWATER/Temperate&Deep####

png(paste(chemin_figures,r,"/",r,"_Habita_Freshwater_Temperate&DeepArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)
Habita_Freshwater_Temperate_DeepArea<-c( "Habita_Freshwater_Temperate&DeepArea",r,
                                    hab_ssp119[Res_ssp119$V1=="2020",]$V9, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V9,hab_ssp119[Res_ssp119$V1=="2050",]$V9, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V9,hab_ssp119[Res_ssp119$V1=="2100",]$V9, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V9 ,
                                    hab_ssp585[Res_ssp585$V1=="2020",]$V9, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V9,hab_ssp585[Res_ssp585$V1=="2050",]$V9, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V9,hab_ssp585[Res_ssp585$V1=="2100",]$V9, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V9 ,
                                    100*hab_ssp119[Res_ssp119$V1=="2100",]$V9/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                    100*hab_ssp585[Res_ssp585$V1=="2100",]$V9/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                    NA)
table_summary<-rbind(table_summary,Habita_Freshwater_Temperate_DeepArea,stringsAsFactors = FALSE)


# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V9, axes=FALSE, xlab="", ylab="", main= expression("Emerging freshwater habitat (MAAT > 0"*~degree*C*" and depth > 5m)"), cex.main=0.8, lwd=1.7, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V9+STD_hab_ssp126$V9)
StNeg<-(hab_ssp126$V9-STD_hab_ssp126$V9)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V9+STD_hab_ssp585$V9)
StNeg<-(hab_ssp585$V9-STD_hab_ssp585$V9)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V9,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V9,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V9,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V9,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V9,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)
# box.lty=0

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V6/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/FRESHWATER/SSP119 & 585####

png(paste(chemin_figures,r,"/",r,"_Habita_Freshwater_SSP119-SSP585.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp119$V1,hab_ssp119$V6, axes=FALSE, xlab="", ylab="", lwd=0.7, main= "Emerging freshwater habitat under SSP119 & 585", cex.main=0.8, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
#StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
#StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
#polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
#StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
#StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
#polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V6,col="blue",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V7,col="green",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V8,col="#081d58",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V9,col="darkgreen",lty=2, lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V6,col="blue",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V7,col="green",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V8,col="#081d58",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V9,col="darkgreen",lty=1, lwd=1.3)

legend("topleft", inset=.05, legend=c("SSP585 (dotted line)","SSP119 (solid line)","Cold&Shallow","Temperate&Shallow","Cold&Deep","Temperate&Deep"), col=c("black", "black","blue","green","#081d58","darkgreen"), lty=c(2,1,1,1,1,1), lwd=c(1,1,1,1,1,1), cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V6/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/FRESHWATER SSP 119 & 585 %####

png(paste(chemin_figures,r,"/",r,"_Habita_Freshwater_SSP119-SSP585_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1a
plot(hab_ssp119$V1,hab_ssp119$V6/(hab_ssp119$V6+hab_ssp119$V7+hab_ssp119$V8+hab_ssp119$V9)*100, axes=FALSE, xlab="", ylab="", main= "Emerging freshwater habitat under SSP119 & 585", cex.main=0.8, lwd=0.7, xlim=c(2020,2100), ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("Relative area within emerging terrestrial overdeepening area (%)", cex=0.65, side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
#StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
#StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
#polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
#StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
#StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
#polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V6/(hab_ssp585$V6+hab_ssp585$V7+hab_ssp585$V8+hab_ssp585$V9)*100,col="blue",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V7/(hab_ssp585$V6+hab_ssp585$V7+hab_ssp585$V8+hab_ssp585$V9)*100,col="green",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V8/(hab_ssp585$V6+hab_ssp585$V7+hab_ssp585$V8+hab_ssp585$V9)*100,col="#081d58",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V9/(hab_ssp585$V6+hab_ssp585$V7+hab_ssp585$V8+hab_ssp585$V9)*100,col="darkgreen",lty=2, lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V6/(hab_ssp119$V6+hab_ssp119$V7+hab_ssp119$V8+hab_ssp119$V9)*100,col="blue",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V7/(hab_ssp119$V6+hab_ssp119$V7+hab_ssp119$V8+hab_ssp119$V9)*100,col="green",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V8/(hab_ssp119$V6+hab_ssp119$V7+hab_ssp119$V8+hab_ssp119$V9)*100,col="#081d58",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V9/(hab_ssp119$V6+hab_ssp119$V7+hab_ssp119$V8+hab_ssp119$V9)*100,col="darkgreen",lty=1, lwd=1.3)

legend("topleft", inset=.05, legend=c("SSP585 (dotted line)","SSP119 (solid line)","Cold&Shallow","Temperate&Shallow","Cold&Deep","Temperate&Deep"), col=c("black", "black","blue","green","#081d58","darkgreen"), lty=c(2,1,1,1,1,1), lwd=c(1,1,1,1,1,1), cex=0.55, bg=mycol)

# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()



###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
####///HABITAT // EXPLORATORY SALTWATER ECOLOGY####
######-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/SALTWATER/Cold&Shallow####

png(paste(chemin_figures,r,"/",r,"_Habita_Saltwater_Cold&ShallowArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Lecture fichier qui contient les data
for (s in  Scenario){  
  assign(paste("hab_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/habitat_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  assign(paste("STD_hab_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_habitat_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
  
} 

Habita_Saltwater_Cold_ShallowArea<-c( "Habita_Saltwater_Cold&ShallowArea",r,
                                       hab_ssp119[Res_ssp119$V1=="2020",]$V10, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V10,hab_ssp119[Res_ssp119$V1=="2050",]$V10, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V10,hab_ssp119[Res_ssp119$V1=="2100",]$V10, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V10 ,
                                       hab_ssp585[Res_ssp585$V1=="2020",]$V10, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V10,hab_ssp585[Res_ssp585$V1=="2050",]$V10, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V10,hab_ssp585[Res_ssp585$V1=="2100",]$V10, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V10 ,
                                       100*hab_ssp119[Res_ssp119$V1=="2100",]$V10/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                       100*hab_ssp585[Res_ssp585$V1=="2100",]$V10/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                       NA)
table_summary<-rbind(table_summary,Habita_Saltwater_Cold_ShallowArea,stringsAsFactors = FALSE)

Ymax_absH1<-(max(hab_ssp585$V10+STD_hab_ssp585$V10))
Ymax_absH2<-(max(hab_ssp585$V11+STD_hab_ssp585$V11))
Ymax_absH3<-(max(hab_ssp585$V12+STD_hab_ssp585$V12))
Ymax_absH4<-(max(hab_ssp585$V13+STD_hab_ssp585$V13))
Ymax_abs<-(max(Ymax_absH1,Ymax_absH2,Ymax_absH3,Ymax_absH4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V10, axes=FALSE, xlab="", ylab="", main= expression("Emerging saltwater habitat (MAAT < 0"*~degree*C*" and depth < 5m)"), cex.main=0.8, lwd=1.7, xlim=c(2000,2100) ,ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V10+STD_hab_ssp126$V10)
StNeg<-(hab_ssp126$V10-STD_hab_ssp126$V10)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V10+STD_hab_ssp585$V10)
StNeg<-(hab_ssp585$V10-STD_hab_ssp585$V10)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V10,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V10,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V10,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V10,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V10,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

Ymax_relH1<-(max(hab_ssp585$V10+STD_hab_ssp585$V10)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH2<-(max(hab_ssp585$V11+STD_hab_ssp585$V11)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH3<-(max(hab_ssp585$V12+STD_hab_ssp585$V12)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_relH4<-(max(hab_ssp585$V13+STD_hab_ssp585$V13)/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100)
Ymax_rel<-(max(Ymax_relH1,Ymax_relH2,Ymax_relH3,Ymax_relH4))

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V10/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/SALTWATER/Temperate&Shallow####

png(paste(chemin_figures,r,"/",r,"_Habita_Saltwater_Temperate&ShallowArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Habita_Saltwater_Temperate_ShallowArea<-c( "Habita_Saltwater_Temperate&ShallowArea",r,
                                            hab_ssp119[Res_ssp119$V1=="2020",]$V11, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V11,hab_ssp119[Res_ssp119$V1=="2050",]$V11, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V11,hab_ssp119[Res_ssp119$V1=="2100",]$V11, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V11 ,
                                            hab_ssp585[Res_ssp585$V1=="2020",]$V11, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V11,hab_ssp585[Res_ssp585$V1=="2050",]$V11, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V11,hab_ssp585[Res_ssp585$V1=="2100",]$V11, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V11 ,
                                            100*hab_ssp119[Res_ssp119$V1=="2100",]$V11/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                            100*hab_ssp585[Res_ssp585$V1=="2100",]$V11/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                            NA)
table_summary<-rbind(table_summary,Habita_Saltwater_Temperate_ShallowArea,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V11, axes=FALSE, xlab="", ylab="", lwd=1.7, main= expression("Emerging saltwater habitat (MAAT > 0"*~degree*C*" and depth < 5m)"), cex.main=0.8, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V11+STD_hab_ssp126$V11)
StNeg<-(hab_ssp126$V11-STD_hab_ssp126$V11)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V11+STD_hab_ssp585$V11)
StNeg<-(hab_ssp585$V11-STD_hab_ssp585$V11)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V11,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V11,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V11,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V11,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V11,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V10/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/SALTWATER/Cold&Deep####

png(paste(chemin_figures,r,"/",r,"_Habita_Saltwater_Cold&DeepArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Habita_Saltwater_Cold_DeepArea<-c( "Habita_Saltwater_Cold&DeepArea",r,
                                    hab_ssp119[Res_ssp119$V1=="2020",]$V12, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V12,hab_ssp119[Res_ssp119$V1=="2050",]$V12, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V12,hab_ssp119[Res_ssp119$V1=="2100",]$V12, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V12 ,
                                    hab_ssp585[Res_ssp585$V1=="2020",]$V12, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V12,hab_ssp585[Res_ssp585$V1=="2050",]$V12, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V12,hab_ssp585[Res_ssp585$V1=="2100",]$V12, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V12 ,
                                    100*hab_ssp119[Res_ssp119$V1=="2100",]$V12/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                    100*hab_ssp585[Res_ssp585$V1=="2100",]$V12/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                    NA)
table_summary<-rbind(table_summary,Habita_Saltwater_Cold_DeepArea,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V12, axes=FALSE, xlab="", ylab="", main= expression("Emerging saltwater habitat (MAAT < 0"*~degree*C*" and depth > 5m)"), cex.main=0.8, lwd=1.7, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V12+STD_hab_ssp126$V12)
StNeg<-(hab_ssp126$V12-STD_hab_ssp126$V12)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V12+STD_hab_ssp585$V12)
StNeg<-(hab_ssp585$V12-STD_hab_ssp585$V12)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V12,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V12,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V12,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V12,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V12,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V10/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/SALTWATER/Temperate&Deep####

png(paste(chemin_figures,r,"/",r,"_Habita_Saltwater_Temperate&DeepArea.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

Habita_Saltwater_Temperate_DeepArea<-c( "Habita_Saltwater_Temperate&DeepArea",r,
                                         hab_ssp119[Res_ssp119$V1=="2020",]$V13, STD_hab_ssp119[Std_ssp119$V1=="2020",]$V13,hab_ssp119[Res_ssp119$V1=="2050",]$V13, STD_hab_ssp119[Std_ssp119$V1=="2050",]$V13,hab_ssp119[Res_ssp119$V1=="2100",]$V13, STD_hab_ssp119[Std_ssp119$V1=="2100",]$V13 ,
                                         hab_ssp585[Res_ssp585$V1=="2020",]$V13, STD_hab_ssp585[Std_ssp585$V1=="2020",]$V13,hab_ssp585[Res_ssp585$V1=="2050",]$V13, STD_hab_ssp585[Std_ssp585$V1=="2050",]$V13,hab_ssp585[Res_ssp585$V1=="2100",]$V13, STD_hab_ssp585[Std_ssp585$V1=="2100",]$V13 ,
                                         100*hab_ssp119[Res_ssp119$V1=="2100",]$V13/Res_ssp119[Res_ssp119$V1=="2100",]$V13,
                                         100*hab_ssp585[Res_ssp585$V1=="2100",]$V13/Res_ssp585[Res_ssp585$V1=="2100",]$V13,
                                         NA)
table_summary<-rbind(table_summary,Habita_Saltwater_Temperate_DeepArea,stringsAsFactors = FALSE)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp126$V1,hab_ssp126$V13, axes=FALSE, xlab="", ylab="", main= expression("Emerging saltwater habitat (MAAT > 0"*~degree*C*" and depth > 5m)"), cex.main=0.8, lwd=1.7, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
StPos<-(hab_ssp126$V13+STD_hab_ssp126$V13)
StNeg<-(hab_ssp126$V13-STD_hab_ssp126$V13)
# Polygone de la StDev ssp126
polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
StPos<-(hab_ssp585$V13+STD_hab_ssp585$V13)
StNeg<-(hab_ssp585$V13-STD_hab_ssp585$V13)
# Polygone de la StDev ssp585
polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V13,col="firebrick4",lwd=1.7)
lines(hab_ssp370$V1,hab_ssp370$V13,col="red",lwd=1.7)
lines(hab_ssp245$V1,hab_ssp245$V13,col="orange",lwd=1.7)
lines(hab_ssp126$V1,hab_ssp126$V13,col="darkblue",lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V13,col="dodgerblue",lwd=1.7)

legend("topleft", inset=.05, legend=c("SSP585", "SSP370", "SSP245", "SSP126", "SSP119"), col=c("firebrick4", "red", "orange", "darkblue", "dodgerblue"), lty=1, cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V10/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/SALTWATER/SSP119 & 585####

png(paste(chemin_figures,r,"/",r,"_Habita_Saltwater_SSP119-SSP585.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,4))

# Plot 1a
plot(hab_ssp119$V1,hab_ssp119$V10, axes=FALSE, xlab="", ylab="", lwd=0.7, main= "Emerging saltwater habitat under SSP119 & 585", cex.main=0.8, xlim=c(2000,2100), ylim=c(0,Ymax_abs), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
mtext(expression("area (km"^~"2"~")"),side=2, line=2.5)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

# Cordonn??es Y de StDev ssp126
#StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
#StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
#polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
#StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
#StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
#polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V10,col="blue",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V11,col="green",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V12,col="#081d58",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V13,col="darkgreen",lty=2, lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V10,col="blue",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V11,col="green",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V12,col="#081d58",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V13,col="darkgreen",lty=1, lwd=1.3)

legend("topleft", inset=.05, legend=c("SSP585 (dotted line)","SSP119 (solid line)","Cold&Shallow","Temperate&Shallow","Cold&Deep","Temperate&Deep"), col=c("black", "black","blue","green","#081d58","darkgreen"), lty=c(2,1,1,1,1,1), lwd=c(1,1,1,1,1,1), cex=0.55, bg=mycol)

# Plot 1b pour faire courbe relative ?? droide
par(new=T)
plot(hab_ssp585$V1,hab_ssp585$V10/Res_ssp585[Res_ssp585$V1=="2000",]$V4*100, axes=FALSE, xlab="", ylab="", xlim=c(2020,2100), ylim=c(0,Ymax_rel), yaxs="i",xaxs="i",las=1, type="l", lwd=1, lty=1, col=rgb(0,0,1,alpha = 0), yaxt="n")
mtext("area relative to the 2000 glacier area (%)",side=4,col="black",line=2.5)
axis(2, labels = FALSE, tck = FALSE, col="black")
axis(4, tck = 0.03, cex.axis=0.75, las=1, col="black")

# Lignes si axe Y coup??
lines(x=c(2000,2000),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()

###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####Habitat/SALTWATER SSP 119 & 585 %####

png(paste(chemin_figures,r,"/",r,"_Habita_Saltwater_SSP119-SSP585_percentage.png",sep=""),width=4.5,height=4, units = 'in', res = 300)

# affichage graph double Y
par(mar=c(4,4,2,2))

# Plot 1a
plot(hab_ssp119$V1,hab_ssp119$V10/(hab_ssp119$V10+hab_ssp119$V11+hab_ssp119$V12+hab_ssp119$V13)*100, axes=FALSE, xlab="", ylab="", main= "Emerging saltwater habitat under SSP119 & 585", cex.main=0.8, lwd=0.7, xlim=c(2020,2100), ylim=c(0,100), yaxs="i",xaxs="i", type="l",col="black")
axis(1, tck = 0.03, cex.axis=0.75, col="black")
axis(2, tck = 0.03, cex.axis=0.75, las=1, col="black")
axis(3, tck = 0.03, labels = FALSE, col="black")
axis(4, tck = 0.03, labels = FALSE, col="black")
mtext("Relative area within emerging overdeepening bsl area (%)", cex=0.65, side=2, line=2.5)

# Cordonn??es Y de StDev ssp126
#StPos<-(hab_ssp126$V5+STD_hab_ssp126$V5)
#StNeg<-(hab_ssp126$V5-STD_hab_ssp126$V5)
# Polygone de la StDev ssp126
#polygon(x=c(hab_ssp126$V1,rev(hab_ssp126$V1)),y=c(StNeg,rev(StPos)),col = rgb(0,0,1,alpha = 0.3),border=NA)

# Cordonn??es Y de StDev ssp585
#StPos<-(hab_ssp585$V5+STD_hab_ssp585$V5)
#StNeg<-(hab_ssp585$V5-STD_hab_ssp585$V5)
# Polygone de la StDev ssp585
#polygon(x=c(hab_ssp585$V1,rev(hab_ssp585$V1)),y=c(StNeg,rev(StPos)),col = rgb(1,0,0,alpha = 0.3),border=NA)

# Data plot 1a
lines(hab_ssp585$V1,hab_ssp585$V10/(hab_ssp585$V10+hab_ssp585$V11+hab_ssp585$V12+hab_ssp585$V13)*100,col="blue",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V11/(hab_ssp585$V10+hab_ssp585$V11+hab_ssp585$V12+hab_ssp585$V13)*100,col="green",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V12/(hab_ssp585$V10+hab_ssp585$V11+hab_ssp585$V12+hab_ssp585$V13)*100,col="#081d58",lty=2, lwd=1.7)
lines(hab_ssp585$V1,hab_ssp585$V13/(hab_ssp585$V10+hab_ssp585$V11+hab_ssp585$V12+hab_ssp585$V13)*100,col="darkgreen",lty=2, lwd=1.7)
lines(hab_ssp119$V1,hab_ssp119$V10/(hab_ssp119$V10+hab_ssp119$V11+hab_ssp119$V12+hab_ssp119$V13)*100,col="blue",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V11/(hab_ssp119$V10+hab_ssp119$V11+hab_ssp119$V12+hab_ssp119$V13)*100,col="green",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V12/(hab_ssp119$V10+hab_ssp119$V11+hab_ssp119$V12+hab_ssp119$V13)*100,col="#081d58",lty=1, lwd=1.3)
lines(hab_ssp119$V1,hab_ssp119$V13/(hab_ssp119$V10+hab_ssp119$V11+hab_ssp119$V12+hab_ssp119$V13)*100,col="darkgreen",lty=1, lwd=1.3)

legend("topleft", inset=.05, legend=c("SSP585 (dotted line)","SSP119 (solid line)","Cold&Shallow","Temperate&Shallow","Cold&Deep","Temperate&Deep"), col=c("black", "black","blue","green","#081d58","darkgreen"), lty=c(2,1,1,1,1,1), lwd=c(1,1,1,1,1,1), cex=0.55, bg=mycol)


# Lignes si axe Y coup??
lines(x=c(2020,2020),y=c(-1000000,1000000),col="black", lwd=1.7)
lines(x=c(2100,2100),y=c(-1000000,1000000),col="black", lwd=1.7)

dev.off()



}

colnames(table_summary)<-table_colnames
write.csv(table_summary,paste(chemin_table_summary,"table_summary.csv",sep=""), row.names=FALSE)


dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()


#############################################
#####TABLE AREA PENTE DEPTH###############
#############################################

table_colnames_var_env<-c("Region","area_slope_inf_5_ssp119_2100","STD_area_slope_inf_5_ssp119_2100","area_slope_inf_30_ssp119_2100",
                          "STD_area_slope_inf_30_ssp119_2100","area_slope_total_ssp119_2100",
                          "STD_area_slope_total_ssp119_2100","slope_mediane_ssp119_2100",
                          "area_slope_mediane_ssp119_2100","STD_area_slope_mediane_ssp119_2100",
                          "area_slope_inf_5_ssp585_2100","STD_area_slope_inf_5_ssp585_2100",
                          "area_slope_inf_30_ssp585_2100","STD_area_slope_inf_30_ssp585_2100",
                          "area_slope_total_ssp585_2100","STD_area_slope_total_ssp585_2100",
                          "slope_mediane_ssp585_2100","area_slope_mediane_ssp585_2100",
                          "STD_area_slope_mediane_ssp585_2100",
                          
                          "area_overdeepening_inf_5_ssp119_2100","STD_area_overdeepening_inf_5_ssp119_2100",
                          "area_overdeepening_total_ssp119_2100","STD_area_overdeepening_total_ssp119_2100",
                          "overdeepening_mediane_ssp119_2100","area_overdeepening_mediane_ssp119_2100",
                          "STD_area_overdeepening_mediane_ssp119_2100","area_overdeepening_inf_5_ssp585_2100",
                          "STD_area_overdeepening_inf_5_ssp585_2100","area_overdeepening_total_ssp585_2100",
                          "STD_area_overdeepening_total_ssp585_2100","overdeepening_mediane_ssp585_2100",
                          "area_overdeepening_mediane_ssp585_2100","STD_area_overdeepening_mediane_ssp585_2100",
                          
                          
                          "terrestrial_area_MAAT_inf_0_ssp585_2100","STD_terrestrial_area_MAAT_inf_0_ssp119_2100",
                          "terrestrial_area_MAAT_total_ssp119_2100","STD_terrestrial_area_MAAT_total_ssp119_2100",
                          "MAAT_terrestrial_mediane_ssp119_2100",
                          "area_MAAT_terrestrial_mediane_ssp119_2100","STD_MAAT_area_terrestrial_mediane_ssp119_2100",
                          "STD_terrestrial_area_MAAT_inf_0_ssp585_2100","terrestrial_area_MAAT_total_ssp585_2100",
                          "STD_terrestrial_area_MAAT_total_ssp585_2100",
                          "MAAT_terrestrial_mediane_ssp585_2100","area_MAAT_terrestrial_mediane_ssp585_2100",
                          "STD_MAAT_area_terrestrial_mediane_ssp585_2100",
                          
                          "overdeepening_area_MAAT_inf_0_ssp585_2100","STD_overdeepening_area_MAAT_inf_0_ssp119_2100",
                          "overdeepening_area_MAAT_total_ssp119_2100","STD_overdeepening_area_MAAT_total_ssp119_2100",
                          "MAAT_overdeepening_mediane_ssp119_2100",
                          "area_MAAT_overdeepening_mediane_ssp119_2100","STD_MAAT_area_overdeepening_mediane_ssp119_2100",
                          "STD_overdeepening_area_MAAT_inf_0_ssp585_2100","overdeepening_area_MAAT_total_ssp585_2100",
                          "STD_overdeepening_area_MAAT_total_ssp585_2100",
                          "MAAT_overdeepening_mediane_ssp585_2100","area_MAAT_overdeepening_mediane_ssp585_2100",
                          "STD_MAAT_area_overdeepening_mediane_ssp585_2100") 




table_summary_var_env<-data.frame(matrix(NA,nrow=0,ncol=length(table_colnames_var_env),dimnames = list(c(),table_colnames_var_env)),stringsAsFactors = FALSE)

zonefiles= list.files(chemin_averaged)                                                        
chemin_zonefiles<-paste(chemin_averaged,zonefiles,"/",sep="")
filesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx("overview*.dat"))
STDfilesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx("STD_overview*.dat"))
filestot<-c()
filesname<-c()
for (z in zonefiles){
  filesname<-c(filesname,  str_sub(list.files(paste(chemin_averaged,z,"/",sep=""),pattern=glob2rx("overview*.dat")),end =-5))
  filestot<-c(filestot,paste(chemin_averaged,z,"/", list.files(paste(chemin_averaged,z,"/",sep=""),pattern=glob2rx("overview*.dat")), sep = ""))
}
Region<-unique(str_sub(filesname,start =10,end=14))
Scenario<-unique(str_sub(filesname,start=-6))

#Si on veut avoir une seul r??gion, changer le num et enlever le di??se
#Region="RGI13"
#r="RGI13"
#zonefiles="CentralAsia"

for (r in Region){
  
  zonefiles[which(Region==r)]
  
  
  ####///SLOPE FILES####
  ####Land area Slope####
  for (s in  Scenario){  
    assign(paste("SLO_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/slope_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    assign(paste("STD_slo_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_slope_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    
  }   
  
  # Data slope 2100
  
  area_slope_inf_5_ssp585_2100<-sum((SLO_ssp585$V12-SLO_ssp585$V4)[c(1:5)])
  STD_area_slope_inf_5_ssp585_2100<-sum((STD_slo_ssp585$V12)[c(1:5)])
  area_slope_inf_30_ssp585_2100<-sum((SLO_ssp585$V12-SLO_ssp585$V4)[c(1:30)])
  STD_area_slope_inf_30_ssp585_2100<-sum((STD_slo_ssp585$V12)[c(1:30)])
  area_slope_total_ssp585_2100<-sum(SLO_ssp585$V12-SLO_ssp585$V4)
  STD_area_slope_total_ssp585_2100<-sum(STD_slo_ssp585$V12)
  
  for (me in c(1:length(SLO_ssp585$V12))){
    area_slope_total_ssp585_2100<-sum(SLO_ssp585$V12-SLO_ssp585$V4)
    slope_mediane_ssp585_2100<-me-0.5
    area_slope_mediane_ssp585_2100<-sum((SLO_ssp585$V12-SLO_ssp585$V4)[c(1:me)])
    STD_area_slope_mediane_ssp585_2100<-sum(STD_slo_ssp585$V12[c(1:me)])
    if (sum((SLO_ssp585$V12-SLO_ssp585$V4)[c(1:me)])>area_slope_total_ssp585_2100/2){
      break
    }
  }
  
  area_slope_inf_5_ssp119_2100<-sum((SLO_ssp119$V12-SLO_ssp119$V4)[c(1:5)])
  STD_area_slope_inf_5_ssp119_2100<-sum((STD_slo_ssp119$V12)[c(1:5)])
  area_slope_inf_30_ssp119_2100<-sum((SLO_ssp119$V12-SLO_ssp119$V4)[c(1:30)])
  STD_area_slope_inf_30_ssp119_2100<-sum((STD_slo_ssp119$V12)[c(1:30)])
  area_slope_total_ssp119_2100<-sum(SLO_ssp119$V12-SLO_ssp119$V4)
  STD_area_slope_total_ssp119_2100<-sum(STD_slo_ssp119$V12)
  
  for (me in c(1:length(SLO_ssp119$V12))){
    area_slope_total_ssp119_2100<-sum(SLO_ssp119$V12-SLO_ssp119$V4)
    slope_mediane_ssp119_2100<-me-0.5
    area_slope_mediane_ssp119_2100<-sum((SLO_ssp119$V12-SLO_ssp119$V4)[c(1:me)])
    STD_area_slope_mediane_ssp119_2100<-sum(STD_slo_ssp119$V12[c(1:me)])
    if (sum((SLO_ssp119$V12-SLO_ssp119$V4)[c(1:me)])>area_slope_total_ssp119_2100/2){
      break
    }
  }
  
  Slope_area<-c(r,area_slope_inf_5_ssp119_2100,STD_area_slope_inf_5_ssp119_2100,
                area_slope_inf_30_ssp119_2100,
                STD_area_slope_inf_30_ssp119_2100,
                area_slope_total_ssp119_2100,
                STD_area_slope_total_ssp119_2100,
                slope_mediane_ssp119_2100,
                area_slope_mediane_ssp119_2100,
                STD_area_slope_mediane_ssp119_2100,
                area_slope_inf_5_ssp585_2100,STD_area_slope_inf_5_ssp585_2100,
                area_slope_inf_30_ssp585_2100,
                STD_area_slope_inf_30_ssp585_2100,
                area_slope_total_ssp585_2100,
                STD_area_slope_total_ssp585_2100,
                slope_mediane_ssp585_2100,
                area_slope_mediane_ssp585_2100,
                STD_area_slope_mediane_ssp585_2100)
  
  
  
  ####///DEPTH overdeepening AREA####
  ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ####overdeepening area Depth####
  
  
  for (s in  Scenario){  
    assign(paste("DPT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/lakearea_depth_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    assign(paste("STD_DPT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_lakearea_depth_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    
  } 
  
  area_overdeepening_inf_5_ssp119_2100<-sum((DPT_ssp119$V12-DPT_ssp119$V4)[c(1:5)])
  STD_area_overdeepening_inf_5_ssp119_2100<-sum((STD_DPT_ssp119$V12)[c(1:5)])
  area_overdeepening_total_ssp119_2100<-sum(DPT_ssp119$V12-DPT_ssp119$V4)
  STD_area_overdeepening_total_ssp119_2100<-sum(STD_DPT_ssp119$V12)
  for (me in c(1:length(DPT_ssp119$V12))){
    area_overdeepening_total_ssp119_2100<-sum(DPT_ssp119$V12-DPT_ssp119$V4)
    overdeepening_mediane_ssp119_2100<-me-0.5
    area_overdeepening_mediane_ssp119_2100<-sum((DPT_ssp119$V12-DPT_ssp119$V4)[c(1:me)])
    STD_area_overdeepening_mediane_ssp119_2100<-sum(STD_DPT_ssp119$V12[c(1:me)])
    if (sum((DPT_ssp119$V12-DPT_ssp119$V4)[c(1:me)])>area_overdeepening_total_ssp119_2100/2){
      break
    }
  }
  
  area_overdeepening_inf_5_ssp585_2100<-sum((DPT_ssp585$V12-DPT_ssp585$V4)[c(1:5)])
  STD_area_overdeepening_inf_5_ssp585_2100<-sum((STD_DPT_ssp585$V12)[c(1:5)])
  area_overdeepening_total_ssp585_2100<-sum(DPT_ssp585$V12-DPT_ssp585$V4)
  STD_area_overdeepening_total_ssp585_2100<-sum(STD_DPT_ssp585$V12)
  for (me in c(1:length(DPT_ssp585$V12))){
    area_overdeepening_total_ssp585_2100<-sum(DPT_ssp585$V12-DPT_ssp585$V4)
    overdeepening_mediane_ssp585_2100<-me-0.5
    area_overdeepening_mediane_ssp585_2100<-sum((DPT_ssp585$V12-DPT_ssp585$V4)[c(1:me)])
    STD_area_overdeepening_mediane_ssp585_2100<-sum(STD_DPT_ssp585$V12[c(1:me)])
    if (sum((DPT_ssp585$V12-DPT_ssp585$V4)[c(1:me)])>area_overdeepening_total_ssp585_2100/2){
      break
    }
  }
  
  overdeepening_area<-c(area_overdeepening_inf_5_ssp119_2100,STD_area_overdeepening_inf_5_ssp119_2100,
                        area_overdeepening_total_ssp119_2100,STD_area_overdeepening_total_ssp119_2100,
                        overdeepening_mediane_ssp119_2100,area_overdeepening_mediane_ssp119_2100,
                        STD_area_overdeepening_mediane_ssp119_2100,area_overdeepening_inf_5_ssp585_2100,
                        STD_area_overdeepening_inf_5_ssp585_2100,area_overdeepening_total_ssp585_2100,
                        STD_area_overdeepening_total_ssp585_2100,overdeepening_mediane_ssp585_2100,
                        area_overdeepening_mediane_ssp585_2100,STD_area_overdeepening_mediane_ssp585_2100)
  
  
  
  ####///MAAT FILES Overdeepening area####
  ####MAAT Overdeepening area#####
  ###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Lecture fichier qui contient les data
  for (s in  Scenario){  
    assign(paste("MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/MAAT_lakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    assign(paste("STD_MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_MAAT_delakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    
  } 
  
  overdeepening_area_MAAT_inf_0_ssp119_2100<-sum(MAAT_ssp119$V12[c(1:which(MAAT_ssp119$V1==-0.25))])
  STD_overdeepening_area_MAAT_inf_0_ssp119_2100<-sum(STD_MAAT_ssp119$V12[c(1:which(MAAT_ssp119$V1==-0.250))])
  overdeepening_area_MAAT_total_ssp119_2100<-sum(MAAT_ssp119$V12)
  STD_overdeepening_area_MAAT_total_ssp119_2100<-sum(STD_MAAT_ssp119$V12)
  for (me in c(1:length(MAAT_ssp119$V12))){
    overdeepening_area_MAAT_total_ssp119_2100<-sum(MAAT_ssp119$V12)
    MAAT_overdeepening_mediane_ssp119_2100<-MAAT_ssp119$V1[me]
    area_MAAT_overdeepening_mediane_ssp119_2100<-sum((MAAT_ssp119$V12)[c(1:me)])
    STD_MAAT_area_overdeepening_mediane_ssp119_2100<-sum(STD_MAAT_ssp119$V12[c(1:me)])
    if (sum((MAAT_ssp119$V12)[c(1:me)])>overdeepening_area_MAAT_total_ssp119_2100/2){
      break
    }
  }
  
  overdeepening_area_MAAT_inf_0_ssp585_2100<-sum(MAAT_ssp585$V12[c(1:which(MAAT_ssp585$V1==-0.25))])
  STD_overdeepening_area_MAAT_inf_0_ssp585_2100<-sum(STD_MAAT_ssp585$V12[c(1:which(MAAT_ssp585$V1==-0.250))])
  overdeepening_area_MAAT_total_ssp585_2100<-sum(MAAT_ssp585$V12)
  STD_overdeepening_area_MAAT_total_ssp585_2100<-sum(STD_MAAT_ssp585$V12)
  for (me in c(1:length(MAAT_ssp585$V12))){
    overdeepening_area_MAAT_total_ssp585_2100<-sum(MAAT_ssp585$V12)
    MAAT_overdeepening_mediane_ssp585_2100<-MAAT_ssp585$V1[me]
    area_MAAT_overdeepening_mediane_ssp585_2100<-sum((MAAT_ssp585$V12)[c(1:me)])
    STD_MAAT_area_overdeepening_mediane_ssp585_2100<-sum(STD_MAAT_ssp585$V12[c(1:me)])
    if (sum((MAAT_ssp585$V12)[c(1:me)])>overdeepening_area_MAAT_total_ssp585_2100/2){
      break
    }
  }
  
  
  
  
  overdeepening_area_MAAT<-c(overdeepening_area_MAAT_inf_0_ssp585_2100,STD_overdeepening_area_MAAT_inf_0_ssp119_2100,
                             overdeepening_area_MAAT_total_ssp119_2100,STD_overdeepening_area_MAAT_total_ssp119_2100,
                             MAAT_overdeepening_mediane_ssp119_2100,
                             area_MAAT_overdeepening_mediane_ssp119_2100,STD_MAAT_area_overdeepening_mediane_ssp119_2100,
                             STD_overdeepening_area_MAAT_inf_0_ssp585_2100,overdeepening_area_MAAT_total_ssp585_2100,
                             STD_overdeepening_area_MAAT_total_ssp585_2100,
                             MAAT_overdeepening_mediane_ssp585_2100,area_MAAT_overdeepening_mediane_ssp585_2100,
                             STD_MAAT_area_overdeepening_mediane_ssp585_2100) 
  
  
  ###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ####///MAAT FILES DA####
  ####MAAT Land AREA####
  
  # Lecture fichier qui contient les data
  for (s in  Scenario){  
    assign(paste("MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/MAAT_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    assign(paste("STD_MAAT_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_MAAT_deglacarea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    
  } 
  
  
  terrestrial_area_MAAT_inf_0_ssp119_2100<-sum(MAAT_ssp119$V12[c(1:which(MAAT_ssp119$V1==-0.25))])
  STD_terrestrial_area_MAAT_inf_0_ssp119_2100<-sum(STD_MAAT_ssp119$V12[c(1:which(MAAT_ssp119$V1==-0.250))])
  terrestrial_area_MAAT_total_ssp119_2100<-sum(MAAT_ssp119$V12)
  STD_terrestrial_area_MAAT_total_ssp119_2100<-sum(STD_MAAT_ssp119$V12)
  for (me in c(1:length(MAAT_ssp119$V12))){
    terrestrial_area_MAAT_total_ssp119_2100<-sum(MAAT_ssp119$V12)
    MAAT_terrestrial_mediane_ssp119_2100<-MAAT_ssp119$V1[me]
    area_MAAT_terrestrial_mediane_ssp119_2100<-sum((MAAT_ssp119$V12)[c(1:me)])
    STD_MAAT_area_terrestrial_mediane_ssp119_2100<-sum(STD_MAAT_ssp119$V12[c(1:me)])
    if (sum((MAAT_ssp119$V12)[c(1:me)])>terrestrial_area_MAAT_total_ssp119_2100/2){
      break
    }
  }
  
  terrestrial_area_MAAT_inf_0_ssp585_2100<-sum(MAAT_ssp585$V12[c(1:which(MAAT_ssp585$V1==-0.25))])
  STD_terrestrial_area_MAAT_inf_0_ssp585_2100<-sum(STD_MAAT_ssp585$V12[c(1:which(MAAT_ssp585$V1==-0.250))])
  terrestrial_area_MAAT_total_ssp585_2100<-sum(MAAT_ssp585$V12)
  STD_terrestrial_area_MAAT_total_ssp585_2100<-sum(STD_MAAT_ssp585$V12)
  for (me in c(1:length(MAAT_ssp585$V12))){
    terrestrial_area_MAAT_total_ssp585_2100<-sum(MAAT_ssp585$V12)
    MAAT_terrestrial_mediane_ssp585_2100<-MAAT_ssp585$V1[me]
    area_MAAT_terrestrial_mediane_ssp585_2100<-sum((MAAT_ssp585$V12)[c(1:me)])
    STD_MAAT_area_terrestrial_mediane_ssp585_2100<-sum(STD_MAAT_ssp585$V12[c(1:me)])
    if (sum((MAAT_ssp585$V12)[c(1:me)])>terrestrial_area_MAAT_total_ssp585_2100/2){
      break
    }
  }
  
  
  
  
  terrestrial_area_MAAT<-c(terrestrial_area_MAAT_inf_0_ssp585_2100,STD_terrestrial_area_MAAT_inf_0_ssp119_2100,
                           terrestrial_area_MAAT_total_ssp119_2100,STD_terrestrial_area_MAAT_total_ssp119_2100,
                           MAAT_terrestrial_mediane_ssp119_2100,
                           area_MAAT_terrestrial_mediane_ssp119_2100,STD_MAAT_area_terrestrial_mediane_ssp119_2100,
                           STD_terrestrial_area_MAAT_inf_0_ssp585_2100,terrestrial_area_MAAT_total_ssp585_2100,
                           STD_terrestrial_area_MAAT_total_ssp585_2100,
                           MAAT_terrestrial_mediane_ssp585_2100,area_MAAT_terrestrial_mediane_ssp585_2100,
                           STD_MAAT_area_terrestrial_mediane_ssp585_2100) 
  
  
  
  table_summary_var_env<-rbind(table_summary_var_env,c(Slope_area,overdeepening_area,terrestrial_area_MAAT,overdeepening_area_MAAT),stringsAsFactors = FALSE)
  
}

colnames(table_summary_var_env)<-table_colnames_var_env
write.csv(table_summary_var_env,paste(chemin_table_summary,"table_summary_var_env.csv",sep=""), row.names=FALSE)
