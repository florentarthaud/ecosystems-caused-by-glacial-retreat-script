library(stringr)
library(matrixStats)

####///VOLUME_OVERDEEPENING ET SD####
chemin_averaged<-("../data_analyse/regional_data/averaged/")
chemin_aggregated<-("../data_analyse/regional_data/aggregated/")
zonefiles= list.files(chemin_aggregated)                                                        
chemin_zonefiles<-paste("../data_analyse/regional_data/aggregated/",zonefiles,"/",sep="")
filesaggregated<-list.files(chemin_zonefiles, pattern=glob2rx("overview*.dat"))[-c(1:14)]
filestot<-paste(rep(chemin_zonefiles, each = length(filesaggregated)), filesaggregated, sep = "")
filesname<-str_sub(str_replace_all(rep(filesaggregated, length(chemin_zonefiles)),"-","_"),end =-5)
chemin_sortie_Vol<-paste("../data_analyse/regional_data/volume_overdpn/volume_overdpn",sep="")
chemin_sortie_Vol_RGI20<-paste("../data_analyse/regional_data/volume_overdpn/volume_overdpnRGI20.csv",sep="")
chemin_sortieREL<-paste("../data_analyse/regional_data/volume_overdpn/volume_overdpn_rel",sep="")
chemin_sortie2020<-paste("../data_analyse/regional_data/volume_overdpn/volume_overdpn_2020",sep="")
chemin_sortieREL_RGI20<-paste("../data_analyse/regional_data/volume_overdpn/volume_overdpn_relRGI20.csv",sep="")
chemin_sortie2020_RGI20<-paste("../data_analyse/regional_data/volume_overdpn/volume_overdpn_2020RGI20.csv",sep="")


# script si pour emp??cher que tout s'arr??te si il manque un ficher (mais normalement tout est ok maintenant)
for(f in 1:length(filestot)){
  if (file.exists(filestot[f])==T){
    t<-read.table(filestot[f], sep="", header=F, skip = 3)
    n<-filesname[f]
    assign(n,t)
  }   
}

model<-c(NA)
vol2000<-(NA)
vol2010<-(NA)
vol2020<-(NA)
vol2030<-(NA)
vol2040<-(NA)
vol2050<-(NA)
vol2060<-(NA)
vol2070<-(NA)
vol2080<-(NA)
vol2090<-(NA)
vol2100<-(NA)
tabVol<-data.frame(list(model,vol2000,vol2010,vol2020,vol2030,vol2040,vol2050,vol2060,vol2070,vol2080,vol2090,vol2100))
colnames(tabVol)<-c("model","vol2000","vol2010","vol2020","vol2030","vol2040","vol2050","vol2060","vol2070","vol2080","vol2090","vol2100")

for(t in 1:length(filestot)) {
  if (file.exists(filestot[t])==T){
    calcvol=(eval(parse(text = filesname[t]))$V13*0.001)*eval(parse(text = filesname[t]))$V14
    rajout<-c(filesname[t],calcvol)
    tabVol<-rbind(tabVol,rajout)
  }
}

tabVol<-na.omit(tabVol)
tabVol2<-tabVol
i <- c(2:12)                                  # Specify columns you want to change
tabVol[ , i] <- apply(tabVol[ , i], 2,        # Specify own function within apply
                      function(x) as.numeric(as.character(x)))
#remettre volume en numeric

Year<-eval(parse(text = filesname[t]))$V1
dataVol<-data.frame(list(Year))
colnames(dataVol)[1]<-"Year"

Region<-unique(str_sub(tabVolrel$model,start =10,end=14))
Region<-Region[Region!="RGI20"]


dataVolRGI20<-matrix(rep(0,121),ncol=11,nrow=11)

for (z in unique(Region)){
  
  Year<-eval(parse(text = filesname[t]))$V1
  dataVol<-data.frame(list(Year))
  dataVolpRGI20<-data.frame(list(Year))
  colnames(dataVol)[1]<-"Year"  
  
  for (s in unique(str_sub(tabVol$model,start=-6))){
    
    newtab<-(tabVol[str_sub(tabVol$model,start =10,end=14)==z & str_sub(tabVol$model,start=-6)==s,])
    
    
    meansVol<-colMeans(newtab[-1])
    SDVol<-colSds(as.matrix(newtab[-1]))
    trendvol<-meansVol[3]-meansVol

    dataVol<-cbind(dataVol,meansVol,trendvol,SDVol)
    colnames(dataVol)[length(dataVol)-2]<-paste("meansVol",z,s,sep="")
    colnames(dataVol)[length(dataVol)-1]<-paste("trendvol",z,s,sep="")
    colnames(dataVol)[length(dataVol)]<-paste("SDVol",z,s,sep="")
    
    #tableau o? SD est au carr? pour ensuite calcul? SD de RGI20
    dataVolpRGI20<-cbind(dataVolpRGI20,meansVol,trendvol,SDVol^2)
    colnames(dataVolpRGI20)[length(dataVol)-2]<-paste("meansVol",z,s,sep="")
    colnames(dataVolpRGI20)[length(dataVol)-1]<-paste("trendvol",z,s,sep="")
    colnames(dataVolpRGI20)[length(dataVol)]<-paste("SDVol",z,s,sep="")
    
    
  }
  
  assign(paste("dataVol_",z,sep=""),dataVol)
  eval(parse(text = filesname[t]))
    dataVolRGI20<-dataVolRGI20+dataVolpRGI20
  chemin_sortie_Vol2<-paste(chemin_sortie_Vol,z,".csv",sep="")
    write.csv(dataVol,chemin_sortie_Vol2)
  
}


dataVolRGI20<-cbind(Year,dataVolRGI20[,2:3],sqrt(dataVolRGI20[,4]),dataVolRGI20[,5:6],sqrt(dataVolRGI20[,7]),dataVolRGI20[,8:9],sqrt(dataVolRGI20[,10]),dataVolRGI20[,11:12],sqrt(dataVolRGI20[,13]),dataVolRGI20[,14:15],sqrt(dataVolRGI20[,16]))

colnames(dataVolRGI20)<-c("Year","meansVolRGI20ssp126","Trend_meansVolRGI20ssp126","SDVolRGI20ssp126","meansVolRGI20ssp245","Trend_meansVolRGI20ssp245","SDVolRGI20ssp245","meansVolRGI20ssp370","Trend_meansVolRGI20ssp370","SDVolRGI20ssp370","meansVolRGI20ssp585","Trend_meansVolRGI20ssp585","SDVolRGI20ssp585","meansVolRGI20ssp119","Trend_meansVolRGI20ssp119","SDVolRGI20ssp119")
write.csv(dataVolRGI20,chemin_sortie_Vol_RGI20)

 

---------------------------------------------------------------------------------------------------------------------------------------------
####///VOLUME_OVERDEEPENING / VOL GLACIAIRE ET SD####
# script si pour emp??cher que tout s'arr??te si il manque un ficher (mais normalement tout est ok maintenant)
for(f in 1:length(filestot)){
  if (file.exists(filestot[f])==T){
    t<-read.table(filestot[f], sep="", header=F, skip = 3)
    n<-filesname[f]
    assign(n,t)
  }   
}

model<-c(NA)
vol2000<-(NA)
vol2010<-(NA)
vol2020<-(NA)
vol2030<-(NA)
vol2040<-(NA)
vol2050<-(NA)
vol2060<-(NA)
vol2070<-(NA)
vol2080<-(NA)
vol2090<-(NA)
vol2100<-(NA)
tabVolrel<-data.frame(list(model,vol2000,vol2010,vol2020,vol2030,vol2040,vol2050,vol2060,vol2070,vol2080,vol2090,vol2100))
colnames(tabVolrel)<-c("model","vol2000","vol2010","vol2020","vol2030","vol2040","vol2050","vol2060","vol2070","vol2080","vol2090","vol2100")
tabVol2020<-data.frame(list(model,vol2000,vol2010,vol2020,vol2030,vol2040,vol2050,vol2060,vol2070,vol2080,vol2090,vol2100))
colnames(tabVol2020)<-c("model","vol2000","vol2010","vol2020","vol2030","vol2040","vol2050","vol2060","vol2070","vol2080","vol2090","vol2100")

for(t in 1:length(filestot)) {
  if (file.exists(filestot[t])==T){
    calcvol=(eval(parse(text = filesname[t]))$V13*0.001)*eval(parse(text = filesname[t]))$V14
    calcvol1=(calcvol-calcvol[3])
    calcvol3=(eval(parse(text = filesname[t]))$V7*0.9)
    calcvol2=((calcvol1/calcvol3)*100)
    calcvol4=((calcvol1/calcvol3[3])*100)  
    rajoutrel<-c(filesname[t],calcvol2)
    tabVolrel<-rbind(tabVolrel,rajoutrel)
    rajout2020<-c(filesname[t],calcvol4)
    tabVol2020<-rbind(tabVol2020,rajout2020)
  }
}

tabVolrel<-na.omit(tabVolrel)
tabVol2020<-na.omit(tabVol2020)
i <- c(2:12)                                  # Specify columns you want to change
tabVolrel[ , i] <- apply(tabVolrel[ , i], 2,        # Specify own function within apply
    function(x) as.numeric(as.character(x)))
tabVol2020[ , i] <- apply(tabVol2020[ , i], 2,        # Specify own function within apply
    function(x) as.numeric(as.character(x)))
#remettre volume en numeric

Region<-unique(str_sub(tabVolrel$model,start =10,end=14))
Region<-Region[Region!="RGI20"]


#dataVolrelRGI20<-matrix(rep(0,121),ncol=11,nrow=11)
dataVol2020RGI20<-matrix(rep(0,121),ncol=11,nrow=11)

for (z in Region){

  Year<-eval(parse(text = filesname[t]))$V1
  dataVolrel<-data.frame(list(Year))
  colnames(dataVolrel)[1]<-"Year" 
  dataVol2020<-data.frame(list(Year))
  colnames(dataVol2020)[1]<-"Year" 
  
 
  
  for (s in unique(str_sub(tabVolrel$model,start=-6))){
    
    newtabrel<-(tabVolrel[str_sub(tabVolrel$model,start =10,end=14)==z & str_sub(tabVolrel$model,start=-6)==s,])
    newtab2020<-(tabVol2020[str_sub(tabVol2020$model,start =10,end=14)==z & str_sub(tabVol2020$model,start=-6)==s,])
    
    volLac<-eval(parse(text =paste("dataVol_",z,"$meansVol",z,s,sep="")))
    overView<-read.csv(paste(chemin_averaged,zonefiles[-13][which(Region==z)],"/overview_",z,"_",s,".dat",sep=""), sep="", header=F, skip = 3)
    volLac<-eval(parse(text =paste("dataVol_",z,"$meansVol",z,s,sep="")))
        meansVolrel<-(volLac-volLac[3])/(overView$V7*0.9)*100
        meansVol2020<-(volLac-volLac[3])/(overView$V7[3]*0.9)*100
    #meansVolrel<-colMeans(newtabrel[-1],na.rm=T)
    SDVolrel<-colSds(as.matrix(newtabrel[-1]),na.rm=T)
    #meansVol2020<-colMeans(newtab2020[-1],na.rm=T)
    SDVol2020<-colSds(as.matrix(newtab2020[-1]),na.rm=T)
    
    dataVolrel<-cbind(dataVolrel,meansVolrel,SDVolrel)
    colnames(dataVolrel)[length(dataVolrel)-1]<-paste("meansVolrel",z,s,sep="")
    colnames(dataVolrel)[length(dataVolrel)]<-paste("SDVolrel",z,s,sep="")
    dataVol2020<-cbind(dataVol2020,meansVol2020,SDVol2020)
    colnames(dataVol2020)[length(dataVol2020)-1]<-paste("meansVol2020",z,s,sep="")
    colnames(dataVol2020)[length(dataVol2020)]<-paste("SDVol2020",z,s,sep="")
    
      }
  
  #dataVolrelRGI20<-dataVolrelRGI20+dataVolrel
  #dataVol2020RGI20<-dataVol2020RGI20+dataVol2020
  
  chemin_sortieREL_2<-paste(chemin_sortieREL,z,".csv",sep="")
  chemin_sortie2020_2<-paste(chemin_sortie2020,z,".csv",sep="")
  
  write.csv(dataVolrel,chemin_sortieREL_2)
  write.csv(dataVol2020,chemin_sortie2020_2)
  
}



#Relatif et relatif2020 pour RGI20

Year<-eval(parse(text = filesname[t]))$V1
dataVolrelRGI20<-data.frame(list(Year))
colnames(dataVolrelRGI20)[1]<-"Year" 
dataVol2020RGI20<-data.frame(list(Year))
colnames(dataVol2020RGI20)[1]<-"Year" 

for (s in unique(str_sub(tabVolrel$model,start=-6))){
  
  newtabrel<-(tabVolrel[str_sub(tabVolrel$model,start =10,end=14)!="RGI20" & str_sub(tabVolrel$model,start=-6)==s,])
  newtab2020<-(tabVol2020[str_sub(tabVol2020$model,start =10,end=14)!="RGI20" & str_sub(tabVol2020$model,start=-6)==s,])
  dataVolRGI20
  volLac<-eval(parse(text =paste("dataVolRGI20$meansVolRGI20",s,sep="")))
  overView<-read.csv(paste(chemin_averaged,"RGI20/overview_RGI20_",s,".dat",sep=""), sep="", header=F, skip = 3)
  meansVolrelRGI20<-(volLac-volLac[3])/(overView$V7*0.9)*100
  meansVol2020RGI20<-(volLac-volLac[3])/(overView$V7[3]*0.9)*100
  
  
  #meansVolrelRGI20<-colMeans(newtabrel[-1],na.rm=T)
  SDVolrelRGI20<-colSds(as.matrix(newtabrel[-1]),na.rm=T)
  dataVolrelRGI20<-cbind(dataVolrelRGI20,meansVolrelRGI20,SDVolrelRGI20)
  
  #meansVol2020RGI20<-colMeans(newtab2020[-1],na.rm=T)
  SDVol2020RGI20<-colSds(as.matrix(newtab2020[-1]),na.rm=T)
  dataVol2020RGI20<-cbind(dataVol2020RGI20,meansVol2020RGI20,SDVol2020RGI20)
  
}


colnames(dataVolrelRGI20)<-c("Year","meansVolrelRGI20ssp126","SDVolrelRGI20ssp126","meansVolrelRGI20ssp245","SDVolrelRGI20ssp245","meansVolrelRGI20ssp370","SDVolrelRGI20ssp370","meansVolrelRGI20ssp585","SDVolrelRGI20ssp585","meansVolrelRGI20ssp119","SDVolrelRGI20ssp119")
write.csv(dataVolrelRGI20,chemin_sortieREL_RGI20)
colnames(dataVol2020RGI20)<-c("Year","meansVol2020RGI20ssp126","SDVol2020RGI20ssp126","meansVol2020RGI20ssp245","SDVol2020RGI20ssp245","meansVol2020RGI20ssp370","SDVol2020RGI20ssp370","meansVol2020RGI20ssp585","SDVol2020RGI20ssp585","meansVol2020RGI20ssp119","SDVol2020RGI20ssp119")
write.csv(dataVol2020RGI20,chemin_sortie2020_RGI20)



