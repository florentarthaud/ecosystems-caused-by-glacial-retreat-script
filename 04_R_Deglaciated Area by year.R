library(stringr)
library(matrixStats)

####///Deglaciated Area by yea ET SD####
chemin_aggregated<-("../data_analyse/regional_data/aggregated/")
zonefiles= list.files(chemin_aggregated)                                                        
chemin_zonefiles<-paste("../data_analyse/regional_data/aggregated/",zonefiles,"/",sep="")
filesaggregated<-list.files(chemin_zonefiles, pattern=glob2rx("overview*.dat"))[-c(1:14)]
filestot<-paste(rep(chemin_zonefiles, each = length(filesaggregated)), filesaggregated, sep = "")
filesname<-str_sub(str_replace_all(rep(filesaggregated, length(chemin_zonefiles)),"-","_"),end =-5)
chemin_sortie_daby<-("../data_analyse/regional_data/volume_overdpn/daby/")

# script si pour emp??cher que tout s'arr??te si il manque un ficher (mais normalement tout est ok maintenant)
for(f in 1:length(filestot)){
  if (file.exists(filestot[f])==T){
    t<-read.table(filestot[f], sep="", header=F, skip = 3)
    n<-filesname[f]
    assign(n,t)
  }   
}

model<-c(NA)
daby2000<-(NA)
daby2010<-(NA)
daby2020<-(NA)
daby2030<-(NA)
daby2040<-(NA)
daby2050<-(NA)
daby2060<-(NA)
daby2070<-(NA)
daby2080<-(NA)
daby2090<-(NA)
daby2100<-(NA)
tabdaby<-data.frame(list(model,daby2000,daby2010,daby2020,daby2030,daby2040,daby2050,daby2060,daby2070,daby2080,daby2090,daby2100))
colnames(tabdaby)<-c("model","daby2000","daby2010","daby2020","daby2030","daby2040","daby2050","daby2060","daby2070","daby2080","daby2090","daby2100")

for(t in 1:length(filestot)) {
  if (file.exists(filestot[t])==T){
    #calcdaby=(eval(parse(text = filesname[t]))$V5[2:12]*eval(parse(text = filesname[t]))$V14
    calcdaby=(eval(parse(text = filesname[t]))$V5[2:12]-eval(parse(text = filesname[t]))$V5[1:11])/10
    rajout<-c(filesname[t],calcdaby)
    tabdaby<-rbind(tabdaby,rajout)
  }
}

#tabdaby<-na.omit(tabdaby)#pas possible car derniere valeur vitesse forcement NA
tabdaby2<-tabdaby
i <- c(2:12)                                  # Specify columns you want to change
tabdaby[ , i] <- apply(tabdaby[ , i], 2,        # Specify own function within apply
                      function(x) as.numeric(as.character(x)))
#remettre volume en numeric

colnames(tabdaby)<-c("model","daby2000","daby2010","daby2020","daby2030","daby2040","daby2050","daby2060","daby2070","daby2080","daby2090","daby2100")

Year<-eval(parse(text = filesname[t]))$V1
datadaby<-data.frame(list(Year))
colnames(datadaby)[1]<-"Year"



Region<-unique(str_sub(tabdaby$model[-1],start =10,end=14))

for (z in unique(Region)){
  
  Year<-eval(parse(text = filesname[t]))$V1
  datadaby<-data.frame(list(Year))
  colnames(datadaby)[1]<-"Year"  
  
  for (s in unique(str_sub(tabdaby$model[-1],start=-6))){
    
    newtab<-(tabdaby[str_sub(tabdaby$model,start =10,end=14)==z & str_sub(tabdaby$model,start=-6)==s,])
   
    meansdaby<-colMeans(newtab[-1],na.rm=T)
    SDdaby<-colSds(as.matrix(newtab[-1]),na.rm=T)
    trenddaby<-meansdaby[3]-meansdaby

    datadaby<-cbind(datadaby,meansdaby,trenddaby,SDdaby)
    colnames(datadaby)[length(datadaby)-2]<-paste("meansdaby",z,s,sep="")
    colnames(datadaby)[length(datadaby)-1]<-paste("trenddaby",z,s,sep="")
    colnames(datadaby)[length(datadaby)]<-paste("SDdaby",z,s,sep="")
  }
  

  chemin_sortie<-paste(chemin_sortie_daby,"daby_",z,".csv",sep="")
  
  write.csv(datadaby,chemin_sortie)
  
}

