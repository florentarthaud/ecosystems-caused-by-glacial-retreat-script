library(stringr)
library(matrixStats)

chemin_averaged<-("../data_analyse/regional_data/averaged/")
chemin_RGI20<-("../data_analyse/regional_data/averaged/RGI20/")
chemin_STD_RGI20<-("../data_analyse/regional_data/averaged/RGI20/")

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
Region<-Region[Region!="RGI20"]
Scenario<-unique(str_sub(filesname,start=-6))



zonefiles= list.files(chemin_averaged)                                                        
zonefiles=zonefiles[zonefiles!="RGI20"]
chemin_zonefiles<-paste(chemin_averaged,zonefiles,"/",sep="")

variable<-c("elevation_deglacarea","elevation_glarea","elevation_glvolume","elevation_lakevolume","habitat","lakearea_depth","lakenumber_lakearea",
"MAAT_deglacarea","MAAT_lakearea","overview","slope_deglacarea","slope_proglacialarea")


for (v in variable){

filesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx(paste(v,"*.dat",sep="")))
STDfilesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx(paste("STD_",v,"*.dat",sep="")))
filestot<-c()
filesname<-c()
for (z in zonefiles){
  filesname<-c(filesname,  str_sub(list.files(paste(chemin_averaged,z,"/",sep=""),pattern=glob2rx(paste(v,"*.dat",sep=""))),end =-5))
  filestot<-c(filestot,paste(chemin_averaged,z,"/", list.files(paste(chemin_averaged,z,"/",sep=""),pattern=glob2rx(paste(v,"*.dat",sep=""))), sep = ""))
}







fichfich<-read.csv(filestot[1], sep="", header=F, skip = 3)
#v="overview"
#r="RGI12"
#s="ssp119"
for (s in  Scenario){ 
 
  G20tab<-data.frame(matrix(NA,nrow=0,ncol=length(fichfich),dimnames = list(c(),colnames(fichfich))))
  G20tab_SD<-data.frame(matrix(NA,nrow=0,ncol=length(fichfich),dimnames = list(c(),colnames(fichfich))))
for(r in Region){    
  assign(paste("Res_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/",v,"_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
 rajout<-cbind((eval(parse(text = paste("Res_",s,sep="")))))

 if (v!="MAAT_lakearea"){
   assign(paste("Std_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_",v,"_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
   rajout_SD<-cbind((eval(parse(text = paste("Std_",s,sep="")))))
 }
 if (v=="MAAT_lakearea"){
 assign(paste("Std_",s,sep=""),read.csv(paste(chemin_averaged,zonefiles[which(Region==r)],"/STD_MAAT_delakearea_",r,"_",s,".dat",sep=""), sep="", header=F, skip = 3))
    rajout_SD<-cbind((eval(parse(text = paste("Std_",s,sep="")))))
 }
 
 
        G20tab<-rbind(G20tab,rajout)
        G20tab_SD<-rbind(G20tab_SD,rajout_SD)
}  
 
  if (v!="overview"){
    G20tabS<-aggregate(. ~ V1, G20tab, sum)  
    G20tabS_SD<-aggregate(. ~ V1, G20tab_SD, sum)    
  }
  length(G20tab)
  if (v=="overview"){   
G20tabS1<-aggregate(. ~ V1, G20tab[,1:13], sum)
G20tabS2<-aggregate(. ~ V1, G20tab[,c(1,14)], mean)
G20tabS3<-aggregate(. ~ V1, G20tab[,c(1,15:length(G20tab))], sum)
G20tabS_SD1<-aggregate(. ~ V1, G20tab_SD[,1:13], sum)
G20tabS_SD2<-aggregate(. ~ V1, G20tab_SD[,c(1,14)], mean)
G20tabS_SD3<-aggregate(. ~ V1, G20tab_SD[,c(1,15:length(G20tab))], sum)
G20tabS<-cbind(G20tabS1,G20tabS2,G20tabS3[,-1])
G20tabS<-G20tabS[-14]
G20tabS_SD<-cbind(G20tabS_SD1,G20tabS_SD2,G20tabS_SD3[,-1])
G20tabS_SD<-G20tabS_SD[-14]
  }
  

G20deb<-data.frame(matrix(NA,nrow=2,ncol=length(fichfich),dimnames = list(c(0,0),colnames(fichfich))))
G20deb_SD<-data.frame(matrix(NA,nrow=2,ncol=length(fichfich),dimnames = list(c(0,0),colnames(fichfich))))
G20tabS<-rbind(G20deb,G20tabS)
G20tabS_SD<-rbind(G20deb_SD,G20tabS_SD)


chemin_sortie<-paste(chemin_STD_RGI20,v,"_RGI20_",s,".dat",sep="")
write.table(G20tabS,chemin_sortie, row.names=FALSE)

if (v!="MAAT_lakearea"){
STD_chemin_sortie<-paste(chemin_STD_RGI20,"STD_",v,"_RGI20_",s,".dat",sep="")
write.table(G20tabS_SD,STD_chemin_sortie, row.names=FALSE)  
}
if (v=="MAAT_lakearea"){
  STD_chemin_sortie<-paste(chemin_STD_RGI20,"STD_MAAT_delakearea_RGI20_",s,".dat",sep="")
  write.table(G20tabS_SD,STD_chemin_sortie, row.names=FALSE)  
  }    
}
}

