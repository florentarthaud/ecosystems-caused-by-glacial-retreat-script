library(stringr)
library(matrixStats)

chemin_aggregated<-("../data_analyse/regional_data/aggregated/")
chemin_RGI20<-("../data_analyse/regional_data/aggregated/RGI20/")
chemin_STD_RGI20<-("../data_analyse/regional_data/aggregated/RGI20/")

zonefiles= list.files(chemin_aggregated)                                                        
chemin_zonefiles<-paste(chemin_aggregated,zonefiles,"/",sep="")
filesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx("overview*.dat"))
STDfilesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx("STD_overview*.dat"))
filestot<-c()
filesname<-c()

for (z in zonefiles){
  filesname<-c(filesname,  str_sub(list.files(paste(chemin_aggregated,z,"/",sep=""),pattern=glob2rx("overview*.dat")),end =-5))
  filestot<-c(filestot,paste(chemin_aggregated,z,"/", list.files(paste(chemin_aggregated,z,"/",sep=""),pattern=glob2rx("overview*.dat")), sep = ""))
}
Region<-unique(str_sub(filesname,start =10,end=14))
Region<-Region[Region!="RGI20"]
Region<-Region[Region!="RGI 9"]
Region<-Region[Region!="RGI 6"]
Scenario<-unique(str_sub(filesname,start=-6))
Modele<-unique(str_sub(filesname,start =16,end=-8))
fichfich<-read.csv(filestot[1], sep="", header=F, skip = 3)

  
zonefiles= list.files(chemin_aggregated)                                                        
zonefiles=zonefiles[zonefiles!="RGI20"]


for (m in Modele){
  
  filesaveraged<-list.files(chemin_zonefiles, pattern=glob2rx(paste("overview*",m,"*.dat",sep="")))
  
  

  for (s in  Scenario){ 
  
    G20tab<-data.frame(matrix(NA,nrow=0,ncol=length(fichfich),dimnames = list(c(),colnames(fichfich))))
    
    for(r in Region){  
      
      if (length(list.files(chemin_zonefiles, pattern=glob2rx(paste("overview_",r,"_",m,"_",s,".dat",sep=""))))>0){
      
      rajout<-read.csv(paste(chemin_aggregated,zonefiles[which(Region==r)],"/overview_",r,"_",m,"_",s,".dat",sep=""), sep="", header=F, skip = 3)
      }
      if (length(list.files(chemin_zonefiles, pattern=glob2rx(paste("overview_",r,"_",m,"_",s,".dat",sep=""))))==0){
        
        rajout<-c()
        }
      
      G20tab<-rbind(G20tab,rajout)
      
    }  
    
    if (length(G20tab[,1])>0){
    G20tabS<-aggregate(. ~ V1, G20tab, sum)
    G20deb<-data.frame(matrix(NA,nrow=2,ncol=length(fichfich),dimnames = list(c(0,0),colnames(fichfich))))
    G20tabS<-rbind(G20deb,G20tabS)

    chemin_sortie<-paste(chemin_STD_RGI20,"overview_RGI20_",m,"_",s,".dat",sep="")
    write.table(G20tabS,chemin_sortie, row.names=FALSE)
    }
    
  }

}
