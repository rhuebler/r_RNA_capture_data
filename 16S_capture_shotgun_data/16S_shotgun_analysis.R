### Analyze 16S Shotgun data
library(ggplot2)
#################
### 16S Shotgun #
#################

# 16S Summary Silva
getSGFractions<-function(){
summarySG<-read.delim("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/16S_summary.txt",stringsAsFactors = FALSE,header=TRUE)
summarySG<-summarySG[,-7]
rownames(summarySG)<-summarySG[,1]
summarySG<-summarySG[,-1]
summarySG<-summarySG[,-6]
fractionBac<-summarySG$Bacteria/summarySG$Total
fractionVer<-summarySG$Vertebrata/summarySG$Total
fractionEuk<-summarySG$Eukaryota/summarySG$Total
fractionUnass<-summarySG$Unassigned/summarySG$Total
namesSG<-rownames(summarySG)
dataSG_var<-round(c(fractionBac,fractionVer,fractionEuk,fractionUnass),4)
dataSG_var
factors<-c(rep("Bacteria",length(namesSG)),rep("Vertebrata",length(namesSG)),rep("Eukaryota",length(namesSG)),rep("Unassigned",length(namesSG)))
dataSG<-as.data.frame(cbind(factors,rep(namesSG,4)))
dataSG<-cbind(dataSG,dataSG_var)
colnames(dataSG)<-c("type","Filename","fraction")
dataSG$fraction<-as.double(dataSG$fraction)
return(dataSG)
}
dataSG<-getSGFractions

p<-ggplot(dataSG,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot",".pdf",sep=""))


### 16S 16S Data
function(){
summaryCap<-read.csv2("/Users/huebler/Desktop/16S_capture_shotgun_data/16S-16S_SG.csv",stringsAsFactors = FALSE,header=TRUE)
rownames(summaryCap)<-summaryCap[,1]
summaryCap<-summaryCap[,-1]
summaryCap
fractionEukSG<-as.numeric(summaryCap[1,])/as.numeric(summaryCap[5,])
fractionVerSG<-as.numeric(summaryCap[2,])/as.numeric(summaryCap[5,])
fractionBacSG<-as.numeric(summaryCap[3,])/as.numeric(summaryCap[5,])
fractionUnassSG<-as.numeric(summaryCap[4,])/as.numeric(summaryCap[5,])

namesSG<-(colnames(summaryCap))
dataSG_var<-(c(c(fractionBacSG),c(fractionVerSG),c(fractionEukSG),c(fractionUnassSG)))
length(namesSG)
factors<-c(rep("Bacteria",length(namesSG)),rep("Vertebrata",length(namesSG)),rep("Eukaryota",length(namesSG)),rep("Unassigned",length(namesSG)))
data16S_SG<-as.data.frame(cbind(factors,rep(namesSG,4)))
data16S_SG<-cbind(data16S_SG,dataSG_var)

colnames(data16S_SG)<-c("type","Filename","fraction")
return()
}

p<-ggplot(data16S_SG,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylim(0,1)
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot_capture16S_SG",".pdf",sep=""))

data16S_SG[is.na(data16S_SG)]<-0
mean(data16S_SG[data16S_SG$type=='Bacteria',]$fraction)
median(data16S_SG[data16S_SG$type=='Bacteria',]$fraction)
sd(data16S_SG[data16S_SG$type=='Bacteria',]$fraction)

#16S Shotgun FullNT

summaryCap<-read.csv2("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_SGFullNT.csv",stringsAsFactors = FALSE,header=TRUE)
summaryCap<-summaryCap[1:6,]
rownames(summaryCap)<-summaryCap[,1]
summaryCap<-summaryCap[,-1]

fractionEukSG<-as.numeric(summaryCap[1,])/as.numeric(summaryCap[5,])
fractionVerSG<-as.numeric(summaryCap[2,])/as.numeric(summaryCap[5,])
fractionBacSG<-as.numeric(summaryCap[3,])/as.numeric(summaryCap[5,])
fractionUnassSG<-as.numeric(summaryCap[4,])/as.numeric(summaryCap[5,])

namesSG<-(colnames(summaryCap))
dataSG_var<-(c(c(fractionBacSG),c(fractionVerSG),c(fractionEukSG),c(fractionUnassSG)))
length(namesSG)
factors<-c(rep("Bacteria",length(namesSG)),rep("Vertebrata",length(namesSG)),rep("Eukaryota",length(namesSG)),rep("Unassigned",length(namesSG)))
data16S_NT<-as.data.frame(cbind(factors,rep(namesSG,4)))
data16S_NT<-cbind(data16S_NT,dataSG_var)
colnames(data16S_NT)<-c("type","Filename","fraction")
p<-ggplot(data16S_NT,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylim(0,1)
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot_capture_SG_FNT",".pdf",sep=""))


Fracton16S_SG<-cbind(data16S_NT[,-3],c(data16S_SG$fraction/data16S_NT$fraction))
colnames(Fracton16S_SG)<-c("type","Filename","fraction")
Fracton16S_SG[is.na(Fracton16S_SG)]<-0

p<-ggplot(Fracton16S_SG,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylim(0,1)
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot_captureSG_FractionOF16S",".pdf",sep=""))

mean(Fracton16S_SG[Fracton16S_SG$type=='Bacteria',]$fraction)
median(Fracton16S_SG[Fracton16S_SG$type=='Bacteria',]$fraction)
sd(Fracton16S_SG[Fracton16S_SG$type=='Bacteria',]$fraction)
library(ggplot2)
#Data 16S capture FullNT
summaryCap<-read.csv2("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/FullNTCap.csv",stringsAsFactors = FALSE,header=TRUE)
rownames(summaryCap)<-summaryCap[,1]
summaryCap<-summaryCap[,-1]
fractionEukCap<-as.numeric(summaryCap[2,])/as.numeric(summaryCap[6,])
fractionVerCap<-as.numeric(summaryCap[3,])/as.numeric(summaryCap[6,])
fractionBacCap<-as.numeric(summaryCap[4,])/as.numeric(summaryCap[6,])
fractionUnassCap<-as.numeric(summaryCap[5,])/as.numeric(summaryCap[6,])
namesCap<-colnames(summaryCap)
dataCap_var<-(c(c(fractionBacCap),c(fractionVerCap),c(fractionEukCap),c(fractionUnassCap)))

factors<-c(rep("Bacteria",length(namesCap)),rep("Vertebrata",length(namesCap)),rep("Eukaryota",length(namesCap)),rep("Unassigned",length(namesCap)))
dataCapNT<-as.data.frame(cbind(factors,rep(namesCap,4)))
dataCapNT<-cbind(dataCapNT,dataCap_var)
colnames(dataCapNT)<-c("type","Filename","fraction")

p<-ggplot(dataCapNT,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylim(0,1)
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot_captureFNT",".pdf",sep=""),width=20, height=10)


#Data 16S capture 16SCapture
summaryCap<-read.csv2("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_16SCap_summary.csv",stringsAsFactors = FALSE,header=TRUE)
rownames(summaryCap)<-summaryCap[,1]
summaryCap<-summaryCap[,-1]
summaryCap
fractionEukCap<-as.numeric(summaryCap[2,])/as.numeric(summaryCap[6,])
fractionVerCap<-as.numeric(summaryCap[3,])/as.numeric(summaryCap[6,])
fractionBacCap<-as.numeric(summaryCap[4,])/as.numeric(summaryCap[6,])
fractionUnassCap<-as.numeric(summaryCap[5,])/as.numeric(summaryCap[6,])
namesCap<-(colnames(summaryCap[1,]))
dataCap_var<-(c(c(fractionBacCap),c(fractionVerCap),c(fractionEukCap),c(fractionUnassCap)))
length(namesCap)
factors<-c(rep("Bacteria",length(namesCap)),rep("Vertebrata",length(namesCap)),rep("Eukaryota",length(namesCap)),rep("Unassigned",length(namesCap)))
dataCap16S<-as.data.frame(cbind(factors,rep(namesCap,4)))
dataCap16S<-cbind(dataCap16S,dataCap_var)

plot(log2(c(fractionBacCap/fractionBacSG))[c(2:6,11)])
c(fractionBacCap/fractionBacSG)[c(2:6,11)]
namesCap[c(2:6,11)]
plot()
namesCap<-(colnames(summaryCap[1,]))
dataCap_var<-(c(c(fractionBacCap),c(fractionVerCap),c(fractionEukCap),c(fractionUnassCap)))
length(namesCap)
factors<-c(rep("Bacteria",length(namesCap)),rep("Vertebrata",length(namesCap)),rep("Eukaryota",length(namesCap)),rep("Unassigned",length(namesCap)))
dataCap16S<-as.data.frame(cbind(factors,rep(namesCap,4)))
dataCap16S<-cbind(dataCap16S,dataCap_var)
colnames(dataCap16S)<-c("type","Filename","fraction")
p<-ggplot(dataCap16S,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylim(0,1)
p

ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot_capture16S",".pdf",sep=""),width=20, height=10)
dataCap16S$fraction
dataCapNT$fraction
S_fraction<-dataCap16S$fraction/dataCapNT$fraction
Fracton16S_DB<-cbind(dataCap16S[,-3],S_fraction)
colnames(Fracton16S_DB)<-c("type","Filename","fraction")
p<-ggplot(Fracton16S_DB,aes(x=Filename,y=fraction,fill=type)) + geom_col(position="dodge")+facet_grid(~type)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylim(0,1.5)
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/16S_summary_plot_capture_fraction16S",".pdf",sep=""),width=20, height=10)

mean(Fracton16S_DB[Fracton16S_DB$type=='Bacteria',]$fraction)
median(Fracton16S_DB[Fracton16S_DB$type=='Bacteria',]$fraction)
sd(Fracton16S_DB[Fracton16S_DB$type=='Bacteria',]$fraction)   
