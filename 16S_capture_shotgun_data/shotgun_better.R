library(ggplot2)
summaryCap<-read.csv2("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/16S_16SCap_summary.csv",stringsAsFactors = FALSE,header=TRUE)
namesCap<-(colnames(summaryCap))
rownames(summaryCap)<-summaryCap[,1]
summaryCap<-summaryCap[,-1]
 mean(unlist(summaryCap[1,]/summaryCap[6,]))
 median(unlist(summaryCap[1,]/summaryCap[6,]))
 min(unlist(summaryCap[1,]/summaryCap[6,]))
 max(unlist(summaryCap[1,]/summaryCap[6,]))
fractionEukCap<-as.numeric(summaryCap[2,])/as.numeric(summaryCap[6,])
fractionVerCap<-as.numeric(summaryCap[3,])/as.numeric(summaryCap[6,])
fractionBacCap<-as.numeric(summaryCap[4,])/as.numeric(summaryCap[6,])
fractionUnassCap<-as.numeric(summaryCap[5,])/as.numeric(summaryCap[6,])
namesCap
namesSG
namesCap<-(colnames(summaryCap))
summaryCap<-read.csv2("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/16S-16S_SG.csv",stringsAsFactors = FALSE,header=TRUE)
namesSG<-(colnames(summaryCap))
rownames(summaryCap)<-summaryCap[,1]
summaryCap<-summaryCap[,-1]

mean(as.numeric(unlist(summaryCap[5,]))/as.numeric(unlist(summaryCap[6,])))
median(as.numeric(unlist(summaryCap[5,]))/as.numeric(unlist(summaryCap[6,])))
max(as.numeric(unlist(summaryCap[5,]))/as.numeric(unlist(summaryCap[6,])))
min(as.numeric(unlist(summaryCap[5,]))/as.numeric(unlist(summaryCap[6,])))

fractionEukSG<-as.numeric(summaryCap[1,])/as.numeric(summaryCap[5,])
fractionVerSG<-as.numeric(summaryCap[2,])/as.numeric(summaryCap[5,])
fractionBacSG<-as.numeric(summaryCap[3,])/as.numeric(summaryCap[5,])
fractionUnassSG<-as.numeric(summaryCap[4,])/as.numeric(summaryCap[5,])
bacteriaEfficancy<-fractionBacCap/fractionBacSG
data<-as.data.frame(namesCap[-1],namesCap[-1])
data<-cbind(data,bacteriaEfficancy)
data
disolution<-data[c(11,6:2),]
disolution$bacteriaEfficancy
blocking_oligos<-data[c(11,7:10),]
human_samples<-data[c(1,12:21),]
human_samples

blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "black", size = 20)
disolution<-cbind(disolution,c(1:6))
colnames(disolution)<-c("namesCap","bacteriaEfficiancy","efficiancy")
disolution$efficiancy<-as.factor(disolution$efficiancy)
levels(disolution$efficiancy)<-disolution$namesCap
levels(disolution$efficiancy)<-c("AT_Mock_1:50","AT_Mock_1:100","AT_Mock_1:1000","AT_Mock_1:10000","AT_Mock_1:100000","AT_Mock_1:1000000")
disolution<-cbind(disolution,log2(disolution$bacteriaEfficiancy))
colnames(disolution)<-c("namesCap","bacteriaEfficiancy","efficiancy","logEfficiancy")
disolution
p<-ggplot(disolution,aes(x= efficiancy,y=bacteriaEfficiancy)) + geom_col()+ theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Capture Efficiency")+xlab("")+  theme(text = element_text(size=18))
p
ggsave(filename=paste("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/Efficancy_Disolution_series",".pdf",sep=""))
p<-ggplot(disolution,aes(x= efficiancy,y=logEfficiancy)) + geom_col()+ theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Capture Efficiency in log2")+xlab("")+  theme(text = element_text(size=18))
p
ggsave(filename=paste("/Users/huebler/Desktop/16S_capture_shotgun_data/Efficancy_Disolution_series_log",".pdf",sep=""))


blocking_oligos<-cbind(blocking_oligos,c(1:5))
colnames(blocking_oligos)<-c("namesCap","bacteriaEfficiancy","efficiancy")
blocking_oligos<-blocking_oligos[c(1,3:5,2),]
blocking_oligos$namesCap<- c("bo_1","bo_1_100","bo_1_10000","bo_1_100000","bo_none")
blocking_oligos$efficiancy<-as.factor(1:5)
levels(blocking_oligos$efficiancy)<-blocking_oligos$namesCap
blocking_oligos<-cbind(blocking_oligos,log2(blocking_oligos$bacteriaEfficiancy))
colnames(blocking_oligos)<-c("namesCap","bacteriaEfficiancy","efficiancy","logEfficiancy")
p<-ggplot(blocking_oligos,aes(x= efficiancy,y=logEfficiancy)) + geom_col()+theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Capture Efficiency\n in log2")+xlab("")+  theme(text = element_text(size=18))
p
ggsave(filename=paste("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/Efficancy_Blocking_oligos_series",".pdf",sep=""))
p<-ggplot(blocking_oligos,aes(x=  efficiancy,y=bacteriaEfficiancy)) + geom_col()+theme_minimal()+theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Capture Efficiency")+xlab("")+  theme(text = element_text(size=18))
p
ggsave(filename=paste("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/Efficancy_Blocking_oligos_series_log2",".pdf",sep=""))

human_samples<-cbind(human_samples,c(1:11))
human_samples<-cbind(human_samples,log2(human_samples$bacteriaEfficancy))
colnames(human_samples)<-c("namesCap","bacteriaEfficiancy","efficancy","logEfficancy")
human_samples$efficancy<-as.factor(human_samples$efficancy)
levels(human_samples$efficancy)<-human_samples$namesCap

levels(human_samples$efficancy)<-c("Adipose_Tissue_Control","Hepatology_High_1", "Hepatology_High_2","Hepatology_Low_1", "Hepatology_Low_2", "patient_1_AT", "patient_1_blood","patient_1_stool","patient_2_AT", "patient_2_blood","patient_2_stool")
p<-ggplot(human_samples,aes(x= efficancy,y=logEfficancy))+ scale_fill_brewer(palette="Dark2") + geom_col()+theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Capture Efficiency in log2")+xlab("")+  theme(text = element_text(size=18))
p
ggsave(filename=paste("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/Efficancyhuman_series_inlog2",".pdf",sep=""))
p<-ggplot(human_samples,aes(x= efficancy,y=bacteriaEfficiancy))+ scale_fill_brewer(palette="Dark2") + geom_col()+theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Capture Efficiency")+xlab("")+  theme(text = element_text(size=18))
p

ggsave(filename=paste("/Users/huebler/Desktop/16S-Capture/16S_capture_shotgun_data/Efficancyhuman_series",".pdf",sep=""))
