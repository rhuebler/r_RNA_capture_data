library(getopt);
library(ggplot2)
raw<-as.data.frame(read.delim("/Users/huebler/Desktop/16S-Capture/analysis/SummaryHumanReads16S18S.txt",header=TRUE,row.names = 1,colnames(1)))

frac_sdna<- as.double(unlist(round(raw[3,]/raw[7,],4)*100))
frac_human<-as.double(unlist(round(raw[4,]/raw[7,],4)*100))
frac_mt<-as.double(unlist(round(raw[5,]/raw[7,],4)*100))
data<-cbind(rep(colnames(raw),3),c(rep('16S18SDNA',11),rep('Human_DNA',11),rep('MTDNA',11)))
data<-as.data.frame(data)
data<-cbind(data,c(frac_sdna,frac_human,frac_mt))

colnames(data)<-c("name","type","fraction")
rownames(data)<-c(1:33)
data$fraction<-as.double(data$fraction)
as.double(data$fraction)
data$fraction
data
p <- ggplot(data=data, aes(x=name, y=fraction, fill=type)) +geom_bar(stat="identity",color="black", position=position_dodge()) + scale_fill_brewer(palette="Reds")
p<-p+ theme_minimal() +theme(axis.text.x = element_text(size = 24) , axis.title=element_text(size=24,face="bold"), axis.text.y = element_text(size = 24) )
p<-p+ xlab("FileName") + ylab("Fraction Of Vertebrate Reads") + theme(text = element_text(size=24))+ theme(axis.text.x = element_text(angle =90, vjust = 1, size = 18, hjust = 1))
p <- p + guides(fill=guide_legend(title="DNA_Type")) + theme(strip.text.x = element_text(size = 18 ))+scale_y_continuous(breaks = c(0,2,4,6,8,10))
p
ggsave(filename=paste("/Users/huebler/Desktop/","humanReads_total",".pdf",sep=""),width=20, height=10)
