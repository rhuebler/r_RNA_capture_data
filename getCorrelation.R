#16S vapture correlation

directory<-"/Users/huebler/Desktop/16S-Capture/Assigned_reads_perTax_cap/ScanSummary_Capture.txt"
data<-as.data.frame(read.delim2(directory,stringsAsFactors = FALSE),rownames=1)
data<-data[-c(17924,17925),]
rownames(data)<-data$Node_Name

contents<-c("Acinetobacter_baumannii","Actinomyces_odontolyticus","Bacillus_cereus",
            "Bacteroides_vulgatus","Clostridium_beijerinckii","Deinococcus_radiodurans",
            "Enterococcus_faecalis","Escherichia_coli","Helicobacter_pylori","Lactobacillus_gasseri",
            "Listeria_monocytogenes","Neisseria_meningitidis","Propionibacterium_acnes",
            "Pseudomonas_aeruginosa","Rhodobacter_sphaeroides","Staphylococcus_aureus",
            "Staphylococcus_epidermidis","Streptococcus_agalactiae","Streptococcus_mutans","Streptococcus_pneumoniae")

data$Node_Name=="Propionibacterium"
pb<-data[c("Propionibacterium_acnes_JCM_18916","Propionibacterium_acnes_KPA171202"),]
pb<-c(pb[1,-1]+pb[2,-1])
data<-rbind(data,c("Propionibacterium_acnes",unlist(pb)))
rownames(data)<-data$Node_Name
numReadsCap<-data[contents,-1]
numReads<-rep(10000,length(contents))
fractions<-c(8.2,1.0,45,0.8,44,1.0,0.7,680,8.6,3.2,5.0,5.8,8.8,160,1400,59,510,32,420,0.6)
i<-1
correlations<-c()
while(i<22){
correlations<-c(correlations, cor(as.numeric(numReadsCap[,i]),fractions))
  i<-i+1
}
cbind(colnames(numReadsCap),fractions,round(correlations,2))
cbind(contents,fractions)

round(correlations,2)
directory<-"/Users/huebler/Desktop/16S-Capture/Assigned_reads_perTax_sg/ScanSummarySG.txt"
data<-as.data.frame(read.delim2(directory,stringsAsFactors = FALSE),rownames=1)
rownames(data)<-data$Node_Name
numReadsSG<-data[contents,-1]
numReadsSG<-numReadsSG[,-21]

i<-1
correlationsSG<-c()
while(i<=21){
  correlationsSG<-c(correlationsSG, cor(as.numeric(numReadsSG[,i]),fractions,use="na.or.complete"))
  i<-i+1
}
help(cor)
cbind(colnames(numReadsSG),round(correlationsSG))
