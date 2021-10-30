getCorrelation<-function(path){
data<-as.data.frame(read.delim2(path,stringsAsFactors = FALSE),rownames=1)
contents<-c("Acinetobacter_baumannii","Actinomyces_odontolyticus","Bacillus_cereus",
            "Bacteroides_vulgatus","Clostridium_beijerinckii","Deinococcus_radiodurans",
            "Enterococcus_faecalis","Escherichia_coli","Helicobacter_pylori","Lactobacillus_gasseri",
            "Listeria_monocytogenes","Neisseria_meningitidis","Propionibacterium_acnes",
            "Pseudomonas_aeruginosa","Rhodobacter_sphaeroides","Staphylococcus_aureus",
            "Staphylococcus_epidermidis","Streptococcus_agalactiae","Streptococcus_mutans","Streptococcus_pneumoniae")
data<-data[-1,]
fractions<-c(8.2,1.0,45,0.8,44,1.0,0.7,680,8.6,3.2,5.0,5.8,8.8,160,1400,59,510,32,420,0.6)
correlations<-c()
columns<-c()
k<-2
  while(k<=length(colnames(data))){
  numberReads<-c()
  i<-1
  while(i<=length(contents)){
    numberReads<-c(numberReads,sum(data[grep(contents[i], data$Node_Name),k]))
    i<-i+1
  }

  correlations<-c(correlations, cor(numberReads,fractions))
  columns<-c(columns,colnames(data)[k])
  k<-k+1
  }
  return(cbind(columns,round(correlations,2)))
}


correlations<-, #CAP_SILVA
getCorrelation("/Users/huebler/Desktop/16S-Capture/Assigned_reads_perTax_sg/ScanSummarySG.txt"), #SG_Silva
getCorrelation("~/Desktop/16S-Capture/16S_capture_shotgun_data/ScanSummary.txt"))) #SG_FNT
correlations
correlations<-cbind(colnames(data)[-1],correlations)

SILVA_cap<-as.data.frame(cbind(getCorrelation("/Users/huebler/Desktop/16S-Capture/Assigned_reads_perTax_cap/ScanSummary_Capture.txt")))
View(SILVA_cap)
SILVA_SG<-getCorrelation("/Users/huebler/Desktop/16S-Capture/Assigned_reads_perTax_sg/ScanSummarySG.txt")               
View(SILVA_SG)
FullNT_SG<-getCorrelation("~/Desktop/16S-Capture/16S_capture_shotgun_data/ScanSummary.txt")
View(FullNT_SG)
