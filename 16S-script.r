silvaData<-read.delim(file="/Users/huebler/mount2/users/huebler/16SCapture/16SDB/maltExtract/ScanSummary.txt",header = 1,row.names = 1)
fullNTData<-read.delim(file="/Users/huebler/mount2/users/huebler/16SCapture/fullNTHuman/maltExtract/ScanSummary.txt",header = 1,row.names = 1,stringsAsFactors = FALSE)
silvaData<-read.delim(file="/Users/huebler/mount2/users/huebler/16SCapture/SILVA_2017/maltExtract/ScanSummary.txt",header = 1,row.names = 1)
columns<-c("AT_Mock_1_50_3.merged.fastq.rma6",	"AT_Mock_1_1000000.merged.fastq.rma6", "AT_Mock_1_50_4.merged.fastq.rma6",	
           "AT_Mock_1_10000.merged.fastq.rma6",	"AT_Mock_1_100.merged.fastq.rma6",	"AT_Mock_1_1000.merged.fastq.rma6",	
           "AT_Mock_1_50_2.merged.fastq.rma6",	"AT_Mock_1_100000.merged.fastq.rma6",	
           "AT_Mock_1_50.merged.fastq.rma6",	"patient_1_AT.merged.fastq.rma6",	"AT_Mock_1_50_1.merged.fastq.rma6")	
columns<-c("AT_Mock_1_50_3.merged.rma6",	"AT_Mock_1_1000000.merged.rma6", "AT_Mock_1_50_4.merged.rma6",	
           "AT_Mock_1_10000.merged.rma6",	"AT_Mock_1_100.merged.rma6",	"AT_Mock_1_1000.merged.rma6",	
           "AT_Mock_1_50_2.merged.rma6",	"AT_Mock_1_100000.merged.rma6",	
           "AT_Mock_1_50.merged.rma6",	"patient_1_AT.merged.rma6",	"AT_Mock_1_50_1.merged.rma6")	
silvaData<-silvaData[,columns]

colnames(fullNTData)
fullNTData<-fullNTData[,columns]

getContained(fullNTData)

getContained(silvaData)
getContained<-function(data){
  contained_names<-c()
  names<-row.names(data)
  k<-1
  while(k<=length(names)){
    i<-1
    contained = TRUE;
    line = data[names[k],]
    while(i<=length(line)){
      if(line[i] == 0){
        contained = FALSE
      }
      i<-i+1
    }
    if(contained){
      contained_names<-c(contained_names,names[k])
    }
    k<-k+1
  }
  return(contained_names)
}


getContainedNT<-function(data, names){
  contained_names<-c()
  k<-1
  while(k<=length(names)){
    i<-1
    contained = TRUE;
    line = data[names[k],]
    while(i<=length(line)){
      if(line[i] == 0){
        contained = FALSE
      }
      i<-i+1
    }
    if(contained){
      contained_names<-c(contained_names,names[k])
    }
    k<-k+1
  }
  return(contained_names)
}
