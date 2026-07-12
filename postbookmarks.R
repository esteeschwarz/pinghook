# idsL load, expo bookmarks, tunnel...
getwd()
setwd(paste0(Sys.getenv("GIT_TOP"),"/pinghook/"))

file.copy("~/Library/Containers/org.dh-index.IDS-Links/Data/Documents/ids-links-expo.csv", paste0(Sys.getenv("GIT_TOP"),"/pinghook/ids-links-expo.csv"),overwrite=T)

d1<-read.csv("ids-links-expo.csv")
d2<-read.csv("ids-links-sf.csv")
d3<-d1
colnames(d1)
colnames(d2)
for(k in 1:length(d3$url)){
  l<-d3$url[k]
  m<-d2$url==l
  n<-d2$notes[m]
  if(sum(m)>0)
    d3$notes[k]<-n
}
d3$notes<-gsub("^ ","",d3$notes)
write.csv(d3,"ids-links-expo.csv",row.names=F)

source(paste0(Sys.getenv("HKW_TOP"),"/R/maria_postbookmarks.R"))

