library(readr)

md_file <- "/Users/guhl/Documents/GitHub/pinghook/pinghook.md"

md_content <- readLines(md_file)
#md_content
#links <- regmatches(md_content, gregexpr("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", md_content))
links <- regmatches(md_content, gregexpr("\\[(.+)\\]\\((.+)\\)(.+)?", md_content))
#linksrep<-
links[11]
l1<-strsplit(links[[24]],"]\\(")
l1<-unlist(l1)
l1
l2<-data.frame(
  titles = gsub(".+?\\[(.+)", "\\1",l1),
  refs =   gsub("(.+)\\.+?","<\\1>",l1),
  desc =   gsub(".+?\\)(.+)", "\\1",l1),
  stringsAsFactors = FALSE)
l2
links <- unlist(links)
links <- links[links != ""]
links[305]
heads<-grep("#####",md_content)
heads
md_content[heads]
l3<-data.frame(title=NA,category=NA,desc=NA,url=NA,id=1:length(md_content))
heads1<-heads+1
heads2<-heads-1
heads2<-c(heads2[2:length(heads2)],length(md_content))
heads2
heads1
heads
#heads1<-heads1
heads2
heads1
k<-1
for(k in 1:length(heads1)){
trange<-heads1[k]:heads2[k]
tmd<-md_content[trange]
h1<-gsub("[# ]","",md_content[heads[k]])
l3$category[trange]<-h1
l3$title[trange] = gsub("\\[(.+)\\]\\(([^\\)].+)\\)(.+)?", "\\1", tmd)
l3$url[trange] = gsub("\\[(.+)\\]\\((https?:.+)\\)(.+)?", "<\\2>", tmd)
l3$desc[trange] = gsub("\\[(.+)\\]\\((.+)\\)(.+)?", "\\3", tmd)
}
sum(is.na(l3$url))
l3na<-l3[!is.na(l3$url),1:4]
l3nb<-l3na[l3na$url!="",]
l3nb$url<-gsub("^<","",l3nb$url)
l3nb$url<-gsub(">$","",l3nb$url)
l3nb<-l3nb[order(l3nb$title,decreasing = T),]
# link_data <- data.frame(
#   h1 = gsub("\\[(.+)\\]\\(([^\\)].+)\\)(.+)", "\\1", links),
#   url = gsub("\\[(.+)\\]\\((.+)\\)(.+)", "<\\2>", links),
#   desc = gsub("\\[(.+)\\]\\((.+)\\)(.+)", "\\3", links),
#   stringsAsFactors = FALSE
# )
#link_data$further =  gsub("\\[(.+)\\]\\((.+)\\)(.+)", "\\2", link_data$desc)
link_data<-l3nb
csv_file <- "/Users/guhl/Documents/GitHub/pinghook/ids-links.csv"
library(readr)
write.table(link_data,file = csv_file,quote = F,sep = ";",row.names = F)
#write_csv(link_data, csv_file)
