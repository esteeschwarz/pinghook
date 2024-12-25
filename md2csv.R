library(readr)

md_file <- "/Users/guhl/Documents/GitHub/pinghook/pinghook.md"

md_content <- readLines(md_file)
md_content
links <- regmatches(md_content, gregexpr("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", md_content))
links <- regmatches(md_content, gregexpr("\\[(.+)\\]\\((.+)\\)(.+)", md_content))
#linksrep<-
links[25]
l1<-strsplit(links[[25]],"]\\(")
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
link_data <- data.frame(
  h1 = gsub("\\[(.+)\\]\\(([^\\)].+)\\)(.+)", "\\1", links),
  url = gsub("\\[(.+)\\]\\((.+)\\)(.+)", "<\\2>", links),
  desc = gsub("\\[(.+)\\]\\((.+)\\)(.+)", "\\3", links),
  stringsAsFactors = FALSE
)
#link_data$further =  gsub("\\[(.+)\\]\\((.+)\\)(.+)", "\\2", link_data$desc)

csv_file <- "/Users/guhl/Documents/GitHub/pinghook/links.csv"

write_csv(link_data, csv_file)
