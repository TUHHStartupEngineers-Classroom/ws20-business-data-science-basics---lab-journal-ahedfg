
library(XML)
library(data.table)
library(stringr)

BOK_urllist <- list()
BOK_urllist[[1]] <- "http://ecos.bok.or.kr/api/KeyStatisticList/QM0K0ZGLSB6BKFKTGJEZ/xml/kr/1/101/"

BOK_raw.data <- list()
BOK_rootNode <- list()
BOK_raw.data[[1]] <- xmlTreeParse(BOK_urllist[1], useInternalNodes = TRUE,encoding = "utf-8") # 생성한 URL 대로 XML 을 요청
BOK_rootNode[[1]] <- xmlRoot(BOK_raw.data[[1]])

BOK_total <- list()

for(i in 1:length(BOK_urllist)){
  
  BOK_item <- list()
  BOK_item_temp_dt <- data.table()
  
  BOK_items <- BOK_rootNode[[i]]
  
  BOK_size <- xmlSize(BOK_items) 
  
  for(j in 1:BOK_size){
    BOK_item_temp <- xmlSApply(BOK_items[[j]],xmlValue)
    BOK_item_temp_dt <- data.table(VAR1=BOK_item_temp[1],
                                   VAR2=BOK_item_temp[2],
                                   VAR3=BOK_item_temp[3],
                                   VAR4=BOK_item_temp[4],
                                   VAR5=BOK_item_temp[5])
    
    BOK_item[[j]]<-BOK_item_temp_dt
  }
  BOK_total[[i]] <- rbindlist(BOK_item)
}

RESULTS <- rbindlist(BOK_total)
View(RESULTS)
write.csv(RESULTS, "openAPIData.csv",row.names = FALSE)