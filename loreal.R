### This script is to get L'Oreal reviews data from Tmall.com

library(XML)
library(rvest)
library(stringr)
library(stringi)
library(jsonlite)
library(dplyr)

## Please mannualy input the following:
productName = 'Missha'
itemID = 528623332362
sellerID = 290850337
reviewNum = 153

## Resetting
# calculating # of pages
if (reviewNum %% 20 == 0) { 
      pageNum = reviewNum / 20
} else {
      pageNum = floor(reviewNum/20)+1
}
# function to extract and paste in to 1 vector from a list
extract <- function(fromPlace) {
      len = length(fromPlace)
      storePlace = c()
      for (index in 1:len) {
            storePlace[index] = paste(fromPlace[[index]], collapse = '')
      }
      return(storePlace)
}
# others
all_data = c()
time.now = Sys.time()

## Running
for (i in 1:pageNum) {
      
      print(i) # for tracking
      
      # figuring the page that contains review data
      fileUrl <- paste("https://rate.tmall.com/list_detail_rate.htm?itemId=",
                       itemID,"&sellerId=",sellerID,"&currentPage=", i, sep = "")
      
      # extracting and cutting into json type
      web = paste0(iconv(readLines(fileUrl), from = "gbk", to = "utf8"), collapse = "\n")
      json_data = str_extract(web, '\"rateList.*\"tags\"')
      json_data = gsub('\"rateList\":','',json_data)
      json_data = gsub(',\"search.*','',json_data)
      
      # cleaning
      current_data = fromJSON(json_data)
      current_data = cbind.data.frame(current_data, current_data$attributesMap$wap,
                                      current_data$attributesMap$ttid,
                                      current_data$attributesMap$tmall_vip_level)
#                                      current_data$attributesMap$sku)
      current_data$attributesMap = NULL
      current_data$structuredRatelist = extract(current_data$structuredRateList)
      current_data$structuredRateList = NULL
      current_data$picture = extract(current_data$pics)
      current_data$pics = NULL
      current_data$attributes = NULL
      
      # extracting append content
      for (index in 1:length(current_data$appendComment)){
            if ('content' %in% names(current_data$appendComment[[index]])) {
                  current_data$appendTime[index] = current_data$appendComment[[index]]$commentTime
                  current_data$appendContent[index] = current_data$appendComment[[index]]$content
                  current_data$appendDays[index] = current_data$appendComment[[index]]$days
                  current_data$appendPics[index] = current_data$appendComment[[index]]$pics
                  current_data$appendReply[index] = current_data$appendComment[[index]]$reply
            }
            else {
                  current_data$appendTime[index] = NA
                  current_data$appendContent[index] = NA
                  current_data$appendDays[index] = NA
                  current_data$appendPics[index] = NA
                  current_data$appendReply[index] = NA
            }
      }
      current_data$appendComment = NULL

      # combining
      all_data = rbind.data.frame(all_data, current_data)
      
      # calculating time consumed
      print(Sys.time() - time.now)
      
      # anti-anti-spider
      print('Holding...')
      Sys.sleep(runif(1,10,10.5))
}

## Outputting
write.csv(all_data, file = paste0(productName, '.csv'), fileEncoding = 'gbk')

################################################################################
##
## Testing
backup = backup[!duplicated(backup$rateDate),]
all_data = backup
write.csv(all_data, file = 'Etude_House.csv', fileEncoding = 'gbk')

