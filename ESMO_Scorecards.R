library(tidyr)
library(readr)
library(data.table)
library(rvest)
# Clean environment
rm(list = ls())

# Source any functions
source(paste(getwd(), "Utils.R", sep ="/"))

# Website to be scraped
url <- "https://www.esmo.org/guidelines/esmo-mcbs/esmo-mcbs-scorecards"

# Parse html code
webpage <- read_html(url)

# Max page number of website
max_page <- readr::parse_number(html_node(webpage, paste0(".page-link"))%>% html_nodes("span")%>%
                                  html_text())
raw_table <- list()

i <-1
# Get all page information
for( i in 1:max_page) {
  
  url <- paste0("https://www.esmo.org/guidelines/esmo-mcbs/esmo-mcbs-scorecards", "?page=", i)
  
  # Parse html code
  webpage <- read_html(url)
  
  info_table <- as.data.frame(html_table(webpage, '.col-xs-12',
                                        fill = TRUE,
                                        header = TRUE,
                                        trim = TRUE))  

  # Get list of reference publications
  mylist <- list()
  for(j in 1:nrow(info_table)){
    mylist[[j]] <- html_node(webpage, xpath = paste0('//*[@id="mcbs-view-table"]/tbody/tr[',j,']/td[10]'))%>%
      html_nodes("a") %>% html_attr("href")
  }
  
  # Table of references
  pubs <- rbindlist(lapply(mylist, function(x) as.data.table(matrix(x, nrow = 1))), fill = T)
  
  # get scorecards
  card_urls <- paste0("https://www.esmo.org", html_nodes(webpage, ".link-arrow-right")%>% html_attr("href"))
  
  card_final_table <- list()
  for (k in 1:length(card_urls)){
  
  card_page <- read_html(card_urls[k])
  
  card_table <- data.frame(column1 = html_nodes(card_page, "dt") %>% html_text(),
                           column2 = html_nodes(card_page, "dd") %>% html_text())
  card_final_table[[k]] <- spread(card_table, "column1", "column2")
  }
  card_final_table <- rbindlist(card_final_table, fill = T)
  
  raw_table[[i]] <- cbind(info_table, pubs, card_final_table)
  print(i)
}

final_table <- rbindlist(raw_table, fill = T)

write.csv(final_table, "/media/chris/DATA/Documents/Clinical_Data_Dashboard/ESMO_Scorecards.csv")
