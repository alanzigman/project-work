#Pulling data from Basketball Reference

library(rvest)
library(readr)
library(lubridate)
library(tidyverse)

#Inputs
player <- c('Russell', 'Westbrook')
year <- 2020

#---
player_id <- paste(str_sub(tolower(player[2]),1,5),str_sub(tolower(player[1]),1,2),'01',sep="")
letter <- str_sub(tolower(player[2]),1,1)
url <- paste('https://www.basketball-reference.com/players/',letter,'/',player_id,'/gamelog/',year,sep="")
webpage <- read_html(url)

col_names <- webpage %>% 
  html_nodes("table#pgl_basic > thead > tr > th") %>% 
  html_attr("data-stat")  
#Remove ranker column from headers as it does not appears in row by row data
col_names <- col_names[-1]

#Pull all data from table
data <- webpage %>% 
  html_nodes("table#pgl_basic > tbody > tr > td") %>% 
  html_text()
#Need to remove rows where DNP ('Did Not Play' or 'Inactive') to be able to fit to columns
data <- data[data != 'Did Not Play']

#Create matrix with the data
data <- matrix(data, ncol = length(col_names), byrow = TRUE)
  
#Convert the data frame with column headers
df <- as.data.frame((data), stringsAsFactors = FALSE)
names(df) <- col_names


#rows <- webpage %>% 
  html_nodes("table#pgl_basic > tbody > tr > th") %>% 
  html_text()
rows <- rows[c(1:20,51:70,101:120,151:154)]
rows <- rows[rows != "Did Not Play"]
rows


