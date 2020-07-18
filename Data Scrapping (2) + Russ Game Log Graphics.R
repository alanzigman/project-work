#Pulling data from Basketball Reference

library(rvest)
library(readr)
library(lubridate)
library(tidyverse)

#### Inputs
player <- c('Russell', 'Westbrook')
year <- 2020

#-----
player_id <- paste(str_sub(tolower(player[2]),1,5),str_sub(tolower(player[1]),1,2),'01',sep="")
letter <- str_sub(tolower(player[2]),1,1)
url <- paste('https://www.basketball-reference.com/players/',letter,'/',player_id,'/gamelog/',year,sep="")
webpage <- read_html(url)

#Pull in data as a table
table <- webpage %>%
  html_nodes("table#pgl_basic") %>%
  html_table()
table <- table[[1]]
#Remove row every 20 that displays headers again
table <- table[c(-21,-42,-63,-84),]

#Remove DNPs from list
data <- table[which(table[,9] != 'Did Not Play' & table[,9] != 'Inactive' & table[,9] != 'Did Not Dress'),]

#Convert data to be used into numeric
data[,2] <- as.numeric(data[,2])
data[,11] <- as.numeric(data[,11])
data[,12] <- as.numeric(data[,12])
data[,13] <- as.numeric(data[,13])
data[,14] <- as.numeric(data[,14])
data[,15] <- as.numeric(data[,15])
data[,17] <- as.numeric(data[,17])
data[,18] <- as.numeric(data[,18])
data[,28] <- as.numeric(data[,28])
data[,29] <- as.numeric(data[,29])
data$Rk <- as.numeric(data$Rk)

str(data)

#Split data into pre Jan 1 and Post Jan 1
data_firsthalf <- data[which(data$Rk < 35),]
data_secondhalf <- data[which(data$Rk >= 35),]

#Compute first half and second half FG%, 3P%, FT%
firsthalf_fg <- sum(data_firsthalf[,11])/sum(data_firsthalf[,12])
firsthalf_3p <- sum(data_firsthalf[,14])/sum(data_firsthalf[,15])
firsthalf_ft <- sum(data_firsthalf[,17])/sum(data_firsthalf[,18])
secondhalf_fg <- sum(data_secondhalf[,11])/sum(data_secondhalf[,12])
secondhalf_3p <- sum(data_secondhalf[,14])/sum(data_secondhalf[,15])
secondhalf_ft <- sum(data_secondhalf[,17])/sum(data_secondhalf[,18])

#Visualizing the data
#Create new table - 5 stats average compared 1st half to 2nd half
firsthalf_stats <- c(firsthalf_fg, firsthalf_3p, firsthalf_ft, mean(data_firsthalf[,28]),mean(data_firsthalf[,29]))
secondhalf_stats <- c(secondhalf_fg, secondhalf_3p, secondhalf_ft, mean(data_secondhalf[,28]),mean(data_secondhalf[,29]))
compare_stats <- data.frame(Stats = c('FG%', '3P%', 'FT%', 'PTS', 'GmSc'),firsthalf_stats,secondhalf_stats)
percentage_stats <- compare_stats[1:3,]
counting_stats <- compare_stats[4:5,]

#FG%
ggplot(data = data, aes(x = G, y = data$`FG%`)) + geom_point(color='red') + ylab('Field Goal Percentage') + xlab('Games Before Jan 1 vs. After Jan 1') +
  geom_hline(yintercept = mean(data$`FG%`)) + geom_vline(xintercept = 30.5) + geom_text(size=2,vjust=2,aes(label=data$`FG%`))

#Game Score                             
ggplot(data = data, aes(x = G, y = GmSc)) + geom_point(color='red') + ylab('Game Score') + xlab('Games Before Jan 1 vs. After Jan 1') +
  geom_hline(yintercept = mean(data$GmSc)) + geom_vline(xintercept = 30.5) + geom_text(size=2,vjust=2,aes(label=data$GmSc))

#Points
ggplot(data = data, aes(x = G, y = PTS)) + geom_point(color='red') + ylab('Points') + xlab('Games Before Jan 1 vs. After Jan 1') +
  geom_hline(yintercept = mean(data$PTS)) + geom_vline(xintercept = 30.5) + geom_text(size=2,vjust=2,aes(label=data$PTS))
