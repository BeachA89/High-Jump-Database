library(shiny)
library(shinydashboard)
library(shinythemes)
#library(SparkR)
library(rCAT)
library(DT)
library(dplyr)
library(tibble)
library(tidyr)
library(plyr)
library(data.table)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(forcats)
library(tidyverse)

data_folder <- "/Users/aaron/Documents/GitHub/High-Jump-Database/Processed data 1"

filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")


table1 <-  lapply(filenames, fread, header=TRUE, stringsAsFactors=FALSE)

#imported_data <- read.csv("C:/Users/aaron.beach/..../R/Hammer Conversion/sample2.csv", header=FALSE, stringsAsFactors=FALSE)


Collateddata =rbindlist(table1, fill=TRUE)
#Collateddata <-  dplyr::rename(Collateddata, "Height_cleared" = "Height cleared")
Collateddata <-  mutate(Collateddata, ID = paste(dataname,`Height cleared`,  sep='_')) 
Collateddata <-  mutate(Collateddata, HeightYN = paste(Height, Cleared,  sep='_')) 
# for (i in 1:length(table1)){
#   data1 = table1[[i]]
#   data1 = data1 %>% reshape2::melt(id = c("dataname", "Name", "Competition", "Jump_number"))
#   data1$variable = as.character(data1$variable)
#   Combineddata[[i]] = data1
# }


# Collateddata <- Collateddata %>%dplyr::mutate("Round&Distance" = paste0(Collateddata$Round, " ",Collateddata$Distance))

# Collateddata$Distance = as.numeric(Collateddata$Distance)

# Collateddata <- Collateddata %>% 
#   dplyr::mutate("Turn 1 Single" = paste0(Collateddata$Turn1Single, " (",Collateddata$Turn1SinglePC,"%)")) %>% 
#   dplyr::mutate("Turn 1 Double"=paste0(Collateddata$Turn1Double, " (",Collateddata$Turn1DoublePC,"%)")) %>% 
#   dplyr::mutate("Turn 2 Single"=paste0(Collateddata$Turn2Single, " (",Collateddata$Turn2SinglePC,"%)")) %>% 
#   dplyr::mutate("Turn 2 Double"=paste0(Collateddata$Turn2Double, " (",Collateddata$Turn2DoublePC,"%)")) %>% 
#   dplyr::mutate("Turn 3 Single"=paste0(Collateddata$Turn3Single, " (",Collateddata$Turn3SinglePC,"%)")) %>% 
#   dplyr::mutate("Turn 3 Double"=paste0(Collateddata$Turn3Double, " (",Collateddata$Turn3DoublePC,"%)")) %>% 
#   dplyr::mutate("Turn 4 Single"=paste0(Collateddata$Turn4Single, " (",Collateddata$Turn4SinglePC,"%)")) %>% 
#   dplyr::mutate("Turn 4 Double"=paste0(Collateddata$Turn4Double, " (",Collateddata$Turn4DoublePC,"%)")) %>% 
#   dplyr::mutate("Turn 1" = paste0(Collateddata$Turn1, " (",Collateddata$Turn1PC,"%)")) %>% 
#   dplyr::mutate("Turn 2" = paste0(Collateddata$Turn2, " (",Collateddata$Turn2PC,"%)")) %>% 
#   dplyr::mutate("Turn 3" = paste0(Collateddata$Turn3, " (",Collateddata$Turn3PC,"%)")) %>% 
#   dplyr::mutate("Turn 4" = paste0(Collateddata$Turn4, " (",Collateddata$Turn4PC,"%)")) 



# Table1 <- Collateddata %>% dplyr::select(Name, Competition, Round, Distance, AngleAvg, HVelAvg, RVelAvg, "Turn 1 Single", "Turn 1 Double", 
#                                   "Turn 2 Single", "Turn 2 Double",  "Turn 3 Single", "Turn 3 Double",
#                                   "Turn 4 Single", "Turn 4 Double")
# 
# 
# 
# Table2 <- Collateddata %>% dplyr::select(Name, Competition, Round, Distance, Turn1, Turn2, Turn3, Turn4, TotalTime)
# 
# 
# 
# Table3 <- Collateddata %>% dplyr::select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
