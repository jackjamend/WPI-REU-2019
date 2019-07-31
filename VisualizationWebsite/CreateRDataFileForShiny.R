# This is a file that should be used to set up the RData file for the shiny app
# to run. This creates the objects that will be loaded in to the app. This
# should not be run as a part of the code when the shiny app is loaded, this
# needs to be done as preprocessing.
# Author: Jack Amend
# Contact: jamend@elon.edu
# Date: 7/31/19

# If you get any errors with the loading the libraries, make sure that you have
# installed the packages to your machine.
library(readr)
library(lubridate)
library(dplyr)
library(reshape2)
library(leaflet)


#### Create w.data ####

# Variabels to change to set to local path
csv_df_path <- "~/Documents/WPI/python/jupyter/full-clean-jun12.csv"
map_df_path <- "./csv-data/map-data.csv"
rdata_path <- "DataExploration/data/pre-shiny-data.RData"

# w.data is the dataframe that contains all the ticket information.

# If this line fails it is cause either the path is incorrect or readr has not
# been loaded.
w.data <- read_csv(csv_df_path, 
                    col_types = cols(Cross_Street_Name_VC = col_character(), 
                                     Cross_Street_Number_CH = col_character(), 
                                     Currently_Assigned_To_This_Employee_VC = col_character(),
                                     Description_VC = col_character(), 
                                     Number_Of_Requests_VC = col_number(), 
                                     Street_Number_CH = col_character(), 
                                     X_Coordinate_DC = col_double(),
                                     Y_Coordinate_DC = col_double()))

w.data$Time_to_Complete <- as.difftime(0, units="days")
# Rows with no close date
mask <- w.data$Closed_BT & is.na(w.data$Closed_DT)
sub.data <- w.data[mask, ]
diffs <- difftime(sub.data$Date_Last_Activity, sub.data$Intake_Date_DT, units = "hours")
w.data[mask, ]$Time_to_Complete <- diffs

# Row with closed date
mask <- w.data$Closed_BT & !is.na(w.data$Closed_DT)
sub.data <- w.data[mask, ]
diffs <- difftime(sub.data$Closed_DT, sub.data$Intake_Date_DT, units = "hours")
w.data[mask, ]$Time_to_Complete <- diffs

w.data[!w.data$Closed_BT,]$Time_to_Complete <- NA
w.data$Intake_Day_of_Week <- weekdays(w.data$Intake_Date_DT)
w.data$Closed_Day_of_Week <- weekdays(w.data$Closed_DT)
rm(list=c("mask", "diffs", "sub.data", "csv_df_path"))


#### ticket.day ####
# This variable is used to see the number of tickets opened, closed, and active
# on a given day.
open.date <- w.data %>%
  count(Day=floor_date(Intake_Date_DT, "day")) 

open.date$CumOpen <- open.date %>%
  select(n) %>%
  cumsum()

close.date <- w.data %>%
  filter(!is.na(Closed_DT)) %>%
  count(Day=floor_date(Closed_DT, "day")) 

close.date$CumClose <- close.date %>%
  select(n) %>%
  cumsum()

close.date$CumClose <- as.vector(close.date$CumClose$n)
open.date$CumOpen <- as.vector(open.date$CumOpen$n)

ticket.day <- merge(as.data.frame(open.date), as.data.frame(close.date), 
                    by = "Day", all.y=TRUE)
names(ticket.day)[c(2,4)] <- c("Opened", "Closed")

ticket.day <- ticket.day %>%  
  mutate(Active=CumOpen-CumClose)

rm(list=c("open.date", "close.date"))

#### dept.open.tickets ####
# Similar to the above variable, but broken down by division
open.dept.date <- w.data %>%
  filter(Closed_BT) %>%
  count(Day=floor_date(Intake_Date_DT, "day"), Dept=Division_Name_VC)

close.dept.date <- w.data %>%
  filter(Closed_BT) %>%
  count(Day=floor_date(Closed_DT, "day"), Dept=Division_Name_VC) 

open.dept.date$CumOpen <- 0
close.dept.date$CumClose <- 0
for (dept in unique(open.dept.date$Dept)){
  
  open.dept.date[open.dept.date$Dept == dept,]$CumOpen <- open.dept.date %>%
    filter(Dept == dept) %>%
    pull(n) %>%
    as.vector() %>%
    cumsum()
  
  close.dept.date[close.dept.date$Dept == dept,]$CumClose <- close.dept.date %>%
    filter(Dept == dept) %>%
    pull(n) %>%
    as.vector() %>%
    cumsum()
  
}

ticket.dept.day <- merge(open.dept.date, close.dept.date, by=c("Day", "Dept"), 
                         all.y=TRUE)
names(ticket.dept.day)[c(3,5)] <- c("Opened", "Closed")
dept.ticket.day <- ticket.dept.day %>%  
  mutate(Active=CumOpen-CumClose)

rm(list=c("close.dept.date", "open.dept.date", "dept","ticket.dept.day"))

#### inVout ####
# Counts for the number of tickets inputted and outputted each month
intake <- w.data %>%
  filter(Closed_BT) %>%
  count(Date=floor_date(Intake_Date_DT, 'month'))

outtake <- w.data %>%
  filter(Closed_BT) %>%
  count(Date=floor_date(Closed_DT, 'month'))

inVout <- merge(intake, outtake, by="Date", all=TRUE)

names(inVout) <- c("Date", "Intake", "Outtake")
inVout[is.na(inVout$Intake),]$Intake <- 0

rm(list=c("intake", "outtake"))

#### melted ####
# Augments the above dataframe in order to make the animated pie

melted <- inVout %>%
  filter(Date < as.Date("2019-01-01")) %>%
  melt(id.vars="Date", measure.vars=c("Intake", "Outtake"))

melted <- melted[order(melted$variable),]
melted$num <- as.numeric(melted$Date)

#### step.list ####
# Creates a vector used for the animated pie chart. This is so the date can be 
# correctly shown.

steps.list = c()
for (ri in 1:nrow(melted)) {
  row <- melted[ri,]
  
  s <- list(args=list(paste("slider-__animation_slider_id__-", row$num, sep = "")), 
            label= row$Date,
            value=row$num,
            method="animate"
  )
  steps.list <- c(steps.list, s)
}
rm(list=c("row", "s", "ri"))

#### wor.map ####
map.df <- read_csv(map_df_path,
                   col_types = cols(longtitude = col_double(),
                                    latitude = col_double(),
                                    Year = col_double(),
                                    Type = col_character(),
                                    Request_VC = col_character(),
                                    Priority_CH = col_character(),
                                    popup = col_character()
                   )
)
wor.map <- leaflet(data=map.df) %>%
  addTiles() %>%
  addMarkers(lat = ~latitude, lng= ~longtitude,
             popup = ~popup,
             label=~lab,
             clusterOptions = markerClusterOptions()
  )
rm(list=c("map.df", "map_df_path"))

save(w.data, ticket.day, steps.list, inVout, dept.ticket.day, wor.map, melted, file=rdata_path)
