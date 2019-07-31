#Load libraries
library(lubridate) #for working with dates and times
library(shiny) #needed for website setup
library(plotly) #graphs and plots
library(leaflet) #map feature
library(shinythemes) #shiny customizations
library(dplyr) #for fitting and manipulating data frames
library(gridExtra) #provides ability to mostly arrange multiple grid-based plots on a page and draw tables
library(reshape2) #helps transform the data in wide and long formats
library(data.table) #helps transform and manipulate large data
library(stringr) #manipulates characters and strings
library(slickR) #primarily used for the carousel/slideshow on the homepage of the shinyapp
library(roxygen2) #for documentation

#### To-do list ####

#### Variables to Modify ####
path_to_rdata <- "data/pre-shiny-data.RData"

#### Global Variables ####
days.of.week <- c("Sunday", "Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday")
m.top = list(t=75)  # Margin list for plotly

load(path_to_rdata)

#### Notes ####
# Color code for background is #4e5d6c

#### Functions ####
Make.Labels <- function(year.month) {
  year.used <- NULL
  d.labels <- list()
  for (ym in year.month) {
    if (substr(ym,1,4) %in% year.used) {

      d.labels[[ym]] = ""
    } else {
      year.used[length(year.used)+1] <- substr(ym,1,4)
      d.labels[[ym]] = ym
    }

  }
  print(d.labels)
  return(d.labels)
}

#format values that have NA in them
Fill.Table <- function(tab) {
  for (i in 1:12) {
    if (is.na(tab[as.character(i)])) {
      tab[as.character(i)] <- 0
    }
  }
  return(as.vector(sort(tab)))
}

#format dates as Year/Month/Day
MakeDate <- function(df) {
  return(as.Date(paste(df$Year,df$Month, "1",sep="-"), format = "%Y-%m-%d"))
}

By.Day.Col <- function(col, name) {
  open.col<- w.data %>%
    filter(Closed_BT) %>%
    count(Day=floor_date(Intake_Date_DT, "day"), .dots=col)

  close.col <- w.data %>%
    filter(Closed_BT) %>%
    count(Day=floor_date(Closed_DT, "day"), .dots=col)

  names(open.col)[2] <- "Col"
  names(close.col)[2] <- "Col"
  open.col$CumOpen <- 0
  close.col$CumClose <- 0
  for (v in unique(open.col$Col)){
    open.col[open.col$Col == v,]$CumOpen <- open.col %>%
      filter(Col == v) %>%
      pull(n) %>%
      as.vector() %>%
      cumsum()
    close.col[close.col$Col == v,]$CumClose <- close.col %>%
      filter(Col == v) %>%
      pull(n) %>%
      as.vector() %>%
      cumsum()
  }

  col.day <- merge(open.col, close.col, by=c("Day", "Col"), all.y=TRUE)
  names(col.day)[c(3,5)] <- c("Opened", "Closed")
  col.day <- col.day %>%
    mutate(Active=CumOpen-CumClose)
  names(col.day)[2] <- name
  return(col.day)
}

Graph.By.Month <- function(col.name, new.name, ticket.type, g.title="") {
  By.Day.Col(col.name, new.name) %>%
    group_by(!!sym(new.name), Month=floor_date(Day, "month")) %>%
    summarise(Count=sum(!!sym(ticket.type), na.rm=TRUE)) %>%
    plot_ly(x=~Month, y=~Count, color=~get(new.name), colors='Paired', type='scatter', mode='lines') %>%
    layout(title=g.title)
}
