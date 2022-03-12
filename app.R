#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries/Imports
library(shiny)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(leaflet)
library(tidyverse)
library(DT)
library(data.table)

#------------------------------------
# create intial dataframe
stationsAll <- data.frame(
  ï..station_id=integer(),
  stationname=character(),
  date=character(),
  daytype=character(),
  rides=integer(),
  stringsAsFactors = FALSE
)
# View(stationsAll)

tsvFileList <- list.files(pattern="*.tsv")
dataStations <- lapply(tsvFileList, read.delim)
stationsAll <- do.call(rbind, dataStations)


newD <- as.Date(stationsAll$date, '%m/%d/%Y')
stationsAll$date <- NULL
stationsAll$date <- newD
view(stationsAll)

# load in station id with long and lat
xData <- fread("CTA_-_System_Information_-_List_of__L__Stops.csv",
               select = c("MAP_ID", "Location"))


 # Loops through every station id and prints out location
locData <- data.frame(
  MAP_ID=c(),
  Location=c(),
  Latitude=c(),
  Longitude=c(),
  stringsAsFactors = FALSE
)

for (row in 1:NROW(xData)) {
  statID = xData[[1]][row]
  print(statID)
  locale <- xData[[2]][row]
  locationString <- gsub("[(,)]", "", xData[[2]][row])
  # print(locationString)
  
  lat <- word(locationString, 1, sep=fixed(' '))
  long <- word(locationString, 2, sep=fixed(' '))
  # print("lat is")
  # print(lat)
  # print("long is")
  # print(long)
  tempRow <- c(statID, locale, lat, long)
  locData <- rbind(locData, tempRow)
}
names(locData) <- c("ï..station_id", "Location", "Latitude", "Longitude")
view(locData)

# d <- merge(stationsAll, locData)

# d <- cbind(stationsAll, locData, by = "ï..station_id", names())
# view(d)
# for (row in 1:NROW(stationsAll)) {
#   id <- stationsAll[[1]][row]
#   print(id)
#   
#   }

#reading in data for halsted
Halsted <-
  read.table(file = "./station_UIC-Halsted.tsv", sep = "\t", header = TRUE)
# View(Halsted)

#converting date type to workable column
newDate <- as.Date(Halsted$date, '%m/%d/%Y')
Halsted$newDate <- newDate
Halsted$date <- NULL
years <- c(2001:2021)

ui <- dashboardPage(
  dashboardHeader(title = "Jack Martin and Shoaib Jakvani Project 2"),
  dashboardSidebar(
    disable = FALSE,
    collapsed = FALSE,
    sidebarMenu(
      id = "tabs",
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("home", tabName = "home"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "home",
      box(
        title = "home",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        
        
        plotOutput("landingPage", height = 1100),
        dateInput(
          "inputDate",
          "Select a date:",
          '2021-08-23',
          '2001-01-01',
          '2021-11-30'
        ),
        actionButton("left", "<<"),
        actionButton("right", ">>")
      )
    ),
    tabItem(
      tabName = "about",
      box(
        title = "About",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        
        
        h2(
          "Jack Martin created this app for Project 1 of UIC's CS 424 - Visual Analytics."
        ),
        p(
          "This data is from the Chicago Data Portal. More specifically, the \'CTA - Ridership - L Station Entries - Daily total\'.
                           The main components on why we are given this project is to teach us and give better familiarity with\n
                          both the R language and Shiny and Shiny dashboard. We were tasked with analyzing and plotting Entries over\n
                          specific stations over 2001-2021 and over each Day of the Week and Month."
        )
      )
    )
  ))
  
)

# Define server logic
#   session as a param allows access to information and functionality relating to the session
# newData <- stationsAll[stationsAll$date == as.Date('2021-08-23'),]
# maxRides <- max(newData$rides)
# view(newData)
server <- function(input, output, session) {
  # changes dataset based on day
  dateReactive <-
    reactive({
      subset(stationsAll, stationsAll$date == input$inputDate)
    })
  
  # shifts data by one day in the past
  observeEvent(input$left, {
    updateDateInput(
      session,
      "inputDate",
      value = input$inputDate - days(1),
      min = '2001-01-01',
      max = '2021-11-30'
    )
  })
  # shifts data by one day in the future
  observeEvent(input$right, {
    updateDateInput(
      session,
      "inputDate",
      value = input$inputDate + days(1),
      min = '2001-01-01',
      max = '2021-11-30'
    )
  })
  
  # ----------------------------
  output$landingPage <- renderPlot({
    newData <- dateReactive()
    maxRides <- max(newData$rides)
    subset(newData) %>%
      ggplot(aes(x = rides, y = fct_rev(stationname))) +
      geom_col(stat = "identity", fill = "#88CCEE") +
      labs(x = "Number of entries", y = "Station name", title = "Entries on August 23, 2021") +
      # geom_col() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
      theme_bw() +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(0, maxRides * 1.05))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
