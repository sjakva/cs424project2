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

tsvFileList <- list.files(pattern="X*.txt")
dataStations <- lapply(tsvFileList, read.delim)
stationsAll <- do.call(rbind, dataStations)


newD <- as.Date(stationsAll$date, '%m/%d/%Y')
stationsAll$date <- NULL
stationsAll$date <- newD
view(stationsAll)

stationsNames <- unique(stationsAll$stationname)
view(stationsNames)

awesome <- makeAwesomeIcon(
  icon = "train",
  iconColor = "black",
  markerColor = "blue",
  library = "fa"
)

selections <- c("Standard", "TonerLines","Positron")

ui <- dashboardPage(
  skin = "black",
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
      menuItem("home", tabName = "home", selected = TRUE),
      menuItem("About", tabName = "about"),
      menuItem("sussy", tabName = "sussy")
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "home",
      box(
        title = "Entries For All Stations On Given Day",
        solidHeader = TRUE,
        status = "primary",
        width = 12, background = "navy",
        
        
        actionButton("std", "Alphabetical"),
        actionButton("min", "Descending"),
        actionButton("max", "Ascending"),
        plotOutput("landingPage", height = 500),
        dateInput(
          "inputDate",
          "Select a date:",
          '2021-08-23',
          '2001-01-01',
          '2021-11-30'
        ),
        actionButton("left", "<<"),
        actionButton("right", ">>"),
        selectInput("markerSearch", "Search for a stop...",
                    stationsNames, selected=NULL, multiple=TRUE
                       ),
        leafletOutput("leafsussy"),
        selectInput("background","Select a background",selections, selected='Standard')
      
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
    ),
    tabItem(
      tabName = "sussy",
      box(
        title = "sussy",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        dataTableOutput("chartsus"),
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
    )

  ))
  
)

# Define server logic
#   session as a param allows access to information and functionality relating to the session
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
  
  # Three observe events that dictate whether the chart shows alphabetically, min, or max
  # Alphabetical
  observeEvent(input$std, {
    # Standard view
    # change/sort data to be alphabetical order 
    output$landingPage <- renderPlot({
      react_title <- paste("Entries on", input$inputDate)
      newData <- dateReactive()
      maxRides <- max(newData$rides)
      if (input$min)
        subset(newData) %>%
        ggplot(aes(y = rides, x = stationname)) +
        geom_col(stat = "identity", fill = "#88CCEE") +
        labs(x = "Station Name", y = "Number of Entries", title = react_title) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)
        ) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, maxRides * 1.05))
    })
  })
  
  # Minimum
  observeEvent(input$min, {
    # Descending
    # change/sort data to be minimum order 
    output$landingPage <- renderPlot({
      react_title <- paste("Entries on", input$inputDate)
      newData <- dateReactive()
      maxRides <- max(newData$rides)
      subset(newData) %>%
        ggplot(aes(y = rides, x = reorder(stationname, -rides, min))) +
        geom_col(stat = "identity", fill = "#88CCEE") +
        labs(x = "Station Name", y = "Number of Entries", title = react_title) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)
        ) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, maxRides * 1.05))
    })
  })
  
  # Maximum
  observeEvent(input$max, {
    # Ascending
    # change/sort data to be maximum order 
    output$landingPage <- renderPlot({
      react_title <- paste("Entries on", input$inputDate)
      newData <- dateReactive()
      maxRides <- max(newData$rides)
      subset(newData) %>%
        ggplot(aes(y = rides, x = reorder(stationname, +rides, max))) +
        geom_col(stat = "identity", fill = "#88CCEE") +
        labs(x = "Station Name", y = "Number of Entries", title = react_title) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)
        ) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, maxRides * 1.05))
    })
  })
  
  # ----------------------------
  output$landingPage <- renderPlot({
    react_title <- paste("Entries on", input$inputDate)
    newData <- dateReactive()
    maxRides <- max(newData$rides)
    subset(newData) %>%
      ggplot(aes(y = rides, x = stationname)) +
      geom_col(stat = "identity", fill = "#88CCEE") +
      labs(x = "Station Name", y = "Number of Entries", title = react_title) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)
      ) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, maxRides * 1.05))
  })
  
  
  
  
  # ----------------------------
  output$chartsus <- DT::renderDataTable(
    
    DT::datatable({
      newData <- dateReactive()
      table_df <- data.frame(Stations=character(), Entries=integer())
      rowdf <- data.frame(Stations=newData$stationname, Entries=newData$rides)
      
      table_df <- rbind(table_df, rowdf)
      table_df
    })
  )
  
  output$leafsussy <- renderLeaflet({
    newData <- dateReactive()
    # lat <- newData$lat
    # long <- newData$long
    leaflet(data = newData) %>%
      addTiles() %>%
      setView(lng =  -87.6000, lat = 41.9291, zoom = 10) %>%
      # addProviderTiles(providers$Stamen.Toner) %>%
      # addMarkers(~long, ~lat ,popup = "sussy")
      addAwesomeMarkers( icon=awesome, lng = newData$long, lat = newData$lat, label = newData$stationname, popup = newData$stationname)#, markerOptions(opacity = 1))
  })
  observe({
    newData <-dateReactive()
    if(input$background == 'Positron')
    {
      leafletProxy("leafsussy",data = newData) %>%
        clearTiles() %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
    else if(input$background == 'TonerLines')
    {
      leafletProxy("leafsussy",data = newData) %>%
        clearTiles() %>%
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLines)
    }
    else if(input$background == 'Standard')
    {
      leafletProxy("leafsussy",data = newData) %>%
        clearTiles() %>%
        addTiles() %>%
        addProviderTiles(providers$nlmaps.standaard) 
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
