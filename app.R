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

#------------------------------------
#reading in data for halsted
Halsted <-
  read.table(file = "./station_UIC-Halsted.csv", sep = ",", header = TRUE)

#converting date type to workable column
newDate <- as.Date(Halsted$date, "%m/%d/%Y")
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
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(tabItems(tabItem(
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
  ))),
  plotOutput("entryYear")
  
)

# Define server logic
#   session as a param allows access to information and functionality relating to the session
# sumOfRidesPerYear = Halsted %>% group_by(year(newDate)) %>% summarise(sum = sum(rides))
server <- function(input, output, session) {
  output$entryYear <- renderPlot({
    subset(Halsted, newDate > as.Date("2000-12-31")) %>%
      ggplot(aes(year(newDate), rides)) +
      geom_bar(stat = "identity", fill = "#88CCEE") +
      labs(x = "Years", y = "Number of Entries", title = "Entries per Year") +
      theme_bw() 
      # scale_y_continuous(expand = c(0, 0))#, limits = c(0, max(sumOfRidesPerYear$sum) * 1.05))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

