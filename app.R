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

# Define UI for application
#   Should be a dashboard page, not fluid page
# ui <- fluidPage(
#
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
#
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
#
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
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
  )))
  
)

# Define server logic
#   session as a param allows access to information and functionality relating to the session
server <- function(input, output, session) {
  # output$distPlot <- renderPlot({
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

