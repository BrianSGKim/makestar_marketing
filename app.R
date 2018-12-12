library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Overview"),
  # Dashboard Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Payment",tabName="payment")
      
    )
  ),
  # Dashboard Body ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "payment",
              fluidRow(
                box(leafletOutput("payHeatmap"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$payHeatmap <- renderLeaflet({
    # need to exclude NA values, otherwise leaflet.extras breaks
    paymentGeo %>% filter(!is.na(order_longitude)) %>% leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addWebGLHeatmap(lng=~as.numeric(order_longitude),lat=~as.numeric(order_latitude),size=17,units="px") # make size alterable by user
  })
  
}

shinyApp(ui, server)
