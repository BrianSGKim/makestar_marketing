source("libraries.R")

paymentCountry <- readRDS("paymentCountry.RDS")
paymentGeo <- readRDS("paymentGeo.RDS")


ui <- dashboardPage(
  dashboardHeader(title="Overview"),
  # Dashboard Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Payment",tabName="payment"),
      airDatepickerInput("dates",
                         label=h3("Date range"),
                         value=c(Sys.Date()-10,Sys.Date()),
                         range=TRUE,
                         autoClose=TRUE,
                         update_on="close"
                         )
    )
  ),
  # Dashboard Body ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "payment",
              # fluidRow(
              #   box(width=6,
              #       verbatimTextOutput("value"),
              #       verbatimTextOutput("value1"),
              #       verbatimTextOutput("value2"))
              # ),
              fluidRow(

                box(width=12,
                    leafletOutput("payHeatmap",height=500))
              ),
              fluidRow(
                box(width=12,
                    
                    dataTableOutput("payFullTable"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # output$value <- renderPrint({ input$dates })
  # output$value1 <- renderPrint({ input$dates[1] })
  # output$value2 <- renderPrint({ input$dates[2] })
  
  geoDate <- reactive({
    paymentGeo %>% filter(!is.na(order_longitude),between(as.Date(paid_date),input$dates[1],input$dates[2]))
  })
  
  output$payHeatmap <- renderLeaflet({
    # need to exclude NA values, otherwise leaflet.extras breaks
    # paymentGeo %>% filter(!is.na(order_longitude),between(as.Date(paid_date),Sys.Date()-7,Sys.Date())) %>% 
    paymentGeo %>% filter(!is.na(order_longitude)) %>%
      leaflet() %>%

      addProviderTiles(providers$CartoDB.DarkMatter,
                       options=providerTileOptions(minZoom=2,maxZoom=7)) %>%
      addHeatmap(lng=~as.numeric(order_longitude),lat=~as.numeric(order_latitude),intensity=~amount,max=1,radius=8) %>% # make radius alterable by user?
      setMaxBounds(lng1=-180, 
                  lat1=80,
                  lng2=190,
                  lat2=-70) %>%
      addMiniMap(tiles=providers$Stamen.Toner,
                 toggleDisplay=T)
      
  })
  
  observe({
    leafletProxy("payHeatmap",data=geoDate()) %>%
      clearHeatmap() %>%
      addHeatmap(lng=~as.numeric(order_longitude),lat=~as.numeric(order_latitude),intensity=~amount,max=1,radius=8) # make radius alterable by user?
      
  })
  
  output$payFullTable <- renderDataTable({
    data <- paymentCountry %>% filter(between(date,input$dates[1],input$dates[2]))
    
    datatable(data,rownames=FALSE,
                  options=list(pageLength=10,
                               lengthMenu=c(10,50,100),
                               order=list(list(1,'desc')),
                               scrollX=TRUE))

  })
  
}

shinyApp(ui, server)
