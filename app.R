source("libraries.R")

paymentCountry <- readRDS("paymentCountry.RDS")
paymentGeo <- readRDS("paymentGeo.RDS")


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
  
  output$payHeatmap <- renderLeaflet({
    # need to exclude NA values, otherwise leaflet.extras breaks
    paymentGeo %>% filter(!is.na(order_longitude)) %>% leaflet() %>%

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
  
  output$payFullTable <- renderDataTable({
    datatable(paymentCountry,
                  rownames=FALSE,
                  options=list(pageLength=10,
                               lengthMenu=c(10,50,100),
                               order=list(list(1,'desc')),
                               scrollX=TRUE))

  })
  
}

shinyApp(ui, server)
