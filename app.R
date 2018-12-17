source("libraries.R")

paymentCountry <- readRDS("paymentCountry.RDS")
paymentGeo <- readRDS("paymentGeo.RDS")
usersC <- readRDS("usersC.RDS")

countries <- na.omit(unique(c(paymentCountry$countryname,paymentGeo$countryname)))

ui <- dashboardPage(
  dashboardHeader(title="Overview"),
  # Dashboard Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Payment",tabName="payment"),
      airDatepickerInput("dates",
                         label=h3("Date range"),
                         value=c(Sys.Date()-7,Sys.Date()),
                         range=TRUE,
                         autoClose=TRUE,
                         update_on="close"
                         )
    ),
    selectizeInput('countrySelect',label="Selected Countries",choices=countries,multiple=TRUE,options=list(placeholder="All",'plugins' = list('remove_button'),
                                                                                                           'create' = TRUE,
                                                                                                           'persist' = FALSE))
    # verbatimTextOutput("countries")
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
                box(width=4,
                    background="black",
                    span(textOutput("orderAmountText"), style = "font-size:16px;font-weight:bold;"),
                    span(textOutput("orderAmountTotalText"), style = "font-size:16px;font-weight:bold;")
                ),
                box(width=4,
                    background="black",
                    span(textOutput("userSignUpText"), style = "font-size:16px;font-weight:bold;")
                )
              ),
              fluidRow(
                box(width=6,
                    plotlyOutput("payScatter",height=250)),
                box(width=6,
                    plotlyOutput("payQScatter",height=250))
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
  
  # output$countries <- renderPrint({
  #   input$countrySelect
  # })
  
  # Payment reactive ----
  payDate <- reactive({
    data <- paymentCountry %>% filter(between(date,input$dates[1],input$dates[2]))
    data <- if (is.null(input$countrySelect)) {
      data
    } else {
      data %>% filter(countryname %in% input$countrySelect)
    }

  })
  
  # Heatmap reactive ----
  geoDate <- reactive({
    data <- paymentGeo %>% filter(!is.na(order_longitude),between(as.Date(paid_date),input$dates[1],input$dates[2]))
    
    data <- if (is.null(input$countrySelect)) {
      data
    } else {
      data %>% filter(countryname %in% input$countrySelect)
    }
  })
  
  # Users reactive (signup) ----
  userSUDate <- reactive({
    data <- usersC %>% filter(!is.na(activated_date),between(activated_date,input$dates[1],input$dates[2]))
    
    data <- if (is.null(input$countrySelect)) {
      data
    } else {
      data %>% filter(countryname %in% input$countrySelect)
    }
  })

  # General Metrics ----
  output$orderAmountText <- renderText({
    paste("모금액:  \U20A9",format(payDate() %$% sum(orderAmount),nsmall=0,big.mark=","))
  })
  
  output$orderAmountTotalText <- renderText({
    paste("모금액+배송:  \U20A9",format(payDate() %$% sum(orderTotal),nsmall=0,big.mark=","))
  })
  
  output$userSignUpText <- renderText({
    paste0("가입자: ",format(userSUDate() %>% nrow(),big.mark=","),"명")
  })
  
  # Heatmap ----
  output$payHeatmap <- renderLeaflet({
    # need to exclude NA values, otherwise leaflet.extras breaks
    # paymentGeo %>% filter(!is.na(order_longitude),between(as.Date(paid_date),Sys.Date()-7,Sys.Date())) %>% 
    paymentGeo %>% filter(!is.na(order_longitude)) %>%
      leaflet() %>%

      addProviderTiles(providers$CartoDB.DarkMatter,
                       options=providerTileOptions(minZoom=2,maxZoom=7)) %>%
      addHeatmap(lng=~as.numeric(order_longitude),
                 lat=~as.numeric(order_latitude),
                 intensity=~amount,
                 max=1,
                 radius=8) %>% # make radius alterable by user?
      setMaxBounds(lng1=-180, 
                  lat1=80,
                  lng2=190,
                  lat2=-70) %>%
      addMiniMap(tiles=providers$Stamen.Toner,
                 toggleDisplay=T)
      
  })
  
  # observer 
  observe({
    leafletProxy("payHeatmap",data=geoDate()) %>%
      clearHeatmap() %>%
      addHeatmap(lng=~as.numeric(order_longitude),
                 lat=~as.numeric(order_latitude),
                 intensity=~amount,
                 max=1,
                 radius=8) # make radius alterable by user?
  })
  

  
  # Scatter plot pay amount ----
  output$payScatter <- renderPlotly({
    data <- payDate()
    data %>% 
      group_by(date) %>% 
      summarize(orderAmount=sum(orderAmount),orderTotal=sum(orderTotal)) %>% 
      plot_ly(x=~date,y=~orderTotal,name="배송 포함",type="scatter",mode="lines",
              hoverinfo="text+x",
              text=~paste0("배송 포함 금액: \U20A9",format(orderTotal,nsmall=0,big.mark=","))) %>% 
      add_trace(y=~orderAmount,name="배송 미포함",
                hoverinfo="text+x",
                text=~paste0("배송 미포함 금액: \U20A9",format(orderAmount,nsmall=0,big.mark=","))) %>%
      layout(title="모금액",
             hovermode="x")
  })
  
  # Scatter plot pay quantity ----
  output$payQScatter <- renderPlotly({
    data <- payDate()
    data %>%
      group_by(date) %>%
      summarize(orderQ=sum(paying),orderQTotal=sum(volume)) %>%
      plot_ly(x=~date,y=~orderQTotal,name="셋트 수",type="scatter",mode="lines",
              hoverinfo="text+x",
              text=~paste0("셋트 수: ",format(orderQTotal,nsmall=0,big.mark=","),"개")) %>% 
      add_trace(y=~orderQ,name="결제 건수",
                hoverinfo="text+x",
                text=~paste0("결제 건수: ",format(orderQ,nsmall=0,big.mark=","),"회")) %>%
      layout(title="결제량",
             hovermode="x")
  })
  
  
  # Table ----
  output$payFullTable <- renderDataTable({
    # data <- paymentCountry %>% filter(between(date,input$dates[1],input$dates[2]))
    data <- payDate()
    datatable(data,rownames=FALSE,
                  options=list(pageLength=10,
                               lengthMenu=c(10,50,100),
                               order=list(list(1,'desc')),
                               scrollX=TRUE))
  })
  
}

shinyApp(ui, server)
