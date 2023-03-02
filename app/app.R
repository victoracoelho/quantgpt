library(shiny)
library(shinymanager)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(rugarch)

ui <- fluidPage(
  titlePanel("Volatility Estimator"),

  sidebarLayout(
    sidebarPanel(
      helpText("Input the tickers below, like PETR4.SA for Petrobras data
      and ^BVSP for Ibovespa data.

        "),
      textInput("symb1", "Stock 1", "PETR4.SA"),

      # Imputando as datas
      dateRangeInput("dates",
                     "Date range",
                     start = "2021-01-01",
                     end = as.character(Sys.Date())),

      br(),
      br(),

      #checkboxInput("log", "Plot y axis on log scale",
      #value = FALSE),

      #checkboxInput("adjust",
      #"Adjust prices for inflation", value = FALSE)
    ),

    mainPanel(plotOutput("plot"))
  )
)


server <- function(input, output, session) {

  dataInput1 <- reactive({
    getSymbols(input$symb1, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
  })



  ret1 <- reactive({
    na.omit(ROC(dataInput1()))

  })

  ret_garch <- reactive({
    ugarchspec(variance.model = list(garchOrder = c(1,1)),
               mean.model = list(armaOrder = c(0,0)))

  })

  ret_fit <- reactive({
    ugarchfit(spec = ret_garch(), data = ret1())
  })

  ret_forecast <- reactive({
    ugarchforecast(ret_fit(), n.ahead = 10)
  })


  output$plot <- renderPlot({

    plot(sigma(ret_forecast()))
  })

}



shinyApp(ui, server)
