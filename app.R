

library(shinydashboard)
library(shiny)
library(haven)
library(base)
library(ggplot2)
library(MASS)
library(ggthemes)




xylabels = c("SALEQ" = "Sales (million $)","CHEQ" = "Cash (million $)", "ATQ" = "Assets (million $)","OIADPQ" = "Profits (million $)",
  "XRDQ" = "R&D (million $)",
  "XSGAQ" = "SG&A (million $) "
)

method = c(
  "Linear Model" = "lm"
  ,
  "LOESS" =  "loess"
  ,
  "Robust Linear" = "rlm"
  ,
  "None" = ""
)

ui = dashboardPage(
  dashboardHeader(title = "Apple Financials"),
  dashboardSidebar(
    sidebarMenu(
      fileInput(inputId = "data",label  = "Upload SAS Data:", multiple = FALSE,accept = ".sas7bdat"
      ),
      selectInput(inputId = "x",label = "X-axis Variable", choices = c("Sales" = "SALEQ","Cash" = "CHEQ","Assets" = "ATQ","Profits" = "OIADPQ","R&D" = "XRDQ",
          "SG&A" = "XSGAQ"
        )
        ,
        selected = "SALEQ"
      ),
      selectInput( inputId = "y", label = "Y-axis Variable",choices =  c("Sales" = "SALEQ","Cash" = "CHEQ","Assets" = "ATQ","Profits" = "OIADPQ","R&D" = "XRDQ",
                     "SG&A" = "XSGAQ"
        ),
        selected = "XRDQ"
      ),
      selectInput(inputId = "scale",label = "Choose the Scale", choices = c("Levels", "Log10")
        
      ),
      radioButtons(inputId = "model", label = "Choose the Model",choices = c("Linear Model", "LOESS", "Robust Linear", "None"),
        selected = "LOESS"
      ),
      checkboxInput(inputId = "checkbox",label = "Standard Error Ribbon",value = TRUE
      ),
      conditionalPanel(
        condition = "input.model == 'LOESS'",
        sliderInput(
          inputId = "span",
          label = "Span for LOESS",
          min = 0,
          max = 1,
          value = 0.75,
          round = -2
        )
        
      )
    )
  ),
  dashboardBody(fluidRow(box(
    plotOutput(outputId = "plot")
  )))
)

`%then%` = shiny:::`%OR%`

server = function(input, output) {
  aapl = reactive({
      validate(
        need(
          input$data!="", 
          HTML('Please upload a SAS data file (sas7bdat extension) 
               Make sure that it has the following variables: 
                 SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ')
        )
      )
      validate(
        need(input$x != input$y, "X and Y variables have to be different")
      )
      tryCatch(
      haven::read_sas(input$data$datapath)
      ,
      error = function(e){
        stop(safeError("An error has occurred. Check your logs or contact the app author for clarification."))
      })
  })

  
  output$plot = renderPlot(
    ggplot(data = aapl(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(x = xylabels[input$x], y = xylabels[input$y]) +
      scale_x_continuous(trans = ifelse(
        input$scale == "Log10", "log10", "identity"
      )) +
      theme_bw()+
      theme(axis.title.x = element_text(size= rel(1), hjust=1),
            axis.title.y = element_text(size= rel(1), hjust=1)) + 
      if (input$model == "LOESS")
      {
        geom_smooth(se = input$checkbox, span = input$span)
      }
    else
    {
      geom_smooth(se = input$checkbox, method = method[input$model])
    }
  )
  
}


shinyApp(ui = ui, server = server)
