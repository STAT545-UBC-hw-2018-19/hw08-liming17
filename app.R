library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  theme = "my.css",
  img(src = "bcl.jpg"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      uiOutput("countryOutput"),
      checkboxInput("checkbox", "Sort the results table by price", FALSE) 
    ),
    mainPanel(
      textOutput("resultNum"),
      tabsetPanel(
        tabPanel("Alcohol content",plotOutput("coolplot")),
        tabPanel("Details",DT::dataTableOutput("results"))
      ) 
     # plotOutput("coolplot"),
      #br(), br(),
      #DT::dataTableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  #resultNum <- nrow(filtered())
  
  output$resultNum <- renderText({paste("We have found ", nrow(filtered()), "results for you")}) 
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram(fill = "#db3a13") + ggtitle("The Alcohol_Content Histogram") +
      theme(
        plot.title=element_text(family='', face='bold', colour='black', size=18, hjust=0.5, vjust=0.5)
      )
  })

  output$results <- DT::renderDataTable({
    #filtered()
    if(input$checkbox == TRUE){
      tableData <- filtered()
      tableData %>% arrange(Price)
    }else{
      filtered()
    }
  })
}

shinyApp(ui = ui, server = server)
