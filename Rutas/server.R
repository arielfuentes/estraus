library(shiny)
library(dplyr)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #Summarize Data and then Plot
    data <- reactive({
        req(input$flt)
        df <- inf5_sum %>%
            select(SerSen, input$flt)
    })
    
    #Plot 
    output$plot <- renderPlot({
        g <- ggplot(data(), aes_string("SerSen", input$flt))
        g + geom_col()
    })
}