library(shiny)
library(dplyr)
library(ggplot2)
# Define data to be plotted and parametric to use
server <- function(input, output, session) {
    
    #Data
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