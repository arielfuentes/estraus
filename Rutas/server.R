library(shiny)
library(dplyr)
library(ggplot2)
# Define data to be plotted and parametric to use
server <- function(input, output) {
    
    #Data
    data_sum <- reactive({
        req(input$flt)
        df <- inf5_sum %>%
            select(SerSen, input$flt)
    })
    
    data <- reactive({
        req(input$flt)
        df <- inf5 %>%
            select(SerSen, input$flt)
    })
    
    #Plot 
    output$plotbar <- renderPlot({
        g <- ggplot(data_sum(), aes_string("SerSen", input$flt))
        g + geom_col()
    })
    
    output$plotviolin <- renderPlot({
        g <- ggplot(data(), aes_string("SerSen", input$flt))
        g + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
    })
}