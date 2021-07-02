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
    
    data_grph <- reactive({
        req(input$flt_grph)
        df <- inf5_grph
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
    
    output$plotarco <- renderPlot({
        ggraph(inf5_grph, layout = 'kk') + 
            geom_edge_link(aes_string(width = input$flt_grph, 
                                      colour = input$flt_grph)) +
            scale_edge_color_gradient(low = "green", high = "red") +
            geom_node_point(aes(size = input$SUBEN, colour = input$BAJAN))
    })
}