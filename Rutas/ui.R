library(shiny)
#frontend
ui <- basicPage(
    h1("Resultado Estraus"),
    selectInput(inputId = "flt",
                label = "Escoge variable",
                list("TIEMPO", "DISTANCIA", "FLUJO_LIN", 
                     "TASA_USO", "SUBEN", "BAJAN")),
    plotOutput("plotbar"),
    plotOutput("plotviolin")
    
)