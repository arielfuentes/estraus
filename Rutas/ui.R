library(shiny)
#fronend
ui <- basicPage(
    h1("Resultado Estraus"),
    selectInput(inputId = "flt",
                label = "Escoge variable",
                list("TIEMPO", "DISTANCIA", "FLUJO_LIN", 
                     "FLUJO_TOT", "TASA_USO", "SUBEN", "BAJAN")),
    plotOutput("plot")
)