library(shiny)
#frontend
ui <- basicPage(
    h1("Resultado Estraus"),
    selectInput(inputId = "flt",
                label = "Variable Recorrido",
                list("TIEMPO", "DISTANCIA", "FLUJO_LIN", "TASA_USO", 
                     "SUBEN", "BAJAN", "TARIFA", "FREC")),
    selectInput(inputId = "flt_grph",
                label = "Variable Arco",
                list("TIEMPO", "DISTANCIA", "FLUJO_TOT")),
    plotOutput("plotbar"),
    plotOutput("plotviolin"),
    plotOutput("plotarco")
)