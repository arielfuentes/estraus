#main file
source("process/dt_wrg.R")
source("process/graph_data.R")
source("process/graph_data.R")
# shiny::runApp('Rutas')
rmarkdown::render(input = "output/reporte Estimación.Rmd", 
                  output_file = "reporte_Estimacion", 
                  output_dir = "output/", 
                  encoding = "utf8")
