library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#input
inf5 <- read_delim("data/salida.txt", delim = ";") %>%
  mutate_at(c("TIEMPO", "DISTANCIA", "FLUJO_LIN",
              "FLUJO_TOT", "TASA_USO",  "SUBEN_NA",
              "SUBEN_NB",  "BAJAN_NA",  "BAJAN_NB"), 
              as.numeric) %>%
  separate(col = Linea, into = c("ws", "ID", "Ruta", "Sentido")) %>%
  mutate(SerSen = paste0(Ruta, str_to_upper(substr(Sentido, 1, 1))),
         SUBEN = SUBEN_NA,
         BAJAN = BAJAN_NB) %>%
    select(-c("ws", "ID", "Ruta", "Sentido"))

#summary
inf5_sum <- group_by(inf5, SerSen) %>%
  summarise(TIEMPO = sum(TIEMPO),
            DISTANCIA = sum(DISTANCIA), 
            FLUJO_LIN = sum(FLUJO_LIN),
            TASA_USO = sum(TASA_USO),  
            SUBEN = sum(SUBEN),
            BAJAN = sum(BAJAN))
