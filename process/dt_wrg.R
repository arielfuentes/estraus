library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#input ----
##complementary operational data ---- 
###-- csv previously processed from .lpa file in excel
cmpl_dt <- read_delim("data/informe5_bus_antofapm_1.csv", delim = ";") %>%
  mutate_at(c("ID", "Ruta"), as.character) %>%
  mutate(SerSen = paste0(Ruta, str_to_upper(substr(Sentido, 1, 1)
                                            )
                         )
         ) %>%
  select(-c("Ruta", "Sentido", "Tarifa"))
##vivaldi output using lee & corre lee executable files ----
inf5 <- read_delim("data/salida.txt", delim = ";") %>%
  mutate_at(c("TIEMPO", "DISTANCIA", "FLUJO_LIN",
              "FLUJO_TOT", "TASA_USO",  "SUBEN_NA",
              "SUBEN_NB",  "BAJAN_NA",  "BAJAN_NB"), 
              as.numeric) %>%
  separate(col = Linea, into = c("ws", "ID", "Ruta", "Sentido")) %>%
  mutate(SerSen = paste0(Ruta, str_to_upper(substr(Sentido, 1, 1)
                                            )
                         )
         ) %>%
  left_join(cmpl_dt) %>%
  group_by(NodoA, NodoB, SerSen) %>%
  summarise(TIEMPO = mean(TIEMPO), 
            DISTANCIA = mean(DISTANCIA), 
            FLUJO_LIN = sum(FLUJO_LIN),
            FLUJO_TOT = mean(FLUJO_TOT), 
            TASA_USO = mean(TASA_USO),  
            SUBEN_NA = sum(SUBEN_NA),
            SUBEN_NB = sum(SUBEN_NB), 
            BAJAN_NA = sum(BAJAN_NA), 
            BAJAN_NB = sum(BAJAN_NA),
            FREC = 60/sum(interv)) %>%
  mutate(SUBEN = SUBEN_NA, BAJAN = BAJAN_NB) %>%
  ungroup()
rm(cmpl_dt)

#summary ----
inf5_sum <- group_by(inf5, SerSen) %>%
  summarise(TIEMPO = sum(TIEMPO),
            DISTANCIA = sum(DISTANCIA), 
            FLUJO_LIN = sum(FLUJO_LIN),
            TASA_USO = sum(TASA_USO),  
            SUBEN = sum(SUBEN),
            BAJAN = sum(BAJAN),
            FREC = mean(FREC))
