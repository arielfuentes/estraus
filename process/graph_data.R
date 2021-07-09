#library ----
library(tidygraph)
library(dplyr)
library(ggplot2)
library(ggraph)

#tbl for edges ----
inf5_edg <- inf5 %>%
  group_by(NodoA, NodoB) %>%
  summarise(TIEMPO = mean(TIEMPO),
            DISTANCIA = mean(DISTANCIA),
            FLUJO_TOT = mean(FLUJO_TOT)) %>%
  ungroup()

#tibble for nodes ----
inf5_ndA <- inf5 %>%
  group_by(NodoA) %>%
  summarise(SUBEN_NA = sum(SUBEN_NA),
            BAJAN_NA = sum(BAJAN_NA)) %>%
  mutate(NodoA = as.character(NodoA)) %>%
  select(name = NodoA, SUBEN = SUBEN_NA, BAJAN = BAJAN_NA)
inf5_ndB <- inf5 %>%
  group_by(NodoB) %>%
  summarise(SUBEN_NB = sum(SUBEN_NB),
            BAJAN_NB = sum(BAJAN_NB)) %>%
  mutate(NodoB = as.character(NodoB)) %>%
  select(name = NodoB, SUBEN = SUBEN_NB, BAJAN = BAJAN_NB)
inf5_nd <- bind_rows(inf5_ndA, inf5_ndB) %>%
  group_by(name) %>%
  summarise(SUBEN = sum(SUBEN), BAJAN = sum(BAJAN)) %>%
  ungroup()
rm(inf5_ndA, inf5_ndB)

#create graph ----
inf5_grph <- as_tbl_graph(inf5_edg)
##add data to nodes ----
inf5_grph <- inf5_grph %>%
  activate(nodes) %>%
  left_join(inf5_nd)
rm(inf5_edg, inf5_nd)
