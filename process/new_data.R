library(stringr)
#data
# n_dt <- inf5 %>%
#   filter(str_sub(SerSen, 1, 3) %in% c("119", "111")) %>%
#   select(NodoA, NodoB, TIEMPO, DISTANCIA, SerSen,  
#          SUBEN_NA, BAJAN_NA, SUBEN_NB, BAJAN_NB) %>%
#   arrange(NodoA, NodoB, SerSen) %>%
#   ungroup()

n_dtI <- inf5 %>%
  filter(SerSen %in% c("119I", "111I")) %>%
  select(NodoA, NodoB, TIEMPO, DISTANCIA, SerSen,  
         SUBEN_NA, BAJAN_NA, SUBEN_NB, BAJAN_NB) %>%
  arrange(NodoA, NodoB, SerSen) %>%
  ungroup()

n_dtR <- inf5 %>%
  filter(SerSen %in% c("119R", "111R")) %>%
  select(NodoA, NodoB, TIEMPO, DISTANCIA, SerSen,  
         SUBEN_NA, BAJAN_NA, SUBEN_NB, BAJAN_NB) %>%
  arrange(NodoA, NodoB, SerSen) %>%
  ungroup()

#tibble for nodes 
ndtI_ndA <- n_dtI %>%
  group_by(NodoA) %>%
  summarise(SUBEN_NA = sum(SUBEN_NA),
            BAJAN_NA = sum(BAJAN_NA)) %>%
  select(name = NodoA, SUBEN = SUBEN_NA, BAJAN = BAJAN_NA)
ndtI_ndB <- n_dtI %>%
  group_by(NodoB) %>%
  summarise(SUBEN_NB = sum(SUBEN_NB),
            BAJAN_NB = sum(BAJAN_NB)) %>%
  select(name = NodoB, SUBEN = SUBEN_NB, BAJAN = BAJAN_NB)
ndtI_nd <- bind_rows(ndtI_ndA, ndtI_ndB) %>%
  group_by(name) %>% ##rm duplicates/group
  filter(row_number(SUBEN)  == 1 & row_number(BAJAN)  == 1) %>%
  ungroup()
rm(ndtI_ndA, ndtI_ndB)

ndtR_ndA <- n_dtR %>%
  group_by(NodoA) %>%
  summarise(SUBEN_NA = sum(SUBEN_NA),
            BAJAN_NA = sum(BAJAN_NA)) %>%
  select(name = NodoA, SUBEN = SUBEN_NA, BAJAN = BAJAN_NA)
ndtR_ndB <- n_dtR %>%
  group_by(NodoB) %>%
  summarise(SUBEN_NB = sum(SUBEN_NB),
            BAJAN_NB = sum(BAJAN_NB)) %>%
  select(name = NodoB, SUBEN = SUBEN_NB, BAJAN = BAJAN_NB)
ndtR_nd <- bind_rows(ndtR_ndA, ndtR_ndB) %>%
  group_by(name) %>% ##rm duplicates/group
  filter(row_number(SUBEN)  == 1 & row_number(BAJAN)  == 1) %>%
  ungroup()
rm(ndtR_ndA, ndtR_ndB)

#create graph
ndtI_grph <- as_tbl_graph(n_dtI)
ndtR_grph <- as_tbl_graph(n_dtR)

 ggraph(ndtI_grph, layout = 'kk') + 
  geom_edge_link(aes(color = SerSen)) +
  geom_node_text(aes(label = name), size = 2)

 ggraph(ndtR_grph, layout = 'kk') + 
   geom_edge_link(aes(color = SerSen)) +
   geom_node_text(aes(label = name), size = 2)
 
#to create new data 
v_1I <- filter(n_dtI, SerSen == "111I")[23:62,]
v_2I <- filter(n_dtI, SerSen == "119I")[43:100,]
v_1R <- filter(n_dtR, SerSen == "111R")[28:62,]
v_2R <- filter(n_dtR, SerSen == "119R")[37:110,]
n_dt2I <- bind_rows(v_1I, v_2I) %>%
  mutate(SerSen ="E01I")

n_dt2R <- bind_rows(v_1R, v_2R) %>%
  mutate(SerSen ="E01R")

n_dt2 <- bind_rows(n_dt2I, n_dt2R)
rm(v_1I, v_1R, v_2I, v_2R, n_dt2I, n_dt2R)  