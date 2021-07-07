#we will consolidate the data to model ----
##selecting data that we will reuse to 
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

#Graphs to visualize the data to pickup ----
##tibble for nodes ----
###Orientation I ----
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
####Consolidated ----
ndtI_nd <- bind_rows(ndtI_ndA, ndtI_ndB) %>%
  group_by(name) %>% 
  summarise(SUBEN = sum(SUBEN), BAJAN = sum(BAJAN)) %>%
  ungroup()
rm(ndtI_ndA, ndtI_ndB)

###Orientation R ----
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
####Consolidated ----
ndtR_nd <- bind_rows(ndtR_ndA, ndtR_ndB) %>%
  group_by(name) %>%
  summarise(SUBEN = sum(SUBEN),
            BAJAN = sum(BAJAN)) %>%
  ungroup()
rm(ndtR_ndA, ndtR_ndB)

##creating graphs ----
ndtI_grph <- as_tbl_graph(n_dtI)
ndtR_grph <- as_tbl_graph(n_dtR)
###plots ----
 ggraph(ndtI_grph, layout = 'kk') + 
  geom_edge_link(aes(color = SerSen)) +
  geom_node_text(aes(label = name), size = 2)

 ggraph(ndtR_grph, layout = 'kk') + 
   geom_edge_link(aes(color = SerSen)) +
   geom_node_text(aes(label = name), size = 2)
 
#Creating new data ----
v_1I <- filter(n_dtI, SerSen == "111I")[23:62,]
v_2I <- filter(n_dtI, SerSen == "119I")[43:100,]
v_1R <- filter(n_dtR, SerSen == "111R")[28:62,]
v_2R <- filter(n_dtR, SerSen == "119R")[37:110,]
##Orientation I ----
n_dt2I <- bind_rows(v_1I, v_2I) %>%
  mutate(SerSen ="E01I")
##Orientation R ----
n_dt2R <- bind_rows(v_1R, v_2R) %>%
  mutate(SerSen ="E01R")
##Consolidated ----
n_dt2 <- bind_rows(n_dt2I, n_dt2R)
rm(v_1I, v_1R, v_2I, v_2R, n_dt2I, n_dt2R)  
#Data to model ----
##open users strata ----
us_stta <- read_delim("data/Users_strata.csv", delim = ";")
##Declassify data by users strata ----
inf5_users <- inf5 %>%
  left_join(us_stta) %>%
  mutate(SUBEN = SUBEN*Prop)
