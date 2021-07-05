library(stringr)
inf5 %>%
  filter(str_sub(SerSen, 1, 3) %in% c("119", "111")) %>%
  arrange(NodoA, NodoB)

