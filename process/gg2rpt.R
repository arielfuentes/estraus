ggraph(inf5_grph, layout = 'kk') + 
  geom_edge_link(aes(colour = TIEMPO, width = TIEMPO)) +
  scale_edge_color_gradient(low = "yellow", high = "red") +
  geom_node_point(aes(colour = SUBEN)) +
  ggtitle("Red Antofagasta")
                  
ggraph(ndtI_grph, layout = 'kk') + 
  geom_edge_link(aes(color = SerSen)) +
  geom_node_text(aes(label = name), size = 2)

sum_predbar <- sum_pred %>%
  tidyr::pivot_longer(c("SUBEN", "SUB.Pred"), names_to = "variables")

ggplot(sum_predbar, aes(Ser, y=value, fill=variables)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle("Modelación v/s Input")

sum_predbar2 <- sum_pred %>%
  tidyr::pivot_longer(c("%SUBEN", "%SUB.Pred"), names_to = "variables")

ggplot(sum_predbar2, aes(Ser, y=value, fill=variables)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle("% Captura del Mercado")

sum_pred %>%
  mutate(delta = abs(`%SUB.Pred` - `%SUBEN`)) %>%
  arrange(delta) %>%
  ggplot(aes(reorder(Ser, -delta), delta)) +
  geom_bar(stat='identity') +
  xlab("Servicio") +
  ggtitle("Recorridos Impactados")
