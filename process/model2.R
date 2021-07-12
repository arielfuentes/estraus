#libraries ----
library(tidymodels)
#split data ----
set.seed(123)
inf5_users_split <- initial_split(inf5_users_sum, 
                                  prop = 3/4, 
                                  strata = Usu)
inf5_train <- training(inf5_users_split)
inf5_test <- testing(inf5_users_split)
##training proportion ----
inf5_train %>%
  count(Usu) %>%
  mutate(prop = n/sum(n))
inf5_test %>%
  count(Usu) %>%
  mutate(prop = n/sum(n))
#define model ----
rf_mod <- 
  rand_forest(trees = 3000) %>% 
  set_engine("ranger") %>%
  set_mode("regression")
#set recipe ----
rf_recipe <- recipe(SUBEN ~ ., data = inf5_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())
#create workflow ----
rf_wkf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)
#fit model ----
set.seed(234)
rf_fit <- rf_wkf %>%
  fit(inf5_train)
#prediction ----
rf_train_pred <- predict(rf_fit, inf5_train) %>%
  bind_cols(select(inf5_train, 
                   Usu, 
                   SUBEN, 
                   SerSen, 
                   TARIFA, 
                   FREC, 
                   DISTANCIA, 
                   TIEMPO))

rf_test_pred <- predict(rf_fit, inf5_test) %>%
  bind_cols(select(inf5_test, 
                   Usu, 
                   SUBEN, 
                   SerSen, 
                   TARIFA, 
                   FREC, 
                   DISTANCIA, 
                   TIEMPO))
##pred plot ----
select(rf_train_pred, SUBEN, .pred) %>%
  ggplot(aes(.pred, SUBEN)) +
  geom_point() + 
  geom_smooth(formula= y~x)
select(rf_test_pred, SUBEN, .pred) %>%
  ggplot(aes(.pred, SUBEN)) +
  geom_point() + 
  geom_smooth(formula= y~x)
select(rf_train_pred, SUBEN, .pred, Usu) %>%
  ggplot(aes(.pred, SUBEN)) +
  geom_point() + 
  facet_wrap(~Usu) +
  geom_smooth(formula= y~x)
select(rf_test_pred, SUBEN, .pred, Usu) %>%
  ggplot(aes(.pred, SUBEN)) +
  geom_point() + 
  facet_wrap(~Usu) +
  geom_smooth(formula= y~x)

##test performance ----
rf_train_pred %>%
  metrics(SUBEN, .pred)
rf_test_pred %>%
  metrics(SUBEN, .pred)
##residual data
SUBEN_res <- rf_test_pred %>%
  arrange(.pred) %>%
  mutate(res = (SUBEN - .pred)/.pred) %>%
  select(.pred, res)
###plot ----
ggplot(SUBEN_res, aes(.pred, res)) +
  geom_point() + 
  geom_smooth(formula= y~x)
##pred new data ----
rf_new_pred <- predict(rf_fit, inf5_users_sum_new) %>%
  bind_cols(select(inf5_users_sum_new, 
                   Usu, 
                   SUBEN, 
                   SerSen, 
                   TARIFA, 
                   FREC, 
                   DISTANCIA, 
                   TIEMPO))
ggplot(rf_new_pred, aes(.pred, SUBEN)) +
  geom_point() + 
  facet_wrap(~Usu) +
  geom_smooth(formula= y~x)