#libraries ----
library(tidymodels)
#split data ----
set.seed(123)
inf5_users <- na.omit(inf5_users)
inf5_users_split <- initial_split(inf5_users #%>% select(-A_B)
                                  , 
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
  rand_forest(trees = 1000) %>% 
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
rf_test_pred <- predict(rf_fit, inf5_test) %>%
  bind_cols(select(inf5_test, Usu, SUBEN, SerSen))
rf_new_pred <- predict(rf_fit, na.omit(inf5_users_pred_dt)) %>%
  bind_cols(inf5_users_pred_dt)













#performance ----
rf_test_pred %>%
  roc_auc()
# #test model ----
# rf_testing_pred <- 
#   predict(rf_fit, inf5_test) %>% 
#   bind_cols(predict(rf_fit, inf5_test, type = "prob")) %>% 
#   bind_cols(cell_test %>% select(class))
