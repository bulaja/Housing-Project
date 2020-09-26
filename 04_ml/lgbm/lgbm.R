library(janitor)
library(lightgbm)
library(tidymodels)
library(tidyverse)
library(tictoc)

# Load data ----

source('01_functions/load_data.R')

# Functions ----

source('01_functions/train_automl.R')
source('01_functions/train_grid.R')
source('01_functions/get_predictions.R')
source('01_functions/get_optimal_predictions.R')


# Recipes ----

rec <- recipe(eur_sqm ~ ., train_data) %>% 
  prep()


train_featured_baked <- bake(rec, train_data)

test_featured_baked <- bake(rec, test_data)

# LGBM ----

x <- data.matrix(train_featured_baked %>% select(-eur_sqm))
y <- train_featured_baked %>% select(eur_sqm) %>% pull

x_test <- data.matrix(test_featured_baked %>% select(-eur_sqm))
y_test <- test_featured_baked %>% select(eur_sqm) %>% pull


dtrain <- lgb.Dataset(data = x, label = y)
dtest <- lgb.Dataset(data = x_test, label = y_test)

params <- list(objective = 'regression',
               learning_rate = 0.001,
               num_iterations = 1500)



temp <- lgb.cv(params = params,
               data = dtrain,
               nfold = 5L,
               eval = c('mae','mse','rmse'),
               eval_freq = 10,
               early_stopping_rounds = 10)


# CV results ---- 
# check later
"
imap_dfr(temp$record_evals$valid, ~data.frame(metric = .y, value = .x)) %>% 
  unnest_wider(value) %>% 
  slice(1, 3, 5) %>% 
  pivot_longer(cols = -metric) %>% 
  select(-name) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  unnest(cols = everything()) %>% 
  arrange(binary_logloss) %>%
  colMeans
"

lgbm_best <- lgb.train(params = params,
                       data = dtrain,
                       valids = list(test = dtest),
                       eval = c('mae', 'rmse'),
                        nrounds = 10)

lgbm_pred <- predict(lgbm_best, x_test)
lgbm_pred <- tibble(p1 = lgbm_pred) %>% 
  mutate(predict = factor(ifelse(p1 > 0.5, 1, 0), levels = c(1, 0)),
         eur_sqm = factor(y_test, levels = c(1, 0)))



caret::confusionMatrix(lgbm_pred$predict, lgbm_pred$eur_sqm, mode = 'everything')

lgbm_pred <- get_optimal_predictions(lgbm_pred)

lgbm_metrics <- bind_rows(bind_cols(bind_rows(accuracy(lgbm_pred, eur_sqm, predict),
                                              f_meas(lgbm_pred, predict, eur_sqm),
                                              recall(lgbm_pred, predict, eur_sqm),
                                              precision(lgbm_pred, predict, eur_sqm)),
                                    model = 'Lgbm base',
                                    threshold = 0.5),
                          bind_cols(bind_rows(accuracy(lgbm_pred, p_optimal, eur_sqm),
                                              f_meas(lgbm_pred, p_optimal, eur_sqm),
                                              recall(lgbm_pred, p_optimal, eur_sqm),
                                              precision(lgbm_pred, p_optimal, eur_sqm)),
                                    model = 'Lgbm base',
                                    threshold = unique(lgbm_pred$optimal_ts)))


lgbm_pred %>% 
  accuracy(predict, eur_sqm)

lgbm_pred %>% 
  f_meas(predict, eur_sqm)

lgbm_pred %>% 
  recall(eur_sqm, predict)

lgbm_pred %>% 
  accuracy(eur_sqm, p_optimal)

lgbm_pred %>% 
  f_meas(eur_sqm, p_optimal)

lgbm_pred %>% 
  recall(eur_sqm, p_optimal)

# Tuning ----
set.seed(11)
grid <- grid_random(parameters(finalize(mtry(), train_featured_baked),
                               trees(),
                               min_n(),
                               tree_depth(),
                               learn_rate(),
                               loss_reduction(),
                               sample_prop()),
                    size = 1000)

params <- as_tibble(expand.grid(learning_rate = c(0.1, 0.01, 0.001),
                                num_iterations = 1000,
                                num_leaves = unique(grid$min_n),
                                max_depth = unique(grid$tree_depth),
                                bagging_fraction = seq(0.6, 0.8, 1),
                                feature_fraction = seq(0.6, 0.8, 1),
                                early_stopping_rounds = 10))

set.seed(11)
params #<- params %>% slice_sample(n = 3)
tic()
set.seed(11)
temp <- pmap_dfr(params, ~tibble(models = list(lgb.cv(params = list(learning_rate = ..1,
                                                                    num_iterations = ..2,
                                                                    num_leaves = ..3,
                                                                    max_depth = ..4,
                                                                    bagging_fraction = ..5,
                                                                    feature_fraction = ..6,
                                                                    early_stopping_rounds = ..7),
                                                      obj = 'regression',
                                                      data = dtrain,
                                                      nfold = 5L,
                                                      eval = c('mae', 'rmse'),
                                                      eval_freq = 100,
                                                      verbose = 1)),
                                 learning_rate = ..1,
                                 num_iterations = ..2,
                                 num_leaves = ..3,
                                 max_depth = ..4,
                                 bagging_fraction = ..5,
                                 feature_fraction = ..6,
                                 early_stopping_rounds = ..7))

toc()

temp %>% view
temp$models[[1]]$record_evals$valid %>% as_data_frame() %>% 
  slice(1) %>% 
  unnest(cols = everything()) %>% 
  unnest(cols = everything())

temp$models %>% 
  map_dfr(~print(.$best_score))

imap(temp, ~map_dfr(., ~tibble(binary = ..1$record_evals$valid$binary_logloss,
                               auc = ..1$record_evals$valid$auc)))
# s%>% 
#       unnest(cols = everything()) %>% 
#       unnest(cols = everything()))

map(temp, ~.$best_score)
map_dfr(temp, ~ tibble(best_score = .$best_score))