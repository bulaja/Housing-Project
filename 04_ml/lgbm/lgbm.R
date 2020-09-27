library(janitor)
library(lightgbm)
library(tidymodels)
library(tidyverse)
library(tictoc)

# Load data ----

source('01_functions/load_data.R')

# Functions ----

source('01_functions/train_automl.R')
#source('01_functions/train_grid.R')
#source('01_functions/get_predictions.R')
#source('01_functions/get_optimal_predictions.R')

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

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

# Tuning round 1 ----

params <- as_tibble(expand.grid(learning_rate = c(0.1, 0.01, 0.001),
                                num_iterations = 1000,
                                num_leaves = c(8,16,32,64,128,256,512,1024),
                                max_depth = c(3,4,5,6,7,8,9,10),
                                #bagging_fraction = c(0.6, 0.8, 1),
                                #feature_fraction = c(0.6, 0.8, 1),
                                early_stopping_rounds = 100))
table(params$num_leaves)
table(params$max_depth)

tic()
set.seed(11)
temp <- pmap_dfr(params, ~tibble(models = list(lgb.cv(params = list(learning_rate = ..1,
                                                                    num_iterations = ..2,
                                                                    num_leaves = ..3,
                                                                    max_depth = ..4),#,
                                                      #bagging_fraction = ..5,
                                                      #feature_fraction = ..5,
                                                      #early_stopping_rounds = ..6),
                                                      obj = 'regression',
                                                      data = dtrain,
                                                      nfold = 5L,
                                                      eval = c('mae', 'rmse'),
                                                      eval_freq = 100,
                                                      verbose = 1,
                                                      early_stopping_rounds = 100)),
                                 learning_rate = ..1,
                                 num_iterations = ..2,
                                 num_leaves = ..3,
                                 max_depth = ..4))#,
#bagging_fraction = ..5,
#feature_fraction = ..5,
#early_stopping_rounds = ..6))


toc() # 6810.19 sec elapsed

temp %>% view()

save(temp, file="lgbmcvresults.RData")
load("lgbmcvresults.RData")
temp$models[[1]]$best_iter
best_round <- c()
for (i in 1:192) {
best_round <- c(best_round,temp$models[[i]]$best_iter)
  
}
temp <- temp %>% add_column(best_iter = best_round)

l1_score <- c()
for (i in 1:192){
 l1_score <- c(l1_score, 
    temp$models[[i]]$record_evals$valid %>%
    as_data_frame() %>% 
    slice(1) %>% 
    unnest(cols = everything()) %>% 
    unnest(cols = everything()) %>%
    select(l1) %>%
    slice(best_round[i]))
}
unlist(l1_score)
temp <- temp %>% add_column(l1 = unlist(l1_score))
temp_sorted <- temp %>% arrange(l1)





temp$models %>% 
  map_dfr(~print(.$best_score))


write.csv(temp_sorted %>% select(-models), "lgbm 20k cv.csv")

# Tuning round 2 ----

params <- as_tibble(expand.grid(learning_rate = c(0.01, 0.03),
                                num_iterations = 1000,
                                num_leaves = c(32,64,128,256,512,1024,2048),
                                max_depth = c(5,6,7,8,9,10,11),
                                bagging_fraction = c(0.6, 0.8, 1),
                                feature_fraction = c(0.6, 0.8, 1),
                                early_stopping_rounds = 100))

tic()
set.seed(11)
temp <- pmap_dfr(params, ~tibble(models = list(lgb.cv(params = list(learning_rate = ..1,
                                                                    num_iterations = ..2,
                                                                    num_leaves = ..3,
                                                                    max_depth = ..4
                                                      bagging_fraction = ..5,
                                                      feature_fraction = ..6),
                                                      obj = 'regression',
                                                      data = dtrain,
                                                      nfold = 5L,
                                                      eval = c('mae', 'rmse'),
                                                      eval_freq = 100,
                                                      verbose = 1,
                                                      early_stopping_rounds = 100)),
                                 learning_rate = ..1,
                                 num_iterations = ..2,
                                 num_leaves = ..3,
                                 max_depth = ..4,
                                 bagging_fraction = ..5,
                                 feature_fraction = ..6))


toc() # 6810.19 sec elapsed





# Best model ----


params <- list(objective = 'regression',
               learning_rate = 0.01,
               num_iterations = 1000,
               num_leaves = 128,
               max_depth = 8)



temp <- lgb.cv(params = params,
               data = dtrain,
               nfold = 5L,
               eval = c('mae','mse','rmse'),
               eval_freq = 10,
               early_stopping_rounds = 10)



lgbm_best <- lgb.train(params = params,
                       data = dtrain,
                       valids = list(test = dtest),
                       eval = c('mae', 'rmse'),
                        nrounds = 10)

lgbm_pred <- predict(lgbm_best, x_test)


