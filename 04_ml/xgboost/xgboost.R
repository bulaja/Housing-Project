library(janitor)
library(tidymodels)
library(tidyverse)
library(DMwR)
library(treesnip)
library(doParallel)
library(tictoc)
library(xgboost)
library(furrr)

# Load functions ----

source('01_functions/load_data.R')
# source('01_functions/get_predictions_parsnip.R')
# source('01_functions/get_optimal_predictions.R')
# source('01_functions/evalerr_xgb.R')

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Data prep ----

rec <- recipe(eur_sqm ~ ., df) %>% 
  prep()

train_featured_baked <- bake(rec, df)
test_featured_baked <- bake(rec, df)

set.seed(11)
val_split <- initial_split(train_featured_baked, prop = 0.8, strata = eur_sqm)
train <- val_split %>% training()
val <- val_split %>% testing()

# XGBoost ----

dtrain <- xgb.DMatrix(data = data.matrix(train %>% select(-eur_sqm)),
                      label = train %>% select(eur_sqm) %>% pull)
dtest <- xgb.DMatrix(data = data.matrix(val %>% select(-eur_sqm)),
                     label = val %>% select(eur_sqm) %>%pull)



set.seed(11)


watchlist <- list(train = dtrain, test = dtest)

params <- list(objective = 'reg:squarederror',
               eta = c(0.001, 0.003 0.01),
               max_depth = c(5,9))
xgb_cv$params

xgb_cv <- xgb.cv(params = params,
       data = dtrain,
       nrounds = 1000,
       print_every_n = 10,
       nfold = 5,
       prediction = T,
       metrics = list('mae', 'rmse'),
       early_stopping_rounds = 5)

install.packages("jsonlite")
devtools::install_url('https://github.com/microsoft/LightGBM/releases/download/v3.0.0/lightgbm-3.0.0-r-cran.tar.gz')

xgb_cv
  
xgboost <- xgb.train(booster = 'gbtree',
                     data = dtrain, 
                     max_depth=9, 
                     eta=1, 
                     nrounds=200, 
                     watchlist=watchlist,
                     eval_metric = 'mae',
                     eval_metric = 'rmse',
                     objective = "reg:squarederror")
