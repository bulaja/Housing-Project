train_automl <- function(grid_id, data) {
  y = 'eur_sqm'
  x = setdiff(names(data), y)
  
  automl <- h2o.automl(x = x,
                       y = y,
                       training_frame = as.h2o(data),
                       nfolds = 5,
                       max_runtime_secs = 300,
                       seed = 11)
  
  automl
}