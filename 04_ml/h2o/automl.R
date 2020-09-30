library(h2o)
library(DataExplorer)
library(correlationfunnel)
# Load data ----

source('01_functions/load_data.R')

# Functions ----

# Initialize H2O JVM
h2o.init()
# Split data into Train/Validation/Test Sets
df_h2o <- as.h2o(df_ml)

split_h2o <- h2o.splitFrame(df_h2o, c(0.7, 0.15), seed = 11 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 15%

# Set names for h2o
y <- "eur_sqm"
x <- setdiff(names(train_h2o), y)

# Run the automated machine learning 
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = valid_h2o,
  max_runtime_secs  = 1200
)
leaderboard <- h2o.get_leaderboard(automl_models_h2o)
leaderboard
mean(df_ml$eur_sqm)

as.data.frame(leaderboard)
write.csv(as.data.frame(leaderboard), "h2o results with macro.csv")


model_ids <- as.vector(automl_models_h2o@leaderboard$model_id)
model <- h2o.getModel(model_ids[3])

pred <- h2o.predict(model, newdata= test_h2o)

round(pred)
true_values <- as.data.frame(test_h2o)
true_values$Predicted <- as.vector(round(pred))
write.csv(true_values,"true_values.csv")
#true_values <- read.csv("true_values.csv")
all_values <- true_values %>% rename(Prediction = Predicted, Actual = eur_sqm)
all_values <- all_values %>% mutate(observation = row_number() %>% as.character() %>% as_factor()) %>%
  gather(key = "key", value = "value", Prediction, Actual, factor_key = TRUE)
write.csv(all_values, "all_values.csv")


write.csv(as.data.frame(h2o.varimp(model)),"varimp.csv")

h2o.varimp(model) %>%
 # mutate(variable = sapply(str_split(variable,"\\."),'[',1)) %>% 
 # distinct(variable, .keep_all=TRUE) %>%
  top_n(20) %>% 
  hchart('bar', hcaes(x = variable, y = scaled_importance)) %>%  
  hc_xAxis(title = list(text = ""))%>% 
  hc_yAxis(title = list(text = "Scaled Importance"), max=1) %>% 
  hc_title(text = "Variable Importance") %>%
  hc_add_theme(hc_theme_elementary())
