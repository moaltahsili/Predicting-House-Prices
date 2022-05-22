

xg_spec <- boost_tree(
  trees = 200,
  learn_rate = tune(),
  tree_depth = tune(),
  sample_size = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost")



house_recipe_xg <- recipe(formula = adj_price ~ ., data = house_training) %>% 
  step_rm(id, Date) %>%
  # Create dummy variables
  step_dummy(all_nominal()) %>% 
  # Log transform numeric predictors
  step_log(all_outcomes(), base = exp(1))

house_prep_xg <- house_recipe_xg %>% 
  prep(training = house_training)

# Transform training data
house_training_xg <- house_prep_xg %>% 
  bake(new_data = NULL)

set.seed(6735)
folds_xg <- vfold_cv(house_training, v = 6)

xg_wf <- workflow() %>% 
  add_recipe(house_recipe_xg) %>% 
  add_model(xg_spec)

set.seed(123)
doParallel::registerDoParallel()

xgb_grid <- expand_grid(learn_rate = c(0.02,0.04,0.06),
                        tree_depth = c(4,7,10),
                        sample_size = c(0.3, 0.6, 0.9))

# A tibble: 5 × 9
# tree_depth learn_rate sample_size .metric .estimator  mean     n std_err .config              
# <dbl>      <dbl>       <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1         10       0.06         0.9 rmse    standard   0.298     6 0.00680 Preprocessor1_Model27
# 2         10       0.06         0.6 rmse    standard   0.300     6 0.00657 Preprocessor1_Model26
# 3         10       0.04         0.9 rmse    standard   0.301     6 0.00636 Preprocessor1_Model18
# 4         10       0.04         0.6 rmse    standard   0.302     6 0.00612 Preprocessor1_Model17
# 5         10       0.06         0.3 rmse    standard   0.307     6 0.00592 Preprocessor1_Model25

xgb_grid2 <- expand_grid(learn_rate = c(0.08, 0.1),
                        tree_depth = c(12, 15),
                        sample_size = c(0.92,0.97))

# A tibble: 5 × 9
# tree_depth learn_rate sample_size .metric .estimator  mean     n std_err .config             
# <dbl>      <dbl>       <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
#   1         12       0.08        0.97 rmse    standard   0.299     6 0.00675 Preprocessor1_Model2
# 2         12       0.1         0.97 rmse    standard   0.300     6 0.00746 Preprocessor1_Model6
# 3         12       0.08        0.92 rmse    standard   0.301     6 0.00699 Preprocessor1_Model1
# 4         12       0.1         0.92 rmse    standard   0.301     6 0.00754 Preprocessor1_Model5
# 5         15       0.08        0.97 rmse    standard   0.301     6 0.00702 Preprocessor1_Model4


xgb_grid3 <- expand_grid(learn_rate = c(0.06, 0.07,0.8),
                         tree_depth = c(10, 11,12),
                         sample_size = 1)

# A tibble: 5 × 9
# tree_depth learn_rate sample_size .metric .estimator  mean     n std_err .config             
# <dbl>      <dbl>       <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
#   1         11       0.06           1 rmse    standard   0.299     6 0.00693 Preprocessor1_Model2
# 2         12       0.06           1 rmse    standard   0.299     6 0.00717 Preprocessor1_Model3
# 3         11       0.07           1 rmse    standard   0.300     6 0.00711 Preprocessor1_Model5
# 4         10       0.07           1 rmse    standard   0.300     6 0.00715 Preprocessor1_Model4
# 5         12       0.07           1 rmse    standard   0.300     6 0.00718 Preprocessor1_Model6

set.seed(123)
doParallel::registerDoParallel()

tune_results_xg <- tune_grid(xg_spec,
                             house_recipe_xg,
                             resamples = folds_xg,
                             grid = xgb_grid,
                             metrics = metric_set(mae, rmse, rsq))

final_xg_spec <- boost_tree(
  trees = 100,
  learn_rate = 0.06,
  tree_depth = 10,
  sample_size = 0.9) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

best_xg_model <- final_xg_spec %>% fit(adj_price ~ ., house_training_xg)

best_xg_cv <- final_xg_spec %>% fit_resamples(house_recipe_xg,
                                              resamples = folds_xg,
                                              metrics = metric_set(mae, rmse, rsq))

all_errors <- best_xg_cv %>% collect_metrics(summarize = F)

ggplot(all_errors, aes(.estimate, fill = .metric)) +
  geom_histogram()

best_xg_cv %>% collect_metrics()
# learn_rate = 0.06,
# tree_depth = 10,
# sample_size = 0.9

# A tibble: 3 × 6
# .metric .estimator  mean     n  std_err .config             
# <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
#   1 mae     standard   0.149     6 0.000467 Preprocessor1_Model1
# 2 rmse    standard   0.300     6 0.00667  Preprocessor1_Model1
# 3 rsq     standard   0.830     6 0.00726  Preprocessor1_Model1

#### best tune:
# trees = 200,
# learn_rate = 0.06,
# tree_depth = 11,
# sample_size = 1

# A tibble: 3 × 6
# .metric .estimator  mean     n  std_err .config             
# <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
#   1 mae     standard   0.146     6 0.000495 Preprocessor1_Model1
# 2 rmse    standard   0.299     6 0.00693  Preprocessor1_Model1
# 3 rsq     standard   0.832     6 0.00757  Preprocessor1_Model1


wf_xggg <- tune_grid(xg_wf,
          resamples = folds_xg,
          grid = 10,
          metrics = metric_set(mae, rmse, rsq))

show_best(tune_results_xg, metric = "rmse")
# formula
# A tibble: 5 × 9
# tree_depth learn_rate sample_size .metric .estimator   mean     n std_err .config              
# <int>      <dbl>       <dbl> <chr>   <chr>       <dbl> <int>   <dbl> <chr>                
#   1          7  0.0369          0.603 rmse    standard    0.313     6 0.00513 Preprocessor1_Model09
# 2          8  0.00654         0.893 rmse    standard    3.47      6 0.00309 Preprocessor1_Model03
# 3          3  0.000303        0.424 rmse    standard   12.0       6 0.00382 Preprocessor1_Model10
# 4          2  0.000146        0.517 rmse    standard   12.4       6 0.00383 Preprocessor1_Model06
# 5          9  0.0000113       0.926 rmse    standard   12.7       6 0.00384 Preprocessor1_Model04

show_best(wf_xggg, metric = "rmse")
# workflow
# A tibble: 5 × 9
# tree_depth learn_rate sample_size .metric .estimator   mean     n std_err .config              
# <int>      <dbl>       <dbl> <chr>   <chr>       <dbl> <int>   <dbl> <chr>                
#   1         11 0.0268           0.384 rmse    standard    0.316     6 0.00593 Preprocessor1_Model01
# 2          6 0.00160          0.647 rmse    standard    9.28      6 0.00362 Preprocessor1_Model06
# 3          2 0.000312         0.558 rmse    standard   12.0       6 0.00383 Preprocessor1_Model10
# 4         15 0.000115         0.779 rmse    standard   12.5       6 0.00382 Preprocessor1_Model02
# 5          4 0.00000883       0.295 rmse    standard   12.8       6 0.00384 Preprocessor1_Model05

# preprocess
# A tibble: 5 × 9
# tree_depth learn_rate sample_size .metric .estimator   mean     n std_err .config              
# <int>      <dbl>       <dbl> <chr>   <chr>       <dbl> <int>   <dbl> <chr>                
#   1          8 0.0536           0.934 rmse    standard    0.304     6 0.00588 Preprocessor1_Model03
# 2         11 0.00173          0.605 rmse    standard    9.04      6 0.00360 Preprocessor1_Model06
# 3          2 0.000352         0.430 rmse    standard   11.9       6 0.00383 Preprocessor1_Model05
# 4          9 0.000122         0.252 rmse    standard   12.5       6 0.00384 Preprocessor1_Model08
# 5         14 0.00000434       0.293 rmse    standard   12.8       6 0.00384 Preprocessor1_Model01


show_best(tune_results_xg, metric = "rsq")
show_best(tune_results_xg, metric = "mae")

autoplot(tune_results_xg, metric = "mae")

final_xg <- xg_spec %>% 
  finalize_model(select_best(tune_results_xg))

set.seed(123)
doParallel::registerDoParallel()

best_xg_model <- final_xg %>% fit(adj_price ~ ., house_training_xg)

# Transform test data
house_test_xg <- house_prep_xg %>% 
  bake(new_data = house_test)

xg_result <- predict(best_xg_model, new_data = house_test_xg)

# Combine test data with predictions
house_test_results_xg <- house_test_xg %>% 
  select(adj_price) %>% 
  bind_cols(xg_result)

house_test_results_xg <- house_test_results_xg %>% 
  mutate(actual_price = exp(adj_price),
         predicted_price = exp(.pred))
  

# Caculate the RMSE metric
house_test_results_xg %>% 
  rmse(adj_price, .pred)
## rmse    standard       0.240
# long
## rmse    standard       0.350

# Calculate the R squared metric
house_test_results_xg %>% 
  rsq(adj_price, .pred)
## rsq     standard       0.802
# long
## rmse    standard       0.758

# Caculate the MAE metric
house_test_results_xg %>% 
  mae(adj_price, .pred) 
## mae     standard       0.145
# long
## rmse    standard       0.218

### Actual Price Errors:

# Caculate the RMSE metric
house_test_results_xg %>% 
  rmse(actual_price, predicted_price)
# rmse    standard     235,281

# Calculate the R squared metric
house_test_results_xg %>% 
  rsq(actual_price, predicted_price)
## rsq     standard       0.840

# Caculate the MAE metric
house_test_results_xg %>% 
  mae(actual_price, predicted_price)
## mae     standard       148,992

library(vip)

best_xg_model %>% vip(geom = "col", num_features = 13)


house_test_results_xg %>% 
  ggplot() + 
  aes(actual_price, predicted_price) + 
  geom_point(alpha = 0.3, color = "blue") + 
  geom_abline() +
  labs(title = "XGBoost Model") +
  xlab("Actual Price") +
  ylab("Predicted Price")


