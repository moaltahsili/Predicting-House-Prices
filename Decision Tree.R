library(rpart)

tr_model <- rpart(adj_price ~ . - id, data=train.wide.data)

summary(tr_model)

tr_p <- predict(tr_model, newdata=test.wide.data)

tr_p <- as.data.frame(tr_p)



comp_tr <- cbind(test.val1, tr_p)

colnames(comp_tr)<-c("Medv", "Pred")

cor.test(comp_tr$Medv, comp_tr$Pred)
## 0.8148841

RMSE(comp_tr$Medv, comp_tr$Pred)
## 971,884.4

## visualizing the Decision Tree

library(rpart.plot)

prp(tr_model)

tree_spec <- decision_tree(tree_depth = tune(),
                           min_n = tune(),
                           cost_complexity = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

house_recipe_tree <- recipe(adj_price ~ ., data = house_training) %>% 
  step_rm(id, Date) %>% 
  # Log transform numeric predictors
  step_log(all_outcomes(), base = exp(1))

# Train recipe
house_recipe_prep_tree <- house_recipe_tree %>% 
  prep(training = house_training)

# Transform training data
house_training_tree <- house_recipe_prep_tree %>% 
  bake(new_data = NULL)

set.seed(1234)
folds_tree <- vfold_cv(house_training, v = 6)

tree_grid1 <- expand_grid(tree_depth = c(14, 16, 18),
                          min_n = c(10, 20, 30),
                          cost_complexity = c(1e-4, 1e-5, 1e-6))

# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n std_err .config              
# <dbl>      <dbl> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1        0.000001         18    20 rmse    standard   0.355     6  0.0109 Preprocessor1_Model24
# 2        0.00001          18    20 rmse    standard   0.356     6  0.0110 Preprocessor1_Model23
# 3        0.000001         18    30 rmse    standard   0.356     6  0.0105 Preprocessor1_Model27
# 4        0.000001         16    20 rmse    standard   0.356     6  0.0110 Preprocessor1_Model15
# 5        0.00001          18    30 rmse    standard   0.356     6  0.0106 Preprocessor1_Model26

tree_grid2 <- expand_grid(tree_depth = c(18, 20, 22),
                          min_n = c(15, 20, 25),
                          cost_complexity = c(1e-6, 1e-7))

# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n std_err .config              
# <dbl>      <dbl> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1       0.000001          22    20 rmse    standard   0.355     6  0.0109 Preprocessor1_Model15
# 2       0.0000001         22    20 rmse    standard   0.355     6  0.0109 Preprocessor1_Model16
# 3       0.000001          20    20 rmse    standard   0.355     6  0.0109 Preprocessor1_Model09
# 4       0.0000001         20    20 rmse    standard   0.355     6  0.0109 Preprocessor1_Model10
# 5       0.000001          18    20 rmse    standard   0.355     6  0.0109 Preprocessor1_Model03

tree_grid3 <- expand_grid(tree_depth = c(22, 25),
                          min_n = c(18, 20, 22),
                          cost_complexity = c(1e-6, 1e-7, 1e-8))

# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n std_err .config              
# <dbl>      <dbl> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1      0.00000001         25    18 rmse    standard   0.355     6  0.0103 Preprocessor1_Model12
# 2      0.0000001          25    18 rmse    standard   0.355     6  0.0103 Preprocessor1_Model11
# 3      0.000001           25    18 rmse    standard   0.355     6  0.0103 Preprocessor1_Model10
# 4      0.00000001         22    18 rmse    standard   0.355     6  0.0103 Preprocessor1_Model03
# 5      0.0000001          22    18 rmse    standard   0.355     6  0.0103 Preprocessor1_Model02

set.seed(123)
doParallel::registerDoParallel()

tune_results_tree <- tune_grid(tree_spec,
                             house_recipe_tree,
                             resamples = folds_tree,
                             grid = tree_grid3,
                             metrics = metric_set(mae, rmse, rsq),
                             control = control_grid(verbose = TRUE))

show_best(tune_results_tree, metric = "rmse")

# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n std_err .config              
# <dbl>      <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1        1.22e- 6         14    23 rmse    standard   0.359     6 0.00767 Preprocessor1_Model09
# 2        4.48e- 6         13    35 rmse    standard   0.363     6 0.00714 Preprocessor1_Model03
# 3        3.55e-10         11    20 rmse    standard   0.371     6 0.00724 Preprocessor1_Model06
# 4        3.71e- 9         11    16 rmse    standard   0.371     6 0.00674 Preprocessor1_Model10
# 5        2.63e- 5          9    37 rmse    standard   0.389     6 0.00692 Preprocessor1_Model04


show_best(tune_results_tree, metric = "rsq")
# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n std_err .config              
# <dbl>      <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1        1.22e- 6         14    23 rsq     standard   0.759     6 0.00831 Preprocessor1_Model09
# 2        4.48e- 6         13    35 rsq     standard   0.753     6 0.00775 Preprocessor1_Model03
# 3        3.71e- 9         11    16 rsq     standard   0.742     6 0.00738 Preprocessor1_Model10
# 4        3.55e-10         11    20 rsq     standard   0.742     6 0.00798 Preprocessor1_Model06
# 5        2.63e- 5          9    37 rsq     standard   0.715     6 0.00791 Preprocessor1_Model04
show_best(tune_results_tree, metric = "mae")

autoplot(tune_results_tree, metric = "rmse")

best_tree_spec <- decision_tree(tree_depth = 25,
                           min_n = 18,
                           cost_complexity = 1e-6) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# Train the model with the training data
tree_fit <- best_tree_spec %>% 
  fit(adj_price ~ .,
      data = house_training_prep_tree)

# Print lm_fit to view model information
tree_fit

# Transform test data
house_test_prep_tree <- house_recipe_prep_tree %>% 
  bake(new_data = house_test)

# Predict selling_price
price_predictions_tree <- predict(tree_fit,
                             new_data = house_test_prep_tree)

# Combine test data with predictions
home_test_results_tree <- house_test_prep_tree %>% 
  select(adj_price) %>% 
  bind_cols(price_predictions_tree)

home_test_results_tree <- home_test_results_tree %>% 
  mutate(actual_price = exp(adj_price),
         predicted_price = exp(.pred))


# Caculate the RMSE metric
home_test_results_tree %>% 
  rmse(adj_price, .pred)
## rmse    standard       0.284

# Calculate the R squared metric
home_test_results_tree %>% 
  rsq(adj_price, .pred)
## rsq     standard       0.702

# Caculate the MAE metric
home_test_results_tree %>% 
  mae(adj_price, .pred) 
## mae     standard       0.191

### Actual Price Errors:

# Caculate the RMSE metric
home_test_results_tree %>% 
  rmse(actual_price, predicted_price)
# rmse    standard     293,852

# Calculate the R squared metric
home_test_results_tree %>% 
  rsq(actual_price, predicted_price)
## rsq     standard       0.743

# Caculate the MAE metric
home_test_results_tree %>% 
  mae(actual_price, predicted_price)
## mae     standard       190,685

