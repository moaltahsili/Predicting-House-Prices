

elasitic_model <- linear_reg(penalty = tune(),
                             mixture = tune()) %>% 
  # Set the model engine
  set_engine('glmnet') %>% 
  # Set the model mode
  set_mode('regression')

house_recipe <- recipe(adj_price ~ ., data = house_training) %>% 
  step_rm(id, Date) %>% 
  # Removed correlated predictors
  step_corr(all_numeric(), threshold = 0.85) %>% 
  # Log transform numeric predictors
  step_log(all_outcomes(), base = exp(1)) %>%
  # Zero Variance Filter
  step_zv(all_numeric(), -all_outcomes()) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric(), -all_outcomes()) %>%
  # Create dummy variables
  step_dummy(all_nominal())


# Train recipe
house_recipe_prep <- house_recipe %>% 
  prep(training = house_training)



# Transform training data
house_training_enr <- house_recipe_prep %>% 
  bake(new_data = NULL)

folds_enr <- vfold_cv(house_training_enr, v = 6)


set.seed(123)
doParallel::registerDoParallel()

enr_grid <- expand_grid(penalty = seq(0,100, by = 20),
                        mixture = seq(0,1, by = 0.25))

tune_results_enr <- tune_grid(elasitic_model,
                             adj_price ~ .,
                             resamples = folds_enr,
                             grid = enr_grid,
                             metrics = metric_set(mae, rmse, rsq))

show_best(tune_results_enr, metric = "rmse")
show_best(tune_results_enr, metric = "rsq")
show_best(tune_results_enr, metric = "mae")


autoplot(tune_results_enr)

final_enr <- elasitic_model %>% 
  finalize_model(select_best(tune_results_enr))

best_enr_model <- final_enr %>% fit(adj_price ~ ., house_training_enr)

# Transform test data
house_test_enr <- house_recipe_prep %>% 
  bake(new_data = house_test)

enr_result <- predict(best_enr_model, new_data = house_test_enr)

# Combine test data with predictions
house_test_results_enr <- house_test_enr %>% 
  select(adj_price) %>% 
  bind_cols(enr_result)


# Caculate the RMSE metric
house_test_results_enr %>% 
  rmse(adj_price, .pred)
## rmse    standard       0.370

# Calculate the R squared metric
house_test_results_enr %>% 
  rsq(adj_price, .pred)
## rsq     standard       0.495

# Caculate the MAE metric
house_test_results_enr %>% 
  mae(adj_price, .pred) 
## mae     standard       0.281

##############################


house_training_long <- long_data %>% 
  filter(Date < "2021-09-01")

house_test_long <- long_data %>% 
  filter(Date >= "2021-09-01")



elasitic_model <- linear_reg(penalty = tune(),
                             mixture = tune()) %>% 
  # Set the model engine
  set_engine('glmnet') %>% 
  # Set the model mode
  set_mode('regression')

house_recipe <- recipe(adj_price ~ ., data = house_training) %>% 
  step_rm(id, Date) %>% 
  # Removed correlated predictors
  step_corr(all_numeric(), threshold = 0.85) %>% 
  # Log transform numeric predictors
  step_log(all_outcomes(), base = exp(1)) %>%
  # Zero Variance Filter
  step_zv(all_numeric(), -all_outcomes()) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric(), -all_outcomes()) %>%
  # Create dummy variables
  step_dummy(all_nominal())


# Train recipe
house_recipe_prep <- house_recipe %>% 
  prep(training = house_training)

# Transform training data
house_training_enr <- house_recipe_prep %>% 
  bake(new_data = NULL)

folds_enr <- vfold_cv(house_training, v = 6)


set.seed(123)
doParallel::registerDoParallel()

enr_grid <- expand_grid(penalty = seq(0,1, by = 0.2),
                        mixture = seq(0,1, by = 0.25))

tune_results_enr <- tune_grid(elasitic_model,
                              house_recipe,
                              resamples = folds_enr,
                              grid = enr_grid,
                              metrics = metric_set(mae, rmse, rsq))

show_best(tune_results_enr, metric = "rmse")
show_best(tune_results_enr, metric = "rsq")
show_best(tune_results_enr, metric = "mae")


autoplot(tune_results_enr, metric = "mae")

final_enr <- elasitic_model %>% 
  finalize_model(select_best(tune_results_enr))

best_enr_model <- final_enr %>% fit(adj_price ~ ., house_training_enr)

# Transform test data
house_test_enr <- house_recipe_prep %>% 
  bake(new_data = house_test)

enr_result <- predict(best_enr_model, new_data = house_test_enr)

# Combine test data with predictions
house_test_results_enr <- house_test_enr %>% 
  select(adj_price) %>% 
  bind_cols(enr_result)

house_test_results_enr <- house_test_results_enr %>% 
  mutate(actual_price = exp(adj_price),
         predicted_price = exp(.pred))


# Caculate the RMSE metric
house_test_results_enr %>% 
  rmse(adj_price, .pred)
## rmse    standard       0.370

# Calculate the R squared metric
house_test_results_enr %>% 
  rsq(adj_price, .pred)
## rsq     standard       0.495

# Caculate the MAE metric
house_test_results_enr %>% 
  mae(adj_price, .pred)
## mae     standard       0.281

### Actual Price Errors:

# Caculate the RMSE metric
house_test_results_enr %>% 
  rmse(actual_price, predicted_price)
# rmse    standard     442,868

# Calculate the R squared metric
house_test_results_enr %>% 
  rsq(actual_price, predicted_price)
## rsq     standard       0.460

# Caculate the MAE metric
house_test_results_enr %>% 
  mae(actual_price, predicted_price)
## mae     standard       292,694

