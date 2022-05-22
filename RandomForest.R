library(randomForest)

set.seed(1000)
RF_model1 <- randomForest(adj_price ~ . - id, 
                          data=train.wide.data, 
                          importance=TRUE, 
                          ntree=30,
                          proximity=TRUE)


RF_model1
## % Var explained: 87.43

RF_p1 <- predict(RF_model1, newdata=test.wide.data)

RF_p1 <- as.data.frame(RF_p1)



comp2<-cbind(test.val1, RF_p1)

colnames(comp2)<-c("Medv", "Pred")

cor.test(comp2$Medv, comp2$Pred)
## 0.9105031

RMSE(comp2$Medv, comp2$Pred)
## 693335.7

RF_model2 <- randomForest(adj_price ~ . - id, 
                          data=train.wide.data, 
                          importance=TRUE, 
                          ntree=200,
                          proximity=TRUE)

RF_summary <- summary(RF_model2)

RF_model2
## % Var explained: 88.28

RF_p2 <- predict(RF_model2, newdata=test.wide.data)

RF_p2 <- as.data.frame(RF_p2)

as.data.frame(importance(RF_model2)) %>% arrange(desc(IncNodePurity))



comp3<-cbind(test.val1, RF_p2)

colnames(comp3)<-c("Medv", "Pred")

cor.test(comp3$Medv, comp3$Pred)
## 0.9129333 

RMSE(comp3$Medv, comp3$Pred)
## 684294.1

ranger_recipe <- recipe(formula = adj_price ~ ., data = house_training) %>% 
  step_rm(id, Date) %>% 
  # Log transform numeric predictors
  step_log(all_outcomes(), base = exp(1))

house_recipe_prep_rf <- ranger_recipe %>% 
  prep(training = house_training)

# Transform training data
house_training_prep_tree <- house_recipe_prep_rf %>% 
  bake(new_data = NULL)

folds_ranger <- vfold_cv(house_training, v = 6)

library(usemodels)

use_ranger(adj_price ~ . , data = house_training)


ranger_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 200) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 


set.seed(57060)
doParallel::registerDoParallel()
ranger_tune <- tune_grid(ranger_spec,
                         ranger_recipe,
                         resamples = folds_ranger, 
                         grid = 10,
                         metrics = metric_set(mae,rmse,rsq),
                         control = control_grid(verbose = TRUE))

show_best(ranger_tune, metric = "rmse")
# A tibble: 5 × 8
# mtry min_n .metric .estimator  mean     n std_err .config              
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1     7     7 rmse    standard   0.302     6  0.0123 Preprocessor1_Model07
# 2     6    12 rmse    standard   0.304     6  0.0116 Preprocessor1_Model01
# 3    10    16 rmse    standard   0.305     6  0.0118 Preprocessor1_Model08
# 4    12    19 rmse    standard   0.307     6  0.0114 Preprocessor1_Model04
# 5     8    22 rmse    standard   0.307     6  0.0113 Preprocessor1_Model03
show_best(ranger_tune, metric = "rsq")
show_best(ranger_tune, metric = "mae")


autoplot(ranger_tune, metric = "mae")

rf_spec <- rand_forest() %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("regression")

final_rf <- ranger_spec %>% 
  finalize_model(select_best(ranger_tune))

best_rf_model <- final_rf %>% fit(adj_price ~ ., house_training_prep_tree)

# Transform test data
house_test_prep_rf <- house_recipe_prep_rf %>% 
  bake(new_data = house_test)

rf_result <- predict(best_rf_model, new_data = house_test_prep_rf)

# Combine test data with predictions
home_test_results_rf <- house_test_prep_rf %>% 
  select(adj_price) %>% 
  bind_cols(rf_result)

home_test_results_rf <- home_test_results_rf %>% 
  mutate(actual_price = exp(adj_price),
         predicted_price = exp(.pred))


# Caculate the RMSE metric
home_test_results_rf %>% 
  rmse(adj_price, .pred)
## rmse    standard       0.235

# Calculate the R squared metric
home_test_results_rf %>% 
  rsq(adj_price, .pred)
## rsq     standard       0.803

# Caculate the MAE metric
home_test_results_rf %>% 
  mae(adj_price, .pred) 
## mae     standard       0.159

### Actual Price Errors:

# Caculate the RMSE metric
home_test_results_rf %>% 
  rmse(actual_price, predicted_price)
# rmse    standard     260,980

# Calculate the R squared metric
home_test_results_rf %>% 
  rsq(actual_price, predicted_price)
## rsq     standard       0.826

# Caculate the MAE metric
home_test_results_rf %>% 
  mae(actual_price, predicted_price)
## mae     standard       164,067
























