library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)
library(yardstick)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)


######################
# tidymodels time!
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)




#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf,
                                       best_acc
)
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "logit") %>% select(-starts_with(".config"))

# Printing the final tidy table
print(logit_ans)
library(readr)

# Save the table as CSV
write_csv(logit_ans, "logit_results.csv")

#asdfafd

#####################
# tree model
#####################

print('Starting TREE')

# Define the tree model specification
tune_tree_spec <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Define the parameter grid
tree_parm_df1 <- tibble(cost_complexity = seq(.001, .2, by = .05))
tree_parm_df2 <- tibble(min_n = seq(10, 50, by = 10))
tree_parm_df3 <- tibble(tree_depth = seq(5, 20, by = 5))
tree_parm_df <- full_join(tree_parm_df1, tree_parm_df2, by = character()) %>%
  full_join(., tree_parm_df3, by = character())

# Set up cross-validation
tree_folds <- vfold_cv(income_train, v = 3)

# Define the workflow
tree_workflow <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Perform the tuning
tree_results <- tune_grid(
  tree_workflow,
  resamples = tree_folds,
  grid = tree_parm_df
)

# Find the best parameters based on accuracy
best_tree <- select_best(tree_results, metric = "accuracy")
# Print the best parameters
print('Best Parameters:')
print(best_tree)
# Finalize the workflow using the best parameters
final_tree_workflow <- finalize_workflow(
  tree_workflow,
  best_tree
)

# Fit the final model to the full training data
final_fit <- final_tree_workflow %>%
  fit(data = income_train)

# Evaluate the model on the test set
test_predictions <- predict(final_fit, new_data = income_test) %>%
  bind_cols(income_test) %>%
  metrics(truth = high.earner, estimate = .pred_class)

# Print the accuracy of the final model on the test set
print('*********** DECISION TREE MODEL **************')
print(paste("Out of Sample Accuracy:", test_predictions$.estimate[1]))


#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

# YOU FILL IN THE REST
print('Starting TREE')

# Set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune() # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# Define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,50,by=10)) # Adjusted to match problem statement
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df <- full_join(tree_parm_df1, tree_parm_df2, by=character()) %>%
  full_join(., tree_parm_df3, by=character())

# 3-fold cross-validation setup
tree_folds <- vfold_cv(income_train, v = 3)

# Workflow
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
tree_res <- tree_wf %>%
  tune_grid(
    resamples = tree_folds,
    grid = tree_parm_df
  )

# What is the best tree model?
best_tree <- select_best(tree_res, metric = "accuracy")

# Finalize the workflow using the best parameters
final_tree_model <- finalize_workflow(
  tree_wf,
  best_tree
)

# Fit the final model to the full training data and evaluate it on the test set
tree_results <- last_fit(final_tree_model, income_split) %>%
  collect_metrics()

# Print best model's performance
print('*********** DECISION TREE MODEL **************')
tree_results %>% print(n =4)
#----
# Extract best parameters and associated metrics
tree_best_params <- select_best(tree_res, metric = "accuracy")
tree_best_results <- collect_metrics(last_fit(final_tree_model, income_split))

# If these contain the necessary columns, proceed to join. Otherwise, check their structure.
# Let's ensure the column names expected are indeed there
print(colnames(tree_best_params))
print(colnames(tree_best_results))

# Now attempting the join again, ensure the columns exist
#tree_ans <- tree_best_params %>% 
#left_join(tree_best_results, by = c(".metric", ".estimator"))

# Assuming tree_best_results contains metrics for the best model
best_metrics <- tree_best_results %>% 
  filter(.metric == "accuracy") %>% # Filtering may not be necessary if only one metric is calculated
  summarize(best_accuracy = max(.estimate))

# Adding the best accuracy to the best parameters directly
tree_ans <- bind_cols(tree_best_params, best_metrics)


# Print the final combined dataset
print(tree_ans)






#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

# YOU FILL IN THE REST

# 3-fold cross-validation setup
nnet_folds <- vfold_cv(income_train, v = 3)

# Workflow
nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning the neural network
nnet_res <- nnet_wf %>%
  tune_grid(
    resamples = nnet_folds,
    grid = nnet_parm_df
  )

# Extract the best parameters and their metrics
best_nnet <- select_best(nnet_res, metric = "accuracy")

# Print the best parameters
print("Best Parameters for Neural Network:")
print(best_nnet_params)

# Finalize the workflow using the best parameters
final_nnet_model <- finalize_workflow(nnet_wf, best_nnet)

# Fit the final model to the full training data and evaluate it on the test set
nnet_results <- last_fit(final_nnet_model, income_split) %>%
  collect_metrics()

# Print the best model's performance
print('*********** NEURAL NETWORK MODEL **************')
nnet_results %>% print(n = 1)

# Print best accuracy from the results
best_metrics_nnet <- nnet_results %>% 
  filter(.metric == "accuracy") %>%
  summarize(best_accuracy = max(.estimate))

print(best_metrics_nnet)
#_________________________________________

print('Starting NNET')

# Set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()       # L2 regularization parameter
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# Define a grid over which to try different values of the regularization parameters
nnet_parm_df1 <- tibble(hidden_units = seq(1, 10))
lambda_grid <- grid_regular(penalty(), levels = 10)
nnet_parm_df <- cross_join(nnet_parm_df1, lambda_grid)

# 3-fold cross-validation setup
nnet_folds <- vfold_cv(income_train, v = 3)

# Workflow
nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning the neural network
nnet_res <- nnet_wf %>%
  tune_grid(
    resamples = nnet_folds,
    grid = nnet_parm_df
  )

# Extract the best parameters and their metrics
best_nnet_params <- select_best(nnet_res, metric = "accuracy")

# Print the best parameters
print("Best Parameters for Neural Network:")
print(best_nnet_params)

# Finalize the workflow using the best parameters
final_nnet_model <- finalize_workflow(nnet_wf, best_nnet_params)

# Fit the final model to the full training data and evaluate it on the test set
nnet_results <- last_fit(final_nnet_model, income_split) %>%
  collect_metrics()

# Print the best model's performance
print('*********** NEURAL NETWORK MODEL **************')
nnet_results %>% print(n = 1)

# Print best accuracy from the results
best_metrics_nnet <- nnet_results %>% 
  filter(.metric == "accuracy") %>%
  summarize(best_accuracy = max(.estimate))

print(best_metrics_nnet)
# Combine best parameters and best accuracy into one data frame
nnet_ans <- bind_cols(best_nnet_params, nnet_results %>% print(n = 1))

# Print the combined results
print("Combined Best Parameters and Accuracy for Neural Network:")
print(nnet_ans)




#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

# YOU FILL IN THE REST

# 3-fold cross-validation setup
knn_folds <- vfold_cv(income_train, v = 3)

# Workflow
knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning the kNN model
knn_res <- knn_wf %>%
  tune_grid(
    resamples = knn_folds,
    grid = knn_parm_df
  )

# Extract the best parameters and their metrics
best_knn_params <- select_best(knn_res, metric = "accuracy")

# Print the best parameters
print("Best Parameters for k-Nearest Neighbors:")
print(best_knn_params)

# Finalize the workflow using the best parameters
final_knn_model <- finalize_workflow(knn_wf, best_knn_params)

# Fit the final model to the full training data and evaluate it on the test set
knn_results <- last_fit(final_knn_model, income_split) %>%
  collect_metrics()

# Print the best model's performance
print('*********** k-NEAREST NEIGHBORS MODEL **************')
knn_results %>% print(n = 1)
# Assuming knn_results contains a dataframe with metrics results
knn_ans <- knn_results %>%
  mutate(model = "KNN") %>%  # Add a column for the model identifier
  arrange(desc(.metric == "accuracy")) %>%  # This makes rows with 'accuracy' appear first
  select(.metric, .estimate, .config, model)  # Select relevant columns

print(knn_ans)
# Assuming knn_results contains a dataframe with metrics results
knn_ans <- knn_results %>%
  mutate(model = "KNN") %>%  # Add a column for the model identifier
  arrange(desc(.metric == "accuracy")) %>%  # This makes rows with 'accuracy' appear first
  select(.metric, .estimate, .config, model)  # Select relevant columns

print(knn_ans)


#knn_results_extra %>% print(n = 3)

#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
#tune_svm_spec <- svm_rbf(
#  cost = tune(), 
#  rbf_sigma = tune()
#) %>% 
# set_engine("kernlab") %>%
#  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
#svm_parm_df1 <- tibble(cost      = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
#svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
#svm_parm_df  <- full_join(svm_parm_df1,svm_parm_df2,by=character())

# YOU FILL IN THE REST

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(tidymodels)
library(kernlab)

# Set up the SVM model specification
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# Define a formula
svm_formula <- high.earner ~ .

# Create a workflow
svm_workflow <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(svm_formula)

# Define the grid for tuning
svm_parm_df1 <- tibble(cost = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df <- full_join(svm_parm_df1, svm_parm_df2, by = character())

# Set up cross-validation
cv_folds <- vfold_cv(income_train, v = 3, strata = high.earner)

# Perform tuning
svm_results <- tune_grid(
  svm_workflow,
  resamples = cv_folds,
  grid = svm_parm_df,
  metrics = metric_set(accuracy)
)

# Extract and print best performing model parameters
#best_params <- select_best(svm_results, "accuracy")
# Extract and print best performing model parameters
best_params <- select_best(svm_results, metric = "accuracy")

print(best_params)

# Finalize the workflow with the best parameters
final_svm_wf <- finalize_workflow(svm_workflow, best_params)

# Fit the final model on the training data
final_fit <- last_fit(final_svm_wf, split = income_split)

# Collect metrics
results <- collect_metrics(final_fit)
print(results)

# Create svm_ans for combining later
svm_ans <- results %>%
  mutate(model = "SVM") %>%
  select(-starts_with(".config"))  # Adjust this selection as needed based on your results structure

print(svm_ans)


#####################
# combine answers
#####################
all_ans <- bind_rows(logit_ans,tree_ans,nnet_ans,knn_ans,svm_ans)
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown") %>% print

# Extract accuracy for SVM
svm_accuracy <- 0.865  # From the printed results

# Extract accuracy for Logistic Regression
logit_accuracy <- 0.846  # From the printed results

# Extract accuracy for Decision Tree
tree_accuracy <- 0.868  # From the printed results

# Extract accuracy for Neural Network
nnet_accuracy <- 0.837  # From the printed results

# Extract accuracy for k-Nearest Neighbors
knn_accuracy <- 0.844  # From the printed results
# Creating a summary data frame
accuracy_summary <- tibble(
  `Model Type` = c("SVM", "Logistic Regression", "Decision Tree", "Neural Network", "k-Nearest Neighbors"),
  `Accuracy` = c(svm_accuracy, logit_accuracy, tree_accuracy, nnet_accuracy, knn_accuracy)
)
# Print the summary table
print(accuracy_summary)

# SVM Best Parameters
svm_best_params <- tibble(
  `Model Type` = "SVM",
  cost = 1,
  rbf_sigma = 0.25
)

# Logistic Regression Best Parameters
logit_best_params <- tibble(
  `Model Type` = "Logistic Regression",
  penalty = 0.000339
)

# Decision Tree Best Parameters
tree_best_params <- tibble(
  `Model Type` = "Decision Tree",
  cost_complexity = 0.001,
  tree_depth = 15,
  min_n = 10
)

# Neural Network Best Parameters
nnet_best_params <- tibble(
  `Model Type` = "Neural Network",
  hidden_units = 6,
  penalty = 0.00000278
)

# k-Nearest Neighbors Best Parameters
knn_best_params <- tibble(
  `Model Type` = "k-Nearest Neighbors",
  neighbors = 26
)

# Combine all parameter data into one tibble
best_parameters_summary <- bind_rows(
  svm_best_params,
  logit_best_params,
  tree_best_params,
  nnet_best_params,
  knn_best_params
)

# Print the summary table
print(best_parameters_summary)


