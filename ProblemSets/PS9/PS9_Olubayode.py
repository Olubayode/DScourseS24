import pandas as pd
import numpy as np
import random

# Set the seed
np.random.seed(123456)
random.seed(123456)



# URL to the housing dataset
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"

# Column names for the dataset
column_names = [
    "crim", "zn", "indus", "chas", "nox", "rm", "age",
    "dis", "rad", "tax", "ptratio", "b", "lstat", "medv"
]

# Load the dataset
housing = pd.read_csv(url, delim_whitespace=True, names=column_names)

# Display the first few rows of the dataframe
print(housing.head())


from sklearn.model_selection import train_test_split
from sklearn.preprocessing import PolynomialFeatures



# Converting `medv` to logs
housing['log_medv'] = np.log(housing['medv'])

# Converting `chas` to a categorical variable
housing['chas'] = housing['chas'].astype('category')

# Split the data into training and testing sets before transforming continuous features
housing_train, housing_test = train_test_split(housing, test_size=0.2, random_state=123456)

# Identifying continuous variables (excluding 'chas' and 'medv')
continuous_vars = ['crim', 'zn', 'indus', 'nox', 'rm', 'age', 'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat']

# 3. Apply PolynomialFeatures for 6th-degree polynomials and interactions
poly = PolynomialFeatures(degree=6, include_bias=False, interaction_only=False)
housing_train_poly = poly.fit_transform(housing_train[continuous_vars])
housing_test_poly = poly.transform(housing_test[continuous_vars])

# Convert the polynomial features back to DataFrame for easy handling
poly_feature_names = poly.get_feature_names_out(continuous_vars)
housing_train_poly_df = pd.DataFrame(housing_train_poly, columns=poly_feature_names, index=housing_train.index)
housing_test_poly_df = pd.DataFrame(housing_test_poly, columns=poly_feature_names, index=housing_test.index)

# Combine the polynomial features with the non-continuous features
housing_train_prepped = pd.concat([housing_train_poly_df, housing_train[['chas', 'log_medv']]], axis=1)
housing_test_prepped = pd.concat([housing_test_poly_df, housing_test[['chas', 'log_medv']]], axis=1)

# Display the dimensions of the training data
print(f"Dimension of training data: {housing_train_prepped.shape}")
print(f"Number of X variables added: {housing_train_prepped.shape[1] - len(continuous_vars) - 2}") # Subtract original continuous variables and 'chas', 'medv'


from sklearn.linear_model import LassoCV
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from math import sqrt

# Extract the target variable 'log_medv' from the preprocessed training and testing sets
y_train = housing_train_prepped['log_medv']
y_test = housing_test_prepped['log_medv']

# Select features, dropping 'log_medv' from the training and testing dataframes
X_train = housing_train_prepped.drop(['log_medv'], axis=1)
X_test = housing_test_prepped.drop([S'log_medv'], axis=1)

# Define the preprocessing for the 'chas' column (categorical)
preprocessor = ColumnTransformer(transformers=[('cat', OneHotEncoder(), ['chas'])], remainder='passthrough')

# Create the LassoCV pipeline
pipeline = Pipeline(steps=[('preprocessor', preprocessor),
                           ('lasso', LassoCV(cv=6, random_state=123456))])

# Fit the pipeline to the training data
pipeline.fit(X_train, y_train)

# Predict on training and test data
y_train_pred = pipeline.predict(X_train)
y_test_pred = pipeline.predict(X_test)

# Calculate RMSE for training and test sets
in_sample_rmse = sqrt(mean_squared_error(y_train, y_train_pred))
out_of_sample_rmse = sqrt(mean_squared_error(y_test, y_test_pred))

# Retrieve the optimal lambda (alpha) from the LassoCV model
optimal_alpha = pipeline.named_steps['lasso'].alpha_

optimal_alpha, in_sample_rmse, out_of_sample_rmse

print(f"The Optimal Alpha: {optimal_alpha}")
print(f"The In Sample RMSE: {in_sample_rmse}")
print(f"The OUt of Sample RMSE: {out_of_sample_rmse}")


import numpy as np
from sklearn.linear_model import RidgeCV

# Ensure numpy is imported
ridge_alphas = np.logspace(-6, 6, 13)

# Create the RidgeCV pipeline
pipeline_ridge = Pipeline(steps=[('preprocessor', preprocessor),
                                 ('ridge', RidgeCV(alphas=ridge_alphas, cv=6, scoring='neg_mean_squared_error'))])

# Fit the pipeline to the training data
pipeline_ridge.fit(X_train, y_train)

# Predict on the test data
y_test_pred_ridge = pipeline_ridge.predict(X_test)

# Calculate the out-of-sample RMSE for Ridge Regression
out_of_sample_rmse_ridge = sqrt(mean_squared_error(y_test, y_test_pred_ridge))

# Retrieve the optimal alpha (lambda) value from the RidgeCV model
optimal_alpha_ridge = pipeline_ridge.named_steps['ridge'].alpha_

optimal_alpha_ridge, out_of_sample_rmse_ridge
print(f"The Optimal Alpha Ridge: {optimal_alpha_ridge}")
print(f"The Out of Sample RMSE Ridge: {out_of_sample_rmse_ridge}")
