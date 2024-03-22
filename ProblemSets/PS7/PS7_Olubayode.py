import pandas as pd
import numpy as np
import seaborn as sns

import warnings
warnings.filterwarnings('ignore')

import matplotlib.pyplot as plt
import seaborn as sns


from sklearn import preprocessing
import sklearn.model_selection as ms
from sklearn import linear_model
import sklearn.metrics as sklm

import numpy.random as nr
import matplotlib.pyplot as plt

import scipy.stats as ss
import math



# Load the dataset
df = pd.read_csv('wages.csv')

# Display the first few rows of the DataFrame to ensure it's loaded correctly
df.head()


# Applying Label Encoding to 'college'
df['college_encoded'] = pd.factorize(df['college'])[0]

# Applying One-Hot Encoding to 'married'
df['married_encoded'] = pd.factorize(df['married'])[0]

# Display the modified DataFrame
print(df.head())

df.shape

def quality_report(df):
    """
    Description: Displays quality of data in terms of missing values, 
    unique numbers, datatypes etc.
    
    Arguments: Dataframe
    """
    dtypes = df.dtypes
    nuniq = df.T.apply(lambda x: x.nunique(), axis=1)
    total = df.isnull().sum().sort_values(ascending = False)
    percent = (df.isnull().sum()/df.isnull().count()*100).sort_values(ascending = False)
    quality_df  = pd.concat([total, percent, nuniq, dtypes], axis=1, keys=['Total NaN', 'Percent of NaN','Nunique', 'Dtype'])
    display(quality_df)
    
quality_report(df)

# Drop Observations Where hgc or tenure Are Missing
# I use the dropna() function from pandas, specifying hgc and tenure in the subset argument to drop rows where these specific columns have missing values.

# Drop rows where `hgc` or `tenure` are missing
df = df.dropna(subset=['hgc', 'tenure'])

# Display the shape of the DataFrame to check how many rows are left
df.shape

quality_report(df)

# Using modelsummary to Produce a Summary Table

# Generate descriptive statistics
summary_table = df.describe()
summary_table

# Convert the descriptive statistics to LaTeX format
summary_latex = summary_table.to_latex()

# Write the LaTeX string to a file
with open('C:/Users/OLUBAYODE/Documents/Data Science Economics/summary_latex', 'w') as file:
    file.write(summary_latex)

# Confirmation message
print("Descriptive statistics have been exported to 'summary_latex.tex'.")

# Check the first few lines of the LaTeX string to ensure it looks correct
print("\n".join(summary_latex.split("\n")[:10]))

# Analyze Missingness of Log Wages
# To analyze the rate at which log wages are missing and theorize whether the missingness is MCAR (Missing Completely At Random), MAR (Missing At Random), or MNAR (Missing Not At Random), I will first calculate the missing rate.

# Calculate the rate of missing values for log wages
missing_rate_logwage = df['logwage'].isnull().mean()

print(f"Rate of missing log wages: {missing_rate_logwage:.2%}")

# Create a boolean mask indicating where 'logwage' is missing
missing_logwage = df['logwage'].isnull()

# Group the data based on the presence or absence of 'logwage' data
# Calculate the mean for each group
grouped_means = df.groupby(missing_logwage).mean()
print(grouped_means)

# The mean values of other variables grouped by the presence or absence of logwage data reveal some differences. For example, the average years of education (hgc) and tenure at the current job are higher in rows where logwage is missing. This suggests that the missingness of logwage may not be completely random (MCAR) but could be related to other observed variables, hinting at a potential MAR situation or MNAR.
# Consideration the context and nature of the data, a deeper understanding emerges, suggesting that the missingness of logwage data is, in fact, Missing Not At Random (MNAR). The dataset comprises wage information, a sensitive topic where the likelihood of non-disclosure could be influenced by the amount of wage itself. Both higher and lower earners may have distinct motivations for not disclosing their income, with higher earners possibly concerned about privacy or social scrutiny, and lower earners potentially influenced by social stigma or personal dissatisfaction. This selective non-disclosure directly relates to the values of logwage itself, making the missing data MNAR


import statsmodels.formula.api as smf
import numpy as np

# Drop rows where `logwage` is missing
complete_cases_df = df.dropna(subset=['logwage'])

# Define the regression formula
formula = 'logwage ~ hgc + C(college) + tenure + np.power(tenure, 2) + age + C(married)'

# Estimate the linear regression model
model_complete_cases = smf.ols(formula, data=complete_cases_df).fit()
# Print the summary of the model
model_complete_cases.summary()


# Perform Mean Imputation to Fill in Missing Log Wages
# For mean imputation, I'll fill in missing logwage observations with the mean value of the logwage column from the complete cases.

# Calculate the mean of `logwage` from complete cases
logwage_mean = complete_cases_df['logwage'].mean()

# Perform mean imputation
df['logwage_mean_imputed'] = df['logwage'].fillna(logwage_mean)

# Estimate the regression with mean imputed logwage
model_mean_imputed = smf.ols(formula.replace('logwage', 'logwage_mean_imputed'), data=df).fit()

model_mean_imputed.summary()


# Impute Missing Log Wages as Their Predicted Values from the Complete Cases Regression
# I'll use the model estimated from the complete cases to predict logwage for the missing observations.

# Predict logwage using the complete cases model for all observations
df['logwage_predicted'] = model_complete_cases.predict(df)

# Use predicted logwage for missing values
df['logwage_pred_imputed'] = df['logwage'].fillna(df['logwage_predicted'])

# Estimate the regression with predicted imputed logwage
model_pred_imputed = smf.ols(formula.replace('logwage', 'logwage_pred_imputed'), data=df).fit()
df['logwage_predicted'] 
df['logwage_pred_imputed'] 


model_pred_imputed.summary()


from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.linear_model import BayesianRidge
import numpy as np

# Prepare the data: extract relevant features and target for imputation purposes
# df['college_encoded'] = df['college'].apply(lambda x: 1 if x == 'college grad' else 0)
# df['married_encoded'] = df['married'].apply(lambda x: 1 if x == 'married' else 0)

features_for_imputation = df[['logwage', 'hgc', 'tenure', 'age', 'college_encoded', 'married_encoded']]


# features_for_imputation = df[['logwage', 'hgc', 'tenure', 'age']]  # Example features

# Initialize the IterativeImputer
imputer = IterativeImputer(estimator=BayesianRidge(), max_iter=10, random_state=0)

# Perform multiple imputation
features_imputed = imputer.fit_transform(features_for_imputation)

# Extract the imputed logwage values
df['logwage_multi_imputed'] = features_imputed[:, 0]  # Assuming 'logwage' is at index 0

# Adjust the regression formula to use the multi-imputed 'logwage'
formula_multi_imputed = formula.replace('logwage', 'logwage_multi_imputed')

# Estimate the regression model with the multi-imputed 'logwage'
model_multi_imputed = smf.ols(formula_multi_imputed, data=df).fit()


model_multi_imputed.summary()


betas = {
    'Complete Cases': model_complete_cases.params['hgc'],
    'Mean Imputation': model_mean_imputed.params['hgc'],
    'Predicted Imputation': model_pred_imputed.params['hgc'],
    'Multiple Imputation': model_multi_imputed.params['hgc'], # Placeholder for multiple imputation model
}

betas


betas_df = pd.DataFrame({
    'Complete Cases': model_complete_cases.params['hgc'],
    'Mean Imputation': model_mean_imputed.params['hgc'],
    'Predicted Imputation': model_pred_imputed.params['hgc'],
    'Multiple Imputation': model_multi_imputed.params['hgc']  # Placeholder for multiple imputation model
}, index=['hgc'])  # Specify the index here

betas_df

import pandas as pd

# Assuming your models are named as follows:
# model_complete_cases, model_mean_imputed, model_pred_imputed, model_multi_imputed

# Create a DataFrame to hold the coefficients
coefficients_df = pd.DataFrame({
    'Complete Cases': model_complete_cases.params,
    'Mean Imputation': model_mean_imputed.params,
    'Predicted Imputation': model_pred_imputed.params,
    'Multiple Imputation': model_multi_imputed.params  # Assuming this model exists and has been fitted
})#.fillna(0)  # Fill NaNs with 0s for any missing coefficients across models for comparison


coefficients_df


# Convert the DataFrame to LaTeX
coefficients_latex = coefficients_df.to_latex()

# Optionally, you can write this string to a .tex file
with open('model_coefficients.tex', 'w') as f:
    f.write(coefficients_latex)

    
# Based on the regression results for the hgc variable (which we're interpreting as β1) across four models, I can analyze the impact of different imputation methods on the estimated returns to schooling hgc (i.e., the effect of an additional year of education on log wages). Here's a detailed look at the hgc coefficient across the models:

# Complete Cases: β1 = 0.062393
# Mean Imputation: β1 = 0.049688
# Predicted Imputation: β1 = 0.062393
# Multiple Imputation: β1 = 0.061235
# The true value of β1 is given as 0.093, which serves as a benchmark for evaluating the accuracy of our estimates.

# Analysis of β1 Across Models
# Complete Cases and Predicted Imputation: Both methods yield a β1 of 0.062393, which is lower than the true value of 0.093. The identical estimates suggest that the predicted imputation method (which uses the complete cases regression to predict missing logwage values) does not significantly alter the estimate for hgc compared to using only complete cases. This could indicate that the mechanism of missingness in logwage does not heavily bias the estimate of hgc when using these methods.

# Mean Imputation: This method yields the lowest β1 estimate of 0.049688, significantly deviating from the true value. Mean imputation tends to reduce the variability in the imputed variable (logwage in this case) and can lead to biased estimates especially if the missing data are not Missing Completely At Random (MCAR). This result suggests that mean imputation might be the least reliable method for this specific context, potentially underestimating the true effect of education on wages.

# Multiple Imputation: The estimate for β1 using multiple imputation is 0.061235, slightly lower than the complete cases and predicted imputation methods but closer to them than to the mean imputation estimate. Multiple imputation generally provides a more nuanced handling of missing data by creating multiple complete datasets and pooling the results, which can help address the biases associated with simpler imputation methods.

# Conclusions
# The complete cases and predicted imputation methods provide higher estimates for β1 compared to mean imputation, suggesting they may be more reliable in this context, although they still underestimate the true effect of education on wages compared to the given true value of 0.093.
# Mean imputation significantly underestimates the returns to schooling, likely due to its simplistic assumption about missing data, which can introduce bias.
# Multiple imputation offers a balanced estimate, reflecting a sophisticated approach to handling missing data. However, it still falls short of the true value, which may suggest the presence of other factors not captured in the model or that the missing data mechanism affects the logwage variable in a way that even sophisticated imputation cannot fully correct for.
# This analysis highlights the importance of choosing appropriate imputation methods based on the nature of the missing data and the specific context of the analysis. While multiple imputation generally provides a robust approach, understanding the limitations of each method is crucial for accurate statistical inference.