R code for predicting the key variables involve in smoking status  
In this project , I conducted a detailed analysis of a smoking dataset using R programming. The primary objectives included data preprocessing and cleaning, exploratory data analysis (EDA),statistical analysis and the implementation of predictive models.(Regression model and Random Forest model)
In Data Preprocessing:I handled missing values, outliers, and transformed variables into factors as needed.
In Exploratory Data Analysis (EDA): I Utilized descriptive statistics( mean,median,variance,and standard deviance which provide comprehensive summary of distribution of numeric data) and visualizations, to gain insights into the dataset and identified patterns, trends, and potential relationships among variables. Data distribution among numeric variables is done after removal of outliers by histogram and density plots, while data distribution among categorical variables is done by using bar plots.
I built a contingency table that typically compares the predicted outcomes from the model with the actual observed outcomes. 
Employed boxplots to examine the relationships between numeric and categorical(smoking) variables.
Split the dataset into training and testing sets to facilitate model evaluation then Utilized a comprehensive approach to ensure robust model evaluation and generalization.
Implemented logistic regression and Random Forest models for predicting binary outcomes related to the 'smoking' variable.
Assessed model performance through metrics such as accuracy and precision 
Identified key variables through the analysis of model outputs.
Determined the importance of variables based on their contribution to model predictions.
In case of logistic regression model ,Used insights from contingency tables, bar plots, and boxplots ,AIC,Z-value,P-value,standard deviation and significance level to inform the selection of key variables.
split data into test and train sets for random forest .Set the number of trees (ntree) and other hyperparameters.Make prediction by apply the trained model to my test data and at last find accuracy
Conclusion:
The analysis provided valuable insights into the smoking dataset, employing a comprehensive approach from data preprocessing to model implementation and evaluation. The combination of statistical techniques, visualizations, and predictive modeling contributed to a thorough understanding of key variables influencing smoking status.
