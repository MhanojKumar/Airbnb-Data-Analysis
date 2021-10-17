# Airbnb-Data-Analysis-

## Abstract:
Here the best technique for identifying the most excellent Airbnb accommodation in NYC based on price, availability, and other variables is determined. In-depth analysis of one of the country's and the world's most densely inhabited cities is conducted. As of November 2018, New York City was one of the top cities for Airbnb, with over 52,000 listings. Here real estate data for all Airbnb lodges in New York was used to analyze the rental landscape in New York City through different static and interactive visuals for our descriptive study. I tried different Data mining methods using R programming such as Linear regression, Regression trees, and the Boosting method approach to arrive at the best model for getting the most significant pricing for stays. It includes model specification, transformation, variable selection and more. Finally, I determined which model is the greatest fit for the customer's needs based on the lowest pricing for each model.

## Data Preprocessing:
In some attributes, there were null values, which were filled by mean imputation techniques as a part of data cleaning. Id, name, host id, host name, last review date and neighborhood variables were removed as they were helpless in making the price prediction. The neighborhood variable has been removed because the training and testing data did not have similar areas while splitting the dataset. This created a problem while testing the data with the help of trained data. In order to avoid that situation, it is neglected altogether.

## Output Category:
Since price is to be predicted as the output, it is chosen as the response variable. Depending on the output, one model will be finalized. The optimal model for the New York Airbnb dataset will be the one that meets customer’s criteria at the lowest cost.

