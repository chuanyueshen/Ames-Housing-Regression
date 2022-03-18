# Final Project Repo for STAT 425 Applied Regression and Design
Fall 2017

## Credit to team members:
- _Chuanyue Shen_
- Lijun Zhang
- Lingzhu Gu

**Note: The project was done locally in Fall 2017. Related files were uploaded to this repo just for future reference. Full usage of these files is not guaranteed.**

## Executive Summary
_Please find more details in the report._

In this project, we analyze the AmesHousing data, which includes housing price in Ames, Iowa from 2006 to 2010, and other 81 variables. Sale price is treated as response, while others are used as possible predictors. There are 2930 observations initially including some missing data and false data, thus data preprocessing is performed first. The features with the same value exceeding 80% is deleted, and the observation with missing categorical feature data is also discarded. The observation with missing numerical feature data is still reserved, but the value of numerical feature is replaced with its median. 2880 observations are left after preprocessing, with 42 categorical features and 35 numerical features. 

Then collinearity is performed on the 35 numerical features with the criteria as 0.9, and no feature is deleted in this step. Using “boxcox” function, we find no transformation needed for the response. After that, variable selection is performed by both AIC and BIC methods, and forward, both, as well as backward models are all considered. 10 common numerical variables and 11 common categorical variables are observed in the six selected models, where the variables used for visualization analysis is chosen. Then AIC forward model is decided as our final model by ANOVA, which contains 22 numerical variables and 27 categorical variables to predict sale price. We also do diagnostics for our data, including high leverage points, outliers, and high influential points. 15 outliers and 12 high influential points are deleted. In addition, it is found that our data satisfies linear assumption but violates constant variance assumption and normality assumption.

In visualization, we explore the relationship between sale price and year built, first floor area, neighborhood, latitude/longitude, as well as sale condition/basement exposure/kitchen quality respectively. Three interesting findings are listed as follows.
1)	The sale price of house increases more rapidly in latest years than past. And based on the trend line and forecasting, we have 95% confidence that the average sale price will stay stable around $260,000 in next five years.
2)	NridgHt neighborhood is noticed with the highest sale price, while the BrDale has the lowest sale price. Also, the sale price in NirdgHt is the most sensitive to first floor area. Therefore, we suppose that BrDale is the most economical location for buying house.
3)	The numbers of house available in different conditions are limited, and most houses have kitchen quality as TA, sale condition as Normal with no basement exposure. The houses with excellent (Ex) kitchen, Abnormal sale condition and good (Gd) basement exposure are noticed with the highest average sale price. Therefore, we can save money if we could tradeoff between basement and kitchen when selecting house.
