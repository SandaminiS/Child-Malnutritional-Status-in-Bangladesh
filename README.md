# Child Malnutritional Status in Bangladesh: Machine Learning Approaches

## Overview
This project, completed for STA6366: Statistical Methodology for Data Science I, focuses on predicting the rates of child malnutrition in Bangladesh using various machine learning techniques. Despite improvements in child malnutrition globally over the last few decades, it remains a significant concern in developing countries, including Bangladesh. 

## Group Members
- Sandamini Senaratne
- Shahd Alnofaie
- Md Mehedi Hasan Bhuiyan

## Objectives
- Predict the rate of child malnutrition in Bangladesh, including underweight, stunting, wasting, and overweight categories.

## Dataset
The data for this project was sourced from the UNICEF and Bangladesh Bureau of Statistics (BBS) through the Multiple Indicator Cluster Survey (MICS) conducted in 2019, which covered 64,000 households. 

### Data Description
- **Total Features**: 18
- **Total Observations**: 24,000

### Response Variable
- **Malnutrition**: A dichotomous variable representing:
  - Underweight (weight for age < −2SD)
  - Stunting (height for age < −2SD)
  - Wasting (weight for height < −2SD)
  - Overweight (weight for height > 2SD)

### Features
- Gender
- Age of child
- Child birth weight
- Living area
- Division (Geographical region)
- Mother’s education
- Father’s education
- Household size
- Wealth index
- ANC (Antenatal care)
- Delivery status
- Number of childbirths
- Mother’s disability
- Ethnicity
- Woman’s age during birth
- Sanitation facility
- Source of drinking water

## Methodology
- **Dimension Reduction Techniques**: 
  - Multiple Correspondence Analysis
  - Factor Analysis for Mixed Data
  
- **Machine Learning Techniques**: 
  - LASSO Regression
  - Logistic Regression
  - Random Forest
  - Support Vector Machine (SVM)
  
- **Diagnostics**: 
  - Root Mean Square Error (RMSE)
  - ROC Curve
  - Accuracy

## Timeline
- **Proposal**: 11/02/2023
- **Progress Report**: 11/21/2023
- **Final Submission**: 07/12/2023

## References
- UNICEF, B. (2019). Multiple Indicator Cluster Survey 2019, Bangladesh. Retrieved from [UNICEF](https://www.unicef.org/bangladesh/)
