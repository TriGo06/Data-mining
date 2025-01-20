# Predicting Loan Payment Defaults

## Project Overview

This project aims to predict loan payment defaults using various classification algorithms. By analyzing data on past loans, the goal is to optimize predictions to minimize financial risks, ensuring robust credit decisions.

### Objective
- Develop and evaluate classifiers to predict defaults for 500 new clients.
- Identify the most effective classifier to balance risk mitigation and operational efficiency.

## Workflow

1. **Data Exploration**
   - Examined variables to identify predictors of defaults.
   - Eliminated redundant or non-informative features (e.g., `client` ID and constant `categorie`).

2. **Data Preprocessing**
   - Imputed missing values using:
     - Median substitution.
     - `missForest` for enhanced accuracy.
   - Transformed `education` into numeric values for compatibility with algorithms.
   - Created new features (e.g., `DE = debcarte/emploi`) to improve model accuracy.

3. **Clustering**
   - Identified pivotal variables (e.g., `age`, `adresse`) through clustering.
   - Created boolean variables for values around these pivots.

4. **Classifier Development**
   - Tested models using `caret` package with:
     - Decision Trees (C5.0, rpart)
     - Random Forest (rf, ranger, Rborist)
     - Neural Networks (avNNet, nnet, pcaNNet)
     - Naive Bayes
     - Support Vector Machines (svmLinear2, svmPoly, svmRadial)

5. **Evaluation**
   - Metrics used:
     - AUC (Area Under Curve) for global performance.
     - Positive Predictive Value (PPV) to focus on risk reduction.
   - Balanced sampling ensured unbiased evaluations.

6. **Optimal Classifier Selection**
   - Chose Naive Bayes with the `SansrevQ` dataset as the optimal model, balancing AUC and PPV.

7. **Prediction**
   - Applied the optimal classifier to predict defaults for new clients.

## Key Results

- **Best Model**: Naive Bayes on the `SansrevQ` dataset.
- **Feature Engineering**:
  - New variable `DE` significantly improved model accuracy.
  - Removing `revenus` simplified models without compromising performance.

## File
- **Code**: [Code_DataMining_project.R](./Code_DataMining_project.R)

## Usage

1. Load and preprocess data from `projet.csv`.
2. Train and evaluate classifiers using the provided R script.
3. Apply the optimal model to `projet_new.csv` for predictions.

## Future Improvements

- Integrate financial impact analysis to weigh false negatives and positives.
- Enhance feature engineering with domain-specific insights.
