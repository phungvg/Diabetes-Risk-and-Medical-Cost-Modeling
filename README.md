# 🩺 Diabetes Risk and Medical Cost Modeling

IDE: R Studio and R Shiny

Predict diabetes (0/1) with Logistic, LDA, QDA, Naive Bayes and tune a clinical decision threshold (precision–recall trade-off).

Estimate medical charges with linear regression, compare feature sets, and report RMSE/R².

Includes ROC/AUC, confusion matrix, and reproducible train/test splits.

2 datasets: diabetes.csv, medical_cost.csv

---

## ⚙️ Setup

### 🔁 1. Clone the Repository

Use Git to download the project from GitHub:

```bash
git clone https://github.com/phungvg/diabetes-medical-cost.git
cd diabetes-medical-cost
```

### ☁️ 2. R Packages
Run locally in app.R

```bash
install.packages(c(
  "shiny","caret","pROC","MASS","e1071","DT"
  # optional: "shinythemes"
))
```


### ▶️ Run the App

- [x] **Diabetes(classification)**  
  - Pick a model: Logistic, LDA, QDA, or Naive Bayes.
  - Choose features and train fraction.
  - Move the threshold slider:
    
    ↑ threshold → higher precision, lower sensitivity
    
    ↓ threshold → higher sensitivity, lower precision

Review confusion matrix, ROC curve, AUC and pick the threshold that matches your clinical goal.
  
- [x] **Medical Cost(regression)**
  - Select predictors for charges.
  - Inspect coefficients and hold-out RMSE / R².
  - Compare feature subsets (e.g., via adjusted R² / BIC in the analysis notebook if included).


---
### Visualization from the app
| Diabetes tab | Medical Cost tab |
Diabetes tab — choose classifier, tune threshold, view ROC/AUC & metrics
<img src="https://github.com/user-attachments/assets/37da685b-c386-41ae-afdf-58af7ef6d99a" alt="Diabetes tab: model, threshold, ROC/AUC" width="100%">

Medical Cost tab — select predictors, inspect coefficients
<img src="https://github.com/user-attachments/assets/dee63ad6-96a0-4c6e-a09f-1af814b410f4" alt="Medical Cost tab: coefficients and RMSE/R²" width="100%">


