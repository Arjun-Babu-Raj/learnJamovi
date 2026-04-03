# learnJamovi

A Jamovi module for learning statistics — within Jamovi itself.

## Overview

**learnJamovi** provides six interactive statistical learning guides, each combining:

- **Statistical theory** – clear explanations of concepts, assumptions, and effect sizes  
- **Step-by-step Jamovi instructions** – how to run each analysis in Jamovi  
- **Live analysis** – apply the analysis to your own data or the included sample datasets  

## Analyses included

| Menu item | Topic |
|-----------|-------|
| Descriptive Statistics | Central tendency, spread, skewness, kurtosis |
| T-Tests | One-sample, independent-samples, paired-samples t-tests; Cohen's d |
| One-Way ANOVA | F-test, post-hoc Tukey HSD, η² effect size |
| Correlation | Pearson r, Spearman ρ, Kendall τ; 95% CIs |
| Linear Regression | Simple & multiple regression; R², standardised β |
| Chi-Square Tests | Test of independence, Cramér's V |

## Sample datasets

Three CSV datasets are included in the `data/` folder:

| File | Description |
|------|-------------|
| `survey.csv` | Survey data (age, gender, education, income, satisfaction, anxiety pre/post, group) |
| `academic.csv` | Student performance (math/reading/writing scores, study hours, attendance, grade, school type) |
| `clinical.csv` | Clinical trial (BMI, blood pressure, cholesterol, treatment group, outcome, pain scores) |

## Installation

### From GitHub (recommended)

```r
# Install remotes if needed
install.packages("remotes")

# Install learnJamovi
remotes::install_github("Arjun-Babu-Raj/learnJamovi")
```

After installing the R package, sideload the module in Jamovi:  
**Jamovi → Modules (⊞) → Sideload → select the installed package folder**

### From within Jamovi

1. Open Jamovi  
2. Click **Modules (⊞)** in the top-right  
3. Click **jamovi library** and search for *learnJamovi*  
4. Click **Install**

## Usage

1. Open any dataset (or one of the sample CSVs from **File → Open**)  
2. Click **learnJamovi** in the analysis menu  
3. Select a topic (e.g., *Descriptive Statistics*)  
4. Drag variables into the input boxes  
5. Read the **Statistical Theory** and **Jamovi Instructions** panels in the output  

## How it helps

- **Students** – Learn statistical concepts alongside real data analysis  
- **Researchers** – Use as a quick reference guide while running analyses  
- **Instructors** – Demonstrate statistics teaching examples directly inside Jamovi  

## License

GPL-3
