UK House Price Trend Analysis (R)
Overview:

This project analyses long-term trends in UK house prices using linear regression in R. Using the UK Price Paid dataset, a large national dataset was cleaned, sampled, and modelled to examine how property prices have changed over time. The project demonstrates skills in data wrangling, statistical modelling, data visualisation, and reproducible analysis.

Tools:
R
data.table – efficient data processing
arrow – handling large datasets
ggplot2 – data visualisation

Method Summary:
Imported and inspected over 1 million property transactions
Created reproducible random samples to improve efficiency
Cleaned and transformed raw data (dates, prices, column names)
Built a linear regression model: Price ~ Year of Sale
Visualised yearly average prices with a fitted trend line
Saved model outputs for reproducibility

How to Run:

Requirements
R (4.0+ recommended)
RStudio (optional)

Setup
Install required packages
install.packages(c("data.table", "arrow", "ggplot2"))
Download the UK Price Paid dataset (pp-complete.csv) and place it in the same directory as the R script (or update the file path).

Run
Open the script in R/RStudio and run it from top to bottom. The script will load the data, fit the regression model, generate a plot, and save outputs.

Outputs
model_summary.txt – regression results
linear_model.rds – saved linear model
Plot of average UK house prices over time

Skills Demonstrated

Data cleaning • Linear regression • Data visualisation • Reproducible analysis • R programming
