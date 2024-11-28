# Data Preprocessing and Analysis
This repository contains the complete workflow for data preprocessing and analysis as presented in the accompanying paper. The workflow is divided into two main sections:

## Data Preprocessing: 
Conducted in Google Colab using Python.
## Data Analysis: 
Performed in R, focusing on statistical analysis and visualization as detailed in the paper.

The workflow begins with preprocessing raw datasets to prepare them for analysis. This involves cleaning, filtering, and transforming data to ensure quality and consistency. The preprocessed data is then analyzed to generate insights and visualizations presented in the paper.

## Requirements
#### Python (Google Colab)
Python 3.x
Libraries: pandas, numpy, scipy, statsmodels, sklearn

#### R
R version 4.x or higher
Libraries: dplyr, ggplot2, readr, tidyr
Data Preprocessing in Google Colab
The preprocessing section is implemented in Python using Google Colab. The scripts include:

### Loading and Cleaning Data:
Handling missing values.
Filtering irrelevant data.
Calculating indices like Shannon Index, trend of yield gap.
Aggregating and summarizing data by country and year.

### Exporting Clean Data:
The preprocessed datasets are saved in .csv format for use in the R analysis.

### Data Analysis in R
The analysis section is implemented in R and focuses on generating the insights presented in the paper. 

## How to Use
Preprocessing in Google Colab
Clone the repository.
Open the preprocessing notebook in Google Colab.
Follow the instructions in the notebook to load raw data, preprocess it, and export the cleaned datasets.
Analysis in R
Load the cleaned datasets into R.
Run the R scripts provided in the R directory.
Explore the outputs and visualizations.


## The files include:
Original datasets used
Cleaned datasets ready for analysis.
Statistical models and insights.
Visualizations of key findings.
These results are discussed in detail in the paper, supported by code provided in this repository.
