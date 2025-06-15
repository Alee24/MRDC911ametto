# MRDC 911 Assignment 1 - EDA & Data Preprocessing on Kenyan Student Dataset


    This project explores academic, socioeconomic, and behavioral data of Kenyan university students using R. The goal is to perform Exploratory Data Analysis (EDA) and Data Preprocessing to prepare the dataset for potential modeling and gain insights on factors affecting student performance.

#requirements

    Make sure R and RStudio are installed. 
    install the following R packages:
    
    install.packages(c("tidyverse", "ggplot2", "corrplot"))
    library(tidyverse)
    library(ggplot2)
    library(corrplot)

#File Structure

    kenya_student_data.csv - Raw dataset
    
    R Script Answers.R - Full analysis script with code and comments
    
    kenya_student_data_preprocessed.csv - Cleaned dataset after preprocessing
    
    README.rd - This documentation

The script answers the following key questions:

    Overview of dataset structure
    
    Summary statistics and visualizations
    
    Correlation analysis
    
    Tests for relationships (e.g., internet access vs. performance)
    
    Handling missing data and outliers
    
    Feature engineering (binning, total score)
    
    Preprocessed output saved as CSV 
    
#Key Findings
    Urban students study slightly more than rural ones.
    
    Students with internet access generally perform better.
    
    Family income is skewed; most students fall in the lower two income quartiles.
    
    Participation in extracurriculars tends to align with better academic outcomes.
    
    Strong correlation exists between study hours and academic success.
    
#Author
    ALEX METTO
    25ZAD111307
    MRDC 911 - Data Science & Computational Intelligence
    Riara University
    June 2025

