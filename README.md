Geography 411 – Homework 2: Precipitation Analysis

Overview:

This repository contains the R code and analysis for the Geography 411 homework assignment. The goal of the analysis is to test whether the average annual precipitation in Buffalo has changed across three distinct time periods: 1940-1967, 1968-1996, and 1997-2025. Similar analysis is also performed for San Diego precipitation data.

Methods:

    The following statistical tests were performed on the data:

        ANOVA: To test for differences in the means of precipitation across the time periods.
        
        Kruskal-Wallis: A non-parametric test used when the data doesn’t meet the assumptions of ANOVA.
        
        Median Test: To check for significant differences in the medians of the precipitation data.
        
        Levene’s Test: To test for homogeneity of variances between groups.
        
        Kolmogorov-Smirnov Test: To assess the normality of the data for each period.

Key Findings:

        • The results from the ANOVA, Kruskal-Wallis, and median tests provide insights into whether precipitation 
          patterns in Buffalo and San Diego have changed over time.
          
        • The Levene and Kolmogorov-Smirnov tests were used to check the assumptions of the parametric tests.

Tools:

        • R: The analysis was conducted using R programming language. The primary functions used include 
          lm(), anova(), kruskal.test(), chisq.test(), and various plotting functions.
        
        • RStudio: RStudio was used to run the R code and visualize the data.

Reflection:

This analysis helped to understand the impact of different time periods on precipitation data. It also highlighted the importance of checking assumptions (like normality and equal variances) before choosing the appropriate statistical test.
