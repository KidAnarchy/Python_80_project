==================================================================
               Regression model analysis report
==================================================================
Dataset: dengue_preprocessing.csv

==========================Features:===============================
1.year_num: 	code of year 2016 – 2020 (1-5)
2.province_num: code of province 1- 77 
3.day_raindrop: how many days of rain drop in 365 day 
4.quant_rain: 	total quantity rainwater in unit of millimeters 
5.humbidity_perc: average % humbidity in each province, each year
6.temp_max: 	maximum temp in each province, each year
7.temp_min: 	minimum temp in each province, each year
8.temp_avg: 	average temp in each province, each year
9.dead: 	amount of dead person from dengue 
10.dead_perc: 	percent of dead person from dengue 
11.patient: 	amount of patient from dengue (target variable)


-------------------------------------------------------------------
What we want to know?                           
Rain has effect to dengue patient or not?
-------------------------------------------------------------------


====================Model testing==================================
1.Linear Regression (Baseline)
2.Repeats K-folds Cross Validation
3.Features selection: Recursive Feature Elimination(RFE)
4.GridSearchCV 
5.Polynomial regression 

**We have 5 jupyter notebook file to run coding to test model
who interest to run it, you have to use our.csv file and set up to 
your working directory.


====================Evaluation metrics=============================
1.R2 Score of Training set
2.R2 Score of Testing set 
3.MAE
4.RMSE

=======================Conclusion==================================
Read in Full report.

Thank you



