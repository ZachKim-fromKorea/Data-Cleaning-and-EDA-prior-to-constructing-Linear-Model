# Data-Cleaning-and-EDA-prior-to-constructing-Linear-Model
Simply import the R file "EDA_for Linear Model.R" using source() function which will result in 6 new functions (1) lm.eda (2) categorical.var (3) numeric.var (4) outlier (5) skewness (6) kurtosis

Files above are for R users who wish to carry out Data Cleaning and (partial) EDA prior to constructing Linear Regression Model.
Simply upload the file "EDA_for Linear Model.R" using source() function which will give you 6 new functions 

Functions include;
1) lm.eda(data)
2) categorical.var(data) : Which variables are categorical & How many categorical variables
3) numeric.var(data) : Which variables are numeric & How many numeric variables
4) outlier(data, x) : Returns the number of outliers (* outliers defined by Tukey, outside of 1.5*Inter Quartile Range)
5) skewness(data, x) : Skewness using 3rd moment.
6) kurtosis(data, x) : Kurtosis using 4rd moment.

- Objects inside the parenthesis are parameters that need to be specified for the functions to work.
- It is crucial that you encode categorical variables as factors as you import your data. Functions above does not convert categorical variables into factors for you. Set "stringsAsFactors = TRUE" as you import your data.

These are the tasks "lm.eda" do for you:
1) Basic EDA : Number of Obs, variables, categorical & numeric variables
2) Get rid of observations which response variables are NAs
3) Get rid of variables which percentage of NAs is bigger than 50%
4) Imputation : if percentage of NAs is less than 20% we impute by median, or else impute using random Forest method.
5) Erase outliers from Bonferonni outlier Test

Hope you enjoy using the functions above. Should there be any questions or suggestions don't hesitate to contact me at ksj949773@gmail.com
Thank you
