## The code is involved for the statistical analysis section of the LASSO for this article
# The code was run using R, software version 4.1.2
# The packages you need to install before running the software:
    "broom"1.0.2
    "dplyr" 1.0.10
    "ggplot2" 3.4.0
    "openxlsx" 4.2.5.2
    "readr" 2.1.3
    "sampling" 2.9
    "showtext" 0.9.5
    "tidymodels" 1.0.0



## First run the file "LASSO SCI.R"
# Line12: Change CFPD into the dataset of your training and testing set. The input form is ".csv".
# Line13: Change CFPD into the dataset of your training and testing set. The input form is ".csv".
# Line75-81: You can change best_penalty or best_se_penalty into the penalty you want. The penalty before the change is the best penalty automatically selected by the code.
# Line136: The code "var_imf" give the Variables importance score of all variables.
# Line 157: The name of the variable selected by the LASSO needs to be filled in after "lambda", otherwise this part of the code will not run.
# Line179-189: "lasso_mod_out" in the code gives the predicted Y value of the training and testing set; "lasso_mod_out2" gives the predicted Y value of the validation set.



## Then run the file "1000penalties train.R" if you need to calculate a large number of penalties at once and summarize the number of variables and R^2 of the model

