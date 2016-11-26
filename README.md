# IN1165 MCS FP
Final project for **IN1165 Multiple Classifier Systems**.

The goal of this project is to replicate the article:
Thanathamathee, Putthiporn, and Chidchanok Lursinsap. 2013. “Handling Imbalanced Data Sets with Synthetic Boundary Data Generation Using Bootstrap Re-Sampling and Adaboost Techniques.” Pattern Recognition Letters 34 (12). Elsevier: 1339–47.

## How to run it?
Running the script `main.R` should do all the work. It can be done from the `R` console with the command `source("main.R")` (this directory should be your working directory). If you have trouble running it, uncomment and run the second line in `main.R`. Script configuration is in the `params.R` file. You can change the cross-validation parameters, the seed value for replicability of the results, and the number of cores for parallel processing. If you are a Windows user please keep to one core. The final results will be stored in the `final_results` variable, and will be nicely displayed after completion of code execution.

## Organisation
The data sets utilised for the experimentation are in the `data` folder. All the implemented functions are logically organised into scripts inside the `R` folder. A report for this final project is in the file `report.html`.
