## Import the necessary packages -----------------------------------------------
library(kohonen)  # Self-Organising Maps
library(kknn)     # kNN
library(nnet)     # Neural Networks

## Define constants ------------------------------------------------------------
DATA_PATH <- "data"
R_PATH <- "R"

## Source defined functions ----------------------------------------------------
source(file.path(R_PATH, "boosting-functions.R"))
source(file.path(R_PATH, "cv-functions.R"))
source(file.path(R_PATH, "data-functions.R"))
source(file.path(R_PATH, "metrics-functions.R"))
source(file.path(R_PATH, "neural-net-functions.R"))
source(file.path(R_PATH, "sampling-functions.R"))
source(file.path(R_PATH, "som-functions.R"))
source(file.path(R_PATH, "util-functions.R"))

## Import data sets ------------------------------------------------------------
data_list <- readDataSets(DATA_PATH)

## Experiments
source("params.R")
nn_results <- Map(cvTrain, 
                  data = data_list, 
                  method = "neuralNet", 
                  folds = FOLDS, 
                  repeats = REPEATS, 
                  cores = CORES, 
                  seed = SEED)
nn_results <- lapply(nn_results, do.call, what = rbind)
nn_results <- lapply(nn_results, colMeans)

adaboost_results <- Map(cvTrain, 
                        data = data_list, 
                        method = "adaboostM1", 
                        folds = FOLDS, 
                        repeats = REPEATS, 
                        cores = CORES, 
                        seed = SEED)
adaboost_results <- lapply(adaboost_results, do.call, what = rbind)
adaboost_results <- lapply(adaboost_results, colMeans)

proposal_results <- Map(cvTrain, 
                        data = data_list, 
                        method = "adaboostM1", 
                        sampling = "BSBDG", 
                        folds = FOLDS, 
                        repeats = REPEATS, 
                        cores = CORES, 
                        seed = SEED)
proposal_results <- lapply(proposal_results, do.call, what = rbind)
proposal_results <- lapply(proposal_results, colMeans)

final_results <- Map(rbind, nn_results, adaboost_results, proposal_results)
lapply(final_results, round, digits = 4)
