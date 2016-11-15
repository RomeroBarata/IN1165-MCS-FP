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
source(file.path(R_PATH, "neural-net-functions.R"))
source(file.path(R_PATH, "sampling-functions.R"))
source(file.path(R_PATH, "som-functions.R"))
source(file.path(R_PATH, "util-functions.R"))

## Import data sets ------------------------------------------------------------
data_list <- readDataSets(DATA_PATH)

## Tests
breast <- data_list[[1]]
