## Import the necessary packages -----------------------------------------------
library(kohonen)  # Self-Organising Maps
library(kknn)     # kNN
library(nnet)     # Neural Networks

## Define constants ------------------------------------------------------------
DATA_PATH <- "data"
R_PATH <- "R"

## Source defined functions ----------------------------------------------------
source(file.path(R_PATH, "boosting-functions.R"))
source(file.path(R_PATH, "data-functions.R"))
source(file.path(R_PATH, "neural-net-functions.R"))
source(file.path(R_PATH, "sampling-functions.R"))
source(file.path(R_PATH, "som-functions.R"))
source(file.path(R_PATH, "util-functions.R"))

## 
iono <- read.csv(file.path(DATA_PATH, "hepatitis.csv"), header = TRUE)
data_maj <- subset(iono, Class == -1)
data_min <- subset(iono, Class == 1)
maj_clusters <- clusters(data_maj[, -ncol(data_maj)])
min_clusters <- clusters(data_min[, -ncol(data_min)])

boundaries <- identifyBoundary(data_maj, data_min, maj_clusters, min_clusters)
synth <- synthesiseBoundary(data_maj[, -ncol(data_maj)], data_min[, -ncol(data_min)], 
                            boundaries$Boundary_Majority, boundaries$Boundary_Minority)
